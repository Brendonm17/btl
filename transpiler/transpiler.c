// ============================================================================
// transpiler.c - BTL to C Transpiler v2 Ã¢â‚¬â€ Performance Edition
//
// KEY PERFORMANCE TECHNIQUES:
//
// 1) INLINE STACK OPS
//    Old: push(vm, x);  // function call, vm->stackTop dereference
//    New: *sp++ = x;    // sp is a C local Ã¢â€ â€™ register
//
// 2) CACHED VM STATE
//    We keep frame, slots, sp as C locals. The C compiler can put
//    these in registers. We only sync back to vm->stackTop at call
//    boundaries (where the callee needs to see the real stack).
//
// 3) DIRECT CALL THREADING
//    When OP_CLOSURE creates a function whose constant-pool index maps
//    to a known transpiled fn id, and OP_CALL later calls that closure,
//    we emit a direct C function call instead of btl_call_value() + run().
//    This skips: generic dispatch, opcode decode loop, indirect branch.
//
// 4) FUSED OPCODE PATTERNS
//    Common patterns get single C statements:
//      GET_LOCAL + GET_LOCAL + LESS + POP_JUMP_IF_FALSE
//        Ã¢â€ â€™ if (AS_NUMBER(slots[a]) >= AS_NUMBER(slots[b])) goto L;
//      GET_LOCAL + GET_LOCAL + ADD + SET_LOCAL_POP
//        Ã¢â€ â€™ slots[c] = NUMBER_VAL(AS_NUMBER(slots[a]) + AS_NUMBER(slots[b]));
//
// 5) TAIL CALL OPTIMIZATION
//    Tail calls to known transpiled functions become a parameter-shuffle
//    + goto to the function top, or a direct call that replaces the frame.
// ============================================================================

#include "transpiler.h"
#include "collect.h"
#include "../src/chunk.h"
#include "../src/object.h"
#include "../src/value.h"
#include "../src/vm.h"

#include <stdlib.h>
#include <string.h>
#include <math.h>

// ================================================================
// Output helpers
// ================================================================

#define OUT(t, ...) fprintf((t)->out, __VA_ARGS__)
#define NL(t) fprintf((t)->out, "\n")

// Build the reverse lookup table: global slot index -> name string.
// Called once per module at the start of emit_program().
static void build_global_name_lookup(BtlTranspiler* t, ObjModule* module) {
    memset(t->globalNameBySlot, 0, sizeof(t->globalNameBySlot));
    t->globalNameCount = 0;
    if (!module) return;
    for (int i = 0; i < module->globalNames.capacity; i++) {
        BtlEntry* entry = &module->globalNames.entries[i];
        if (!IS_EMPTY(entry->key) && IS_STRING(entry->key)) {
            int slot = (int)AS_NUMBER(entry->value);
            if (slot >= 0 && slot < BTL_MAX_GLOBAL_SLOTS) {
                t->globalNameBySlot[slot] = AS_STRING(entry->key)->chars;
                if (slot >= t->globalNameCount) t->globalNameCount = slot + 1;
            }
        }
    }
}

// O(1) global name lookup via pre-built reverse table.
static const char* find_global_name(BtlTranspiler* t, int index) {
    if (index >= 0 && index < BTL_MAX_GLOBAL_SLOTS && t->globalNameBySlot[index])
        return t->globalNameBySlot[index];
    return "?";
}

static void emit_comment(BtlTranspiler* t, int offset, const char* name) {
    if (t->config.emit_comments) {
        OUT(t, "    /* %04d: %s*/\n", offset, name);
    }
}

static void emit_label(BtlTranspiler* t, int offset) {
    OUT(t, "  L_%04d:;\n", offset);
}

// ================================================================
// Loop detection structures for LICM
//
// A loop is identified by an OP_LOOP instruction that jumps backward.
// The loop header is the target of that backward jump.
// The loop end is the IP after the OP_LOOP instruction.
// ================================================================

#define MAX_LOOPS_PER_FUNCTION 64

typedef struct {
    int header;     // IP of loop header (target of backward jump)
    int end;        /* IP after OP_LOOP instruction*/
} LoopInfo;

typedef struct {
    LoopInfo loops[MAX_LOOPS_PER_FUNCTION];
    int count;
} LoopTable;

// ================================================================
// Type Specialization & Constant Propagation Infrastructure
//
// Track abstract types and constant values through bytecode execution
// to skip runtime type checks and evaluate expressions at compile time.
//
// Types:
//   TYPE_UNKNOWN - Could be anything, must type check
//   TYPE_NUMBER  - Known to be a number, skip IS_NUMBER checks
//   TYPE_BOOL    - Known to be boolean
//   TYPE_NIL     - Known to be nil
//   TYPE_STRING  - Known to be a string
//   TYPE_OBJECT  - Known to be some object type
//
// Constant Tracking:
//   Each tracked value can optionally have a known constant value.
//   When isConstant is true, constBtlValue holds the actual Value.
// ================================================================

typedef enum {
    TYPE_UNKNOWN = 0,
    TYPE_NUMBER,
    TYPE_INT,
    TYPE_BOOL,
    TYPE_NIL,
    TYPE_STRING,
    TYPE_OBJECT
} AbstractType;

static const char* type_name(AbstractType t) {
    switch (t) {
        case TYPE_INT: return "INT";
        case TYPE_NUMBER: return "NUMBER";
        case TYPE_BOOL: return "BOOL";
        case TYPE_NIL: return "NIL";
        case TYPE_STRING: return "STRING";
        case TYPE_OBJECT: return "OBJECT";
        default: return "UNKNOWN";
    }
}

/* Tracked value with optional constant*/
typedef struct {
    AbstractType type;
    bool isConstant;   /* true if constBtlValue is known at compile time*/
    double numValue;   /* for TYPE_NUMBER constants*/
    int64_t intValue;  /* for TYPE_INT constants*/
    int stringConstIdx; /* for TYPE_STRING: index into constants array, -1 if unknown*/
} TrackedValue;

#define MAX_TRACKED_LOCALS 32
#define MAX_TRACKED_STACK 32

typedef struct {
    TrackedValue locals[MAX_TRACKED_LOCALS];  /* Known types/values of local slots*/
    TrackedValue stack[MAX_TRACKED_STACK];    /* Known types/values on stack*/
    int stackDepth;                           /* Current stack depth for tracking*/
} TypeState;

/* Create a tracked value*/
static TrackedValue tracked_unknown(void) {
    return (TrackedValue){ TYPE_UNKNOWN, false, 0.0, 0, -1 };
}

static TrackedValue tracked_type(AbstractType t) {
    return (TrackedValue){ t, false, 0.0, 0, -1 };
}

static TrackedValue tracked_number(double v) {
    return (TrackedValue){ TYPE_NUMBER, true, v, 0, -1 };
}

static TrackedValue tracked_bool(bool v) {
    return (TrackedValue){ TYPE_BOOL, true, v ? 1.0 : 0.0, 0, -1 };
}

static TrackedValue tracked_int(int64_t v) {
    return (TrackedValue){ TYPE_INT, true, 0.0, v, -1 };
}

static TrackedValue tracked_string_const(int constIdx) {
    return (TrackedValue){ TYPE_STRING, true, 0.0, 0, constIdx };
}

/* Initialize type state - all unknown*/
static void type_state_init(TypeState* ts) {
    for (int i = 0; i < MAX_TRACKED_LOCALS; i++) {
        ts->locals[i] = tracked_unknown();
    }
    for (int i = 0; i < MAX_TRACKED_STACK; i++) {
        ts->stack[i] = tracked_unknown();
    }
    ts->stackDepth = 0;
}

/* Push a tracked value onto the abstract stack*/
static void type_push_tv(TypeState* ts, TrackedValue tv) {
    if (ts->stackDepth < MAX_TRACKED_STACK) {
        ts->stack[ts->stackDepth++] = tv;
    }
}

/* Push just a type (no constant)*/
static void type_push(TypeState* ts, AbstractType t) {
    type_push_tv(ts, tracked_type(t));
}

/* Pop a tracked value from the abstract stack*/
static TrackedValue type_pop_tv(TypeState* ts) {
    if (ts->stackDepth > 0) {
        return ts->stack[--ts->stackDepth];
    }
    return tracked_unknown();
}

/* Pop just getting the type*/
static AbstractType type_pop(TypeState* ts) {
    return type_pop_tv(ts).type;
}

/* Peek at tracked value on stack*/
static TrackedValue type_peek_tv(TypeState* ts, int depth) {
    int idx = ts->stackDepth - 1 - depth;
    if (idx >= 0 && idx < MAX_TRACKED_STACK) {
        return ts->stack[idx];
    }
    return tracked_unknown();
}

/* Peek at the type only*/
static AbstractType type_peek(TypeState* ts, int depth) {
    return type_peek_tv(ts, depth).type;
}

/* Set stack top tracked value*/
static void type_set_top_tv(TypeState* ts, TrackedValue tv) {
    if (ts->stackDepth > 0 && ts->stackDepth <= MAX_TRACKED_STACK) {
        ts->stack[ts->stackDepth - 1] = tv;
    }
}

/* Set stack top type (no constant)*/
static void type_set_top(TypeState* ts, AbstractType t) {
    type_set_top_tv(ts, tracked_type(t));
}

// ================================================================
// Function list (collect.c)
// ================================================================

void btl_function_list_init(BtlFunctionList* list);
void btl_function_list_free(BtlFunctionList* list);
void btl_collect_functions(ObjFunction* main_fn, BtlFunctionList* out);
int btl_function_id(BtlFunctionList* list, ObjFunction* fn);

// ================================================================
// Jump target collection Ã¢â‚¬â€ first pass over bytecode
// ================================================================

static void collect_jump_targets(ObjFunction* fn, bool* targets, int code_len, LoopTable* loops) {
    uint8_t* code = fn->chunk.code;
    memset(targets, 0, sizeof(bool) * (code_len + 1));
    if (loops) {
        loops->count = 0;
    }

    int ip = 0;
    while (ip < code_len) {
        uint8_t op = code[ip];
        ip++;

        switch (op) {
            /* 16-bit forward jumps*/
        case BTL_OP_JUMP:
        case BTL_OP_JUMP_IF_FALSE:
        case BTL_OP_POP_JUMP_IF_FALSE:
        case BTL_OP_JUMP_IF_TRUE:
        case BTL_OP_POP_JUMP_IF_TRUE:
        case BTL_OP_JUMP_IF_NOT_EQUAL:
        case BTL_OP_JUMP_IF_EQUAL:
        case BTL_OP_JUMP_IF_NOT_GREATER:
        case BTL_OP_JUMP_IF_NOT_LESS: {
            uint16_t offset = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            int target = ip + offset;
            if (target <= code_len) targets[target] = true;
            break;
        }
                                /* 1-byte slot + 16-bit forward jump (iterator)*/
        case BTL_OP_ITER_NEXT: {
            ip += 1; /* skip slot byte */
            uint16_t offset = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            int target = ip + offset;
            if (target <= code_len) targets[target] = true;
            break;
        }
                                /* 16-bit backward jump - this is a loop!*/
        case BTL_OP_LOOP: {
            uint16_t offset = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            int target = ip - offset;
            if (target >= 0) {
                targets[target] = true;
                /* Record loop info for LICM*/
                if (loops && loops->count < MAX_LOOPS_PER_FUNCTION) {
                    loops->loops[loops->count].header = target;
                    loops->loops[loops->count].end = ip;  /* IP after OP_LOOP*/
                    loops->count++;
                }
            }
            break;
        }

                    /* Opcodes with operands Ã¢â‚¬â€ skip their bytes*/
        case BTL_OP_CONSTANT: case BTL_OP_GET_LOCAL: case BTL_OP_SET_LOCAL:
        case BTL_OP_GET_GLOBAL: case BTL_OP_DEFINE_GLOBAL: case BTL_OP_SET_GLOBAL:
        case BTL_OP_GET_UPVALUE: case BTL_OP_GET_UPVALUE_OPEN:
        case BTL_OP_GET_UPVALUE_CLOSED: case BTL_OP_GET_UPVALUE_IMMUTABLE:
        case BTL_OP_SET_UPVALUE: case BTL_OP_SET_UPVALUE_OPEN:
        case BTL_OP_SET_UPVALUE_CLOSED:
        case BTL_OP_INC_LOCAL_POP: case BTL_OP_INC_LOCAL:
        case BTL_OP_CALL: case BTL_OP_TAIL_CALL:
        case BTL_OP_POP_N: case BTL_OP_FIELD:
        case BTL_OP_GET_FIELD_THIS: case BTL_OP_SET_FIELD_THIS:
        case BTL_OP_CLASS: case BTL_OP_BUILD_LIST: case BTL_OP_BUILD_TABLE:
        case BTL_OP_IMPORT: case BTL_OP_DO_NEW:
            ip += 1; break;

        case BTL_OP_CONSTANT_LONG:
        case BTL_OP_GET_GLOBAL_LONG: case BTL_OP_DEFINE_GLOBAL_LONG:
        case BTL_OP_SET_GLOBAL_LONG: case BTL_OP_GET_SUPER_LONG:
        case BTL_OP_CLASS_LONG: case BTL_OP_IMPORT_LONG:
        case BTL_OP_GET_PROPERTY_IC: case BTL_OP_SET_PROPERTY_IC:
        case BTL_OP_METHOD: case BTL_OP_DO_INVOKE:
            ip += 2; break;

        case BTL_OP_GET_SUPER:
            ip += 1; break;

        case BTL_OP_INVOKE: case BTL_OP_TAIL_INVOKE:
        case BTL_OP_SUPER_INVOKE: case BTL_OP_TAIL_SUPER_INVOKE:
            ip += 2; break;

        case BTL_OP_INVOKE_LONG: case BTL_OP_TAIL_INVOKE_LONG:
        case BTL_OP_SUPER_INVOKE_LONG: case BTL_OP_TAIL_SUPER_INVOKE_LONG:
        case BTL_OP_INVOKE_IC: case BTL_OP_TAIL_INVOKE_IC:
        case BTL_OP_METHOD_LONG:
            ip += 3; break;

            /* Invoke_N and tail invoke_N: 1 operand byte*/
        case BTL_OP_INVOKE_0: case BTL_OP_INVOKE_1: case BTL_OP_INVOKE_2:
        case BTL_OP_INVOKE_3: case BTL_OP_INVOKE_4: case BTL_OP_INVOKE_5:
        case BTL_OP_INVOKE_6: case BTL_OP_INVOKE_7: case BTL_OP_INVOKE_8:
        case BTL_OP_TAIL_INVOKE_0: case BTL_OP_TAIL_INVOKE_1: case BTL_OP_TAIL_INVOKE_2:
        case BTL_OP_TAIL_INVOKE_3: case BTL_OP_TAIL_INVOKE_4: case BTL_OP_TAIL_INVOKE_5:
        case BTL_OP_TAIL_INVOKE_6: case BTL_OP_TAIL_INVOKE_7: case BTL_OP_TAIL_INVOKE_8:
        case BTL_OP_SUPER_INVOKE_0: case BTL_OP_SUPER_INVOKE_1: case BTL_OP_SUPER_INVOKE_2:
        case BTL_OP_SUPER_INVOKE_3: case BTL_OP_SUPER_INVOKE_4: case BTL_OP_SUPER_INVOKE_5:
        case BTL_OP_SUPER_INVOKE_6: case BTL_OP_SUPER_INVOKE_7: case BTL_OP_SUPER_INVOKE_8:
        case BTL_OP_TAIL_SUPER_INVOKE_0: case BTL_OP_TAIL_SUPER_INVOKE_1: case BTL_OP_TAIL_SUPER_INVOKE_2:
        case BTL_OP_TAIL_SUPER_INVOKE_3: case BTL_OP_TAIL_SUPER_INVOKE_4: case BTL_OP_TAIL_SUPER_INVOKE_5:
        case BTL_OP_TAIL_SUPER_INVOKE_6: case BTL_OP_TAIL_SUPER_INVOKE_7: case BTL_OP_TAIL_SUPER_INVOKE_8:
            ip += 1; break;

            /* Closure: variable-length*/
        case BTL_OP_CLOSURE: {
            uint8_t fn_idx = code[ip++];
            BtlValue fn_val = fn->chunk.constants.values[fn_idx];
            if (IS_OBJ(fn_val) && OBJ_TYPE(fn_val) == BTL_OBJ_FUNCTION) {
                ObjFunction* child = AS_FUNCTION(fn_val);
                ip += child->upvalueCount * 3;
            }
            break;
        }
        case BTL_OP_CLOSURE_LONG: {
            uint16_t fn_idx = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            BtlValue fn_val = fn->chunk.constants.values[fn_idx];
            if (IS_OBJ(fn_val) && OBJ_TYPE(fn_val) == BTL_OBJ_FUNCTION) {
                ObjFunction* child = AS_FUNCTION(fn_val);
                ip += child->upvalueCount * 3;
            }
            break;
        }

                            /* Zero-operand opcodes: everything else*/
        default:
            break;
        }
    }
}

// ================================================================
// Check if a function creates closures that capture upvalues.
//
// This is used to determine if we need to call close_upvalues on return.
// If a function creates closures that capture its locals, we MUST call
// close_upvalues so those upvalues are properly boxed when we return.
// ================================================================
static bool function_creates_capturing_closures(ObjFunction* fn) {
    uint8_t* code = fn->chunk.code;
    int code_len = fn->chunk.count;
    int ip = 0;

    while (ip < code_len) {
        uint8_t op = code[ip++];

        switch (op) {
        case BTL_OP_CLOSURE: {
            uint8_t fn_idx = code[ip++];
            BtlValue fn_val = fn->chunk.constants.values[fn_idx];
            if (IS_OBJ(fn_val) && OBJ_TYPE(fn_val) == BTL_OBJ_FUNCTION) {
                ObjFunction* child = AS_FUNCTION(fn_val);
                if (child->upvalueCount > 0) {
                    return true;  /* This function creates closures that capture*/
                }
            }
            break;
        }
        case BTL_OP_CLOSURE_LONG: {
            uint16_t fn_idx = (uint16_t)((code[ip] << 8) | code[ip + 1]); ip += 2;
            BtlValue fn_val = fn->chunk.constants.values[fn_idx];
            if (IS_OBJ(fn_val) && OBJ_TYPE(fn_val) == BTL_OBJ_FUNCTION) {
                ObjFunction* child = AS_FUNCTION(fn_val);
                if (child->upvalueCount > 0) {
                    return true;  /* This function creates closures that capture*/
                }
            }
            break;
        }
        /* Skip operand bytes for other opcodes*/
        case BTL_OP_CONSTANT: case BTL_OP_GET_LOCAL: case BTL_OP_SET_LOCAL:
        case BTL_OP_GET_GLOBAL: case BTL_OP_DEFINE_GLOBAL: case BTL_OP_SET_GLOBAL:
        case BTL_OP_GET_UPVALUE: case BTL_OP_GET_UPVALUE_OPEN:
        case BTL_OP_GET_UPVALUE_CLOSED: case BTL_OP_GET_UPVALUE_IMMUTABLE:
        case BTL_OP_SET_UPVALUE: case BTL_OP_SET_UPVALUE_OPEN:
        case BTL_OP_SET_UPVALUE_CLOSED:
        case BTL_OP_CALL: case BTL_OP_TAIL_CALL: case BTL_OP_POP_N:
        case BTL_OP_FIELD: case BTL_OP_GET_FIELD_THIS: case BTL_OP_SET_FIELD_THIS:
        case BTL_OP_INC_LOCAL_POP: case BTL_OP_INC_LOCAL:
            ip += 1; break;
        case BTL_OP_JUMP: case BTL_OP_JUMP_IF_FALSE: case BTL_OP_POP_JUMP_IF_FALSE:
        case BTL_OP_JUMP_IF_TRUE: case BTL_OP_POP_JUMP_IF_TRUE:
        case BTL_OP_LOOP: case BTL_OP_CONSTANT_LONG:
        case BTL_OP_GET_GLOBAL_LONG: case BTL_OP_DEFINE_GLOBAL_LONG:
        case BTL_OP_SET_GLOBAL_LONG:
        case BTL_OP_JUMP_IF_NOT_EQUAL: case BTL_OP_JUMP_IF_EQUAL:
        case BTL_OP_JUMP_IF_NOT_GREATER: case BTL_OP_JUMP_IF_NOT_LESS:
            ip += 2; break;
        case BTL_OP_GET_PROPERTY_IC: case BTL_OP_SET_PROPERTY_IC:
        case BTL_OP_INVOKE_IC: case BTL_OP_TAIL_INVOKE_IC:
            ip += 2; break;
        case BTL_OP_INVOKE: case BTL_OP_INVOKE_LONG: case BTL_OP_SUPER_INVOKE:
        case BTL_OP_SUPER_INVOKE_LONG: case BTL_OP_TAIL_INVOKE:
        case BTL_OP_TAIL_INVOKE_LONG: case BTL_OP_TAIL_SUPER_INVOKE:
        case BTL_OP_TAIL_SUPER_INVOKE_LONG:
        case BTL_OP_METHOD: case BTL_OP_GET_SUPER:
            ip += 2; break;
        default:
            break;
        }
    }
    return false;  /* No capturing closures found*/
}

/* Check if a function needs close_upvalues called on return.
// This is true if:
//  - The function captures upvalues itself, OR
//  - The function creates closures that capture its locals
*/
static bool function_needs_close_upvalues(ObjFunction* fn) {
    return fn->upvalueCount > 0 || function_creates_capturing_closures(fn);
}

// ================================================================
// LICM (Loop-Invariant Code Motion) analysis
//
// For each loop, we track which global slots are:
//   - Read (via OP_GET_GLOBAL or OP_GET_GLOBAL_LONG)
//   - Written (via OP_SET_GLOBAL or OP_SET_GLOBAL_LONG)
//
// Globals that are read but never written inside the loop are
// candidates for hoisting.
// ================================================================

#define MAX_HOISTABLE_GLOBALS 32

typedef struct {
    int slot;           /* Global slot index*/
    const char* name;   /* Global name for debugging*/
} HoistCandidate;

typedef struct {
    HoistCandidate candidates[MAX_HOISTABLE_GLOBALS];
    int count;
} HoistInfo;

/* Scan bytecode from start_ip to end_ip (exclusive) and find global reads/writes*/
static void analyze_loop_for_licm(BtlTranspiler* t, ObjFunction* fn, int start_ip, int end_ip, HoistInfo* out) {
    uint8_t* code = fn->chunk.code;
    int code_len = fn->chunk.count;

    /* Track which globals are read vs written in this loop*/
    bool global_read[BTL_MAX_GLOBAL_SLOTS] = {0};
    bool global_written[BTL_MAX_GLOBAL_SLOTS] = {0};

    int ip = start_ip;
    while (ip < end_ip && ip < code_len) {
        uint8_t op = code[ip++];

        switch (op) {
        case BTL_OP_GET_GLOBAL: {
            uint8_t idx = code[ip++];
            global_read[idx] = true;
            break;
        }
        case BTL_OP_GET_GLOBAL_LONG: {
            uint16_t idx = (uint16_t)((code[ip] << 8) | code[ip + 1]); ip += 2;
            if (idx < BTL_MAX_GLOBAL_SLOTS) global_read[idx] = true;
            break;
        }
        case BTL_OP_SET_GLOBAL: case BTL_OP_DEFINE_GLOBAL: {
            uint8_t idx = code[ip++];
            global_written[idx] = true;
            break;
        }
        case BTL_OP_SET_GLOBAL_LONG: case BTL_OP_DEFINE_GLOBAL_LONG: {
            uint16_t idx = (uint16_t)((code[ip] << 8) | code[ip + 1]); ip += 2;
            if (idx < BTL_MAX_GLOBAL_SLOTS) global_written[idx] = true;
            break;
        }
        /* Skip operands for other opcodes (simplified - same as collect_jump_targets)*/
        case BTL_OP_CONSTANT: case BTL_OP_GET_LOCAL: case BTL_OP_SET_LOCAL:
        case BTL_OP_GET_UPVALUE: case BTL_OP_GET_UPVALUE_OPEN:
        case BTL_OP_GET_UPVALUE_CLOSED: case BTL_OP_GET_UPVALUE_IMMUTABLE:
        case BTL_OP_SET_UPVALUE: case BTL_OP_SET_UPVALUE_OPEN:
        case BTL_OP_SET_UPVALUE_CLOSED:
        case BTL_OP_INC_LOCAL_POP: case BTL_OP_INC_LOCAL:
        case BTL_OP_CALL: case BTL_OP_TAIL_CALL:
        case BTL_OP_POP_N: case BTL_OP_FIELD:
        case BTL_OP_GET_FIELD_THIS: case BTL_OP_SET_FIELD_THIS:
        case BTL_OP_CLASS: case BTL_OP_BUILD_LIST: case BTL_OP_BUILD_TABLE:
        case BTL_OP_IMPORT: case BTL_OP_DO_NEW: case BTL_OP_GET_SUPER:
            ip += 1; break;
        case BTL_OP_CONSTANT_LONG:
        case BTL_OP_GET_SUPER_LONG:
        case BTL_OP_CLASS_LONG: case BTL_OP_IMPORT_LONG:
        case BTL_OP_GET_PROPERTY_IC: case BTL_OP_SET_PROPERTY_IC:
        case BTL_OP_METHOD: case BTL_OP_DO_INVOKE:
            ip += 2; break;
        case BTL_OP_INVOKE: case BTL_OP_TAIL_INVOKE:
        case BTL_OP_SUPER_INVOKE: case BTL_OP_TAIL_SUPER_INVOKE:
        case BTL_OP_JUMP: case BTL_OP_JUMP_IF_FALSE: case BTL_OP_POP_JUMP_IF_FALSE:
        case BTL_OP_JUMP_IF_TRUE: case BTL_OP_POP_JUMP_IF_TRUE:
        case BTL_OP_JUMP_IF_NOT_EQUAL: case BTL_OP_JUMP_IF_EQUAL:
        case BTL_OP_JUMP_IF_NOT_GREATER: case BTL_OP_JUMP_IF_NOT_LESS:
        case BTL_OP_LOOP:
            ip += 2; break;
        case BTL_OP_INVOKE_LONG: case BTL_OP_TAIL_INVOKE_LONG:
        case BTL_OP_SUPER_INVOKE_LONG: case BTL_OP_TAIL_SUPER_INVOKE_LONG:
        case BTL_OP_INVOKE_IC: case BTL_OP_TAIL_INVOKE_IC:
        case BTL_OP_METHOD_LONG:
            ip += 3; break;
        case BTL_OP_INVOKE_0: case BTL_OP_INVOKE_1: case BTL_OP_INVOKE_2:
        case BTL_OP_INVOKE_3: case BTL_OP_INVOKE_4: case BTL_OP_INVOKE_5:
        case BTL_OP_INVOKE_6: case BTL_OP_INVOKE_7: case BTL_OP_INVOKE_8:
        case BTL_OP_TAIL_INVOKE_0: case BTL_OP_TAIL_INVOKE_1: case BTL_OP_TAIL_INVOKE_2:
        case BTL_OP_TAIL_INVOKE_3: case BTL_OP_TAIL_INVOKE_4: case BTL_OP_TAIL_INVOKE_5:
        case BTL_OP_TAIL_INVOKE_6: case BTL_OP_TAIL_INVOKE_7: case BTL_OP_TAIL_INVOKE_8:
        case BTL_OP_SUPER_INVOKE_0: case BTL_OP_SUPER_INVOKE_1: case BTL_OP_SUPER_INVOKE_2:
        case BTL_OP_SUPER_INVOKE_3: case BTL_OP_SUPER_INVOKE_4: case BTL_OP_SUPER_INVOKE_5:
        case BTL_OP_SUPER_INVOKE_6: case BTL_OP_SUPER_INVOKE_7: case BTL_OP_SUPER_INVOKE_8:
        case BTL_OP_TAIL_SUPER_INVOKE_0: case BTL_OP_TAIL_SUPER_INVOKE_1: case BTL_OP_TAIL_SUPER_INVOKE_2:
        case BTL_OP_TAIL_SUPER_INVOKE_3: case BTL_OP_TAIL_SUPER_INVOKE_4: case BTL_OP_TAIL_SUPER_INVOKE_5:
        case BTL_OP_TAIL_SUPER_INVOKE_6: case BTL_OP_TAIL_SUPER_INVOKE_7: case BTL_OP_TAIL_SUPER_INVOKE_8:
            ip += 1; break;
        case BTL_OP_CLOSURE: {
            uint8_t fn_idx = code[ip++];
            BtlValue fn_val = fn->chunk.constants.values[fn_idx];
            if (IS_OBJ(fn_val) && OBJ_TYPE(fn_val) == BTL_OBJ_FUNCTION) {
                ObjFunction* child = AS_FUNCTION(fn_val);
                ip += child->upvalueCount * 3;
            }
            break;
        }
        case BTL_OP_CLOSURE_LONG: {
            uint16_t fn_idx = (uint16_t)((code[ip] << 8) | code[ip + 1]); ip += 2;
            BtlValue fn_val = fn->chunk.constants.values[fn_idx];
            if (IS_OBJ(fn_val) && OBJ_TYPE(fn_val) == BTL_OBJ_FUNCTION) {
                ObjFunction* child = AS_FUNCTION(fn_val);
                ip += child->upvalueCount * 3;
            }
            break;
        }
        default:
            break;
        }
    }

    /* Build list of hoistable globals: read but not written*/
    out->count = 0;
    for (int i = 0; i < BTL_MAX_GLOBAL_SLOTS && out->count < MAX_HOISTABLE_GLOBALS; i++) {
        if (global_read[i] && !global_written[i]) {
            out->candidates[out->count].slot = i;
            out->candidates[out->count].name = find_global_name(t, i);
            out->count++;
        }
    }
}

/* Check if a global slot is in the hoist candidates list*/
static int find_hoist_candidate(HoistInfo* info, int slot) {
    for (int i = 0; i < info->count; i++) {
        if (info->candidates[i].slot == slot) return i;
    }
    return -1;
}

// ================================================================
// Loop Type Propagation
//
// For each loop, determine which local variables have stable types
// (i.e., every assignment within the loop produces the same type).
// This allows the codegen to seed the type tracker at loop headers
// instead of resetting to TYPE_UNKNOWN, eliminating redundant
// type checks in hot loops.
//
// The analysis is conservative: if ANY assignment to a local could
// produce a different type, we mark it TYPE_UNKNOWN.
// ================================================================

typedef struct {
    AbstractType localTypes[MAX_TRACKED_LOCALS];
} LoopTypeInfo;

/* Merge two abstract types: if they agree, keep; otherwise TYPE_UNKNOWN */
static AbstractType type_merge(AbstractType a, AbstractType b) {
    if (a == b) return a;
    /* First assignment: uninitialized sentinel (we use TYPE_NIL+1 trick, but
       simpler: TYPE_UNKNOWN means "no assignment seen yet" initially,
       but we need to distinguish "never assigned" from "ambiguous".
       We'll use a special init value and handle it in the caller. */
    return TYPE_UNKNOWN;
}

/* Analyze a loop body to determine stable local variable types.
   We do a simplified abstract interpretation: track what the stack
   top type would be at each SET_LOCAL point.

   Strategy: We maintain a small abstract stack to track types through
   the loop bytecode. When we encounter a SET_LOCAL/SET_LOCAL_N_POP,
   we record the type being written. If all writes to a slot agree,
   the slot has a stable type.

   We also check what types flow INTO the loop (pre-loop assignments)
   and verify they agree with in-loop assignments.
*/
/* Forward declarations */
static void analyze_pre_loop_types(ObjFunction* fn, int loop_header,
                                   AbstractType out_types[MAX_TRACKED_LOCALS]);

static void analyze_loop_types(ObjFunction* fn, int start_ip, int end_ip,
                               LoopTypeInfo* out) {
    uint8_t* code = fn->chunk.code;
    int code_len = fn->chunk.count;

    /* Track: for each local slot, what types have been assigned to it?
       -1 = never assigned, otherwise the AbstractType.
       We use a separate "assigned" flag.  */
    bool slot_assigned[MAX_TRACKED_LOCALS] = {0};
    AbstractType slot_types[MAX_TRACKED_LOCALS];
    for (int i = 0; i < MAX_TRACKED_LOCALS; i++) {
        out->localTypes[i] = TYPE_UNKNOWN;
    }

    /* CRITICAL: Seed initial slot types from pre-loop context.
       Without this, slot_types starts as TYPE_UNKNOWN, so when we encounter
       OP_GET_LOCAL for a loop variable, we push TYPE_UNKNOWN.
       Then OP_INCREMENT on TYPE_UNKNOWN stays TYPE_UNKNOWN,
       and we record TYPE_UNKNOWN for the SET_LOCAL -- losing all type info.
       By seeding from pre-loop init (e.g. "var i = 0" → TYPE_INT),
       the flow becomes: get_local→TYPE_INT → increment→TYPE_INT → set_local→TYPE_INT. */
    analyze_pre_loop_types(fn, start_ip, slot_types);

    /* Simple abstract stack for type tracking (up to 16 deep) */
    AbstractType astack[32];
    int adepth = 0;

    #define APUSH(t) do { if (adepth < 32) astack[adepth++] = (t); } while(0)
    #define APOP()   (adepth > 0 ? astack[--adepth] : TYPE_UNKNOWN)
    #define APEEK(n) (adepth > (n) ? astack[adepth - 1 - (n)] : TYPE_UNKNOWN)

    /* Record a write to a local slot */
    #define RECORD_SLOT_WRITE(slot, wtype) do { \
        if ((slot) < MAX_TRACKED_LOCALS) { \
            if (!slot_assigned[slot]) { \
                slot_assigned[slot] = true; \
                slot_types[slot] = (wtype); \
            } else { \
                slot_types[slot] = type_merge(slot_types[slot], (wtype)); \
            } \
        } \
    } while(0)

    int ip = start_ip;
    while (ip < end_ip && ip < code_len) {
        uint8_t op = code[ip++];

        switch (op) {
        /* ---- Constants push known types ---- */
        case BTL_OP_INT_0: case BTL_OP_INT_1: case BTL_OP_INT_2:
            APUSH(TYPE_INT); break;
        case BTL_OP_0: case BTL_OP_1: case BTL_OP_2:
            APUSH(TYPE_NUMBER); break;
        case BTL_OP_NULL:
            APUSH(TYPE_NIL); break;
        case BTL_OP_TRUE: case BTL_OP_FALSE:
            APUSH(TYPE_BOOL); break;
        case BTL_OP_CONSTANT: {
            uint8_t cidx = code[ip++];
            BtlValue cv = fn->chunk.constants.values[cidx];
            if (IS_INT(cv)) APUSH(TYPE_INT);
            else if (IS_NUMBER(cv)) APUSH(TYPE_NUMBER);
            else if (IS_STRING(cv)) APUSH(TYPE_STRING);
            else APUSH(TYPE_UNKNOWN);
            break;
        }
        case BTL_OP_CONSTANT_LONG: {
            ip += 2;
            APUSH(TYPE_UNKNOWN);
            break;
        }

        /* ---- Local gets push the local's tracked type ---- */
        case BTL_OP_GET_LOCAL_0: case BTL_OP_GET_LOCAL_1: case BTL_OP_GET_LOCAL_2:
        case BTL_OP_GET_LOCAL_3: case BTL_OP_GET_LOCAL_4: case BTL_OP_GET_LOCAL_5:
        case BTL_OP_GET_LOCAL_6: case BTL_OP_GET_LOCAL_7: {
            int slot = op - BTL_OP_GET_LOCAL_0;
            /* Push the slot's current known type */
            APUSH(slot < MAX_TRACKED_LOCALS ? slot_types[slot] : TYPE_UNKNOWN);
            break;
        }
        case BTL_OP_GET_LOCAL: {
            uint8_t slot = code[ip++];
            APUSH(slot < MAX_TRACKED_LOCALS ? slot_types[slot] : TYPE_UNKNOWN);
            break;
        }

        /* ---- Local sets record the stack-top type ---- */
        case BTL_OP_SET_LOCAL_0: case BTL_OP_SET_LOCAL_1: case BTL_OP_SET_LOCAL_2:
        case BTL_OP_SET_LOCAL_3: case BTL_OP_SET_LOCAL_4: case BTL_OP_SET_LOCAL_5:
        case BTL_OP_SET_LOCAL_6: case BTL_OP_SET_LOCAL_7: {
            int slot = op - BTL_OP_SET_LOCAL_0;
            AbstractType wt = APEEK(0);
            RECORD_SLOT_WRITE(slot, wt);
            break;
        }
        case BTL_OP_SET_LOCAL: {
            uint8_t slot = code[ip++];
            AbstractType wt = APEEK(0);
            RECORD_SLOT_WRITE(slot, wt);
            break;
        }
        case BTL_OP_SET_LOCAL_0_POP: case BTL_OP_SET_LOCAL_1_POP:
        case BTL_OP_SET_LOCAL_2_POP: case BTL_OP_SET_LOCAL_3_POP:
        case BTL_OP_SET_LOCAL_4_POP: case BTL_OP_SET_LOCAL_5_POP:
        case BTL_OP_SET_LOCAL_6_POP: case BTL_OP_SET_LOCAL_7_POP: {
            int slot = op - BTL_OP_SET_LOCAL_0_POP;
            AbstractType wt = APOP();
            RECORD_SLOT_WRITE(slot, wt);
            break;
        }

        /* ---- Increment/decrement: preserves int type ---- */
        case BTL_OP_INCREMENT: {
            AbstractType t = APEEK(0);
            /* int stays int, number stays number */
            if (t != TYPE_INT && t != TYPE_NUMBER) {
                astack[adepth > 0 ? adepth - 1 : 0] = TYPE_UNKNOWN;
            }
            break;
        }
        case BTL_OP_DECREMENT: {
            AbstractType t = APEEK(0);
            if (t != TYPE_INT && t != TYPE_NUMBER) {
                astack[adepth > 0 ? adepth - 1 : 0] = TYPE_UNKNOWN;
            }
            break;
        }
        case BTL_OP_INC_LOCAL_POP: {
            uint8_t slot = code[ip++];
            /* Increments slot in-place: preserves type */
            AbstractType ct = (slot < MAX_TRACKED_LOCALS) ? slot_types[slot] : TYPE_UNKNOWN;
            if (ct == TYPE_INT || ct == TYPE_NUMBER)
                RECORD_SLOT_WRITE(slot, ct);
            else
                RECORD_SLOT_WRITE(slot, TYPE_UNKNOWN);
            break;
        }
        case BTL_OP_INC_LOCAL: {
            uint8_t slot = code[ip++];
            AbstractType ct = (slot < MAX_TRACKED_LOCALS) ? slot_types[slot] : TYPE_UNKNOWN;
            if (ct == TYPE_INT || ct == TYPE_NUMBER) {
                RECORD_SLOT_WRITE(slot, ct);
                APUSH(ct);
            } else {
                RECORD_SLOT_WRITE(slot, TYPE_UNKNOWN);
                APUSH(TYPE_UNKNOWN);
            }
            break;
        }

        /* ---- Arithmetic ops: int+int→int, number+number→number, else unknown ---- */
        case BTL_OP_ADD: {
            AbstractType b = APOP(), a = APOP();
            if (a == TYPE_INT && b == TYPE_INT) APUSH(TYPE_INT);
            else if (a == TYPE_NUMBER && b == TYPE_NUMBER) APUSH(TYPE_NUMBER);
            else if (a == TYPE_STRING && b == TYPE_STRING) APUSH(TYPE_STRING);
            else APUSH(TYPE_UNKNOWN);
            break;
        }
        case BTL_OP_SUBTRACT: case BTL_OP_MULTIPLY: case BTL_OP_DIVIDE:
        case BTL_OP_MODULO: {
            AbstractType b = APOP(), a = APOP();
            if (a == TYPE_INT && b == TYPE_INT) APUSH(TYPE_INT);
            else if (a == TYPE_NUMBER && b == TYPE_NUMBER) APUSH(TYPE_NUMBER);
            else APUSH(TYPE_UNKNOWN);
            break;
        }
        case BTL_OP_NEGATE: {
            /* Preserves int/number */
            AbstractType t = APEEK(0);
            if (t != TYPE_INT && t != TYPE_NUMBER) {
                astack[adepth > 0 ? adepth - 1 : 0] = TYPE_UNKNOWN;
            }
            break;
        }

        /* ---- Comparisons produce booleans ---- */
        case BTL_OP_LESS: case BTL_OP_GREATER: case BTL_OP_EQUAL:
        case BTL_OP_NOT: {
            if (op == BTL_OP_NOT) { APOP(); }
            else { APOP(); APOP(); }
            APUSH(TYPE_BOOL);
            break;
        }

        /* ---- Stack manipulation ---- */
        case BTL_OP_POP:
            APOP(); break;
        case BTL_OP_POP_N: {
            uint8_t n = code[ip++];
            for (int i = 0; i < n && adepth > 0; i++) adepth--;
            break;
        }
        case BTL_OP_DUP: {
            AbstractType dup_t = APEEK(0);
            APUSH(dup_t); break;
        }
        case BTL_OP_SWAP: {
            if (adepth >= 2) {
                AbstractType tmp = astack[adepth - 1];
                astack[adepth - 1] = astack[adepth - 2];
                astack[adepth - 2] = tmp;
            }
            break;
        }

        /* ---- Globals push unknown ---- */
        case BTL_OP_GET_GLOBAL: case BTL_OP_DEFINE_GLOBAL: case BTL_OP_SET_GLOBAL:
            ip += 1;
            if (op == BTL_OP_GET_GLOBAL) APUSH(TYPE_UNKNOWN);
            else if (op == BTL_OP_DEFINE_GLOBAL) APOP();
            /* SET_GLOBAL peeks, no stack change */
            break;
        case BTL_OP_GET_GLOBAL_LONG: case BTL_OP_DEFINE_GLOBAL_LONG:
        case BTL_OP_SET_GLOBAL_LONG:
            ip += 2;
            if (op == BTL_OP_GET_GLOBAL_LONG) APUSH(TYPE_UNKNOWN);
            else if (op == BTL_OP_DEFINE_GLOBAL_LONG) APOP();
            break;

        /* ---- Upvalues push unknown ---- */
        case BTL_OP_GET_UPVALUE: case BTL_OP_GET_UPVALUE_OPEN:
        case BTL_OP_GET_UPVALUE_CLOSED: case BTL_OP_GET_UPVALUE_IMMUTABLE:
            ip += 1; APUSH(TYPE_UNKNOWN); break;
        case BTL_OP_SET_UPVALUE: case BTL_OP_SET_UPVALUE_OPEN:
        case BTL_OP_SET_UPVALUE_CLOSED:
            ip += 1; break;

        /* ---- Jumps: we just skip operands, don't follow branches ---- */
        case BTL_OP_JUMP: case BTL_OP_LOOP:
            ip += 2; break;
        case BTL_OP_JUMP_IF_FALSE: case BTL_OP_JUMP_IF_TRUE:
            ip += 2; break;  /* peek, no pop */
        case BTL_OP_POP_JUMP_IF_FALSE: case BTL_OP_POP_JUMP_IF_TRUE:
            ip += 2; APOP(); break;
        case BTL_OP_JUMP_IF_NOT_EQUAL: case BTL_OP_JUMP_IF_EQUAL:
            ip += 2; APOP(); APOP(); break;
        case BTL_OP_JUMP_IF_NOT_GREATER: case BTL_OP_JUMP_IF_NOT_LESS:
            ip += 2; APOP(); APOP(); break;

        /* ---- Calls and invokes: can't predict result type, reset ---- */
        case BTL_OP_CALL_0: case BTL_OP_CALL_1: case BTL_OP_CALL_2:
        case BTL_OP_CALL_3: case BTL_OP_CALL_4: case BTL_OP_CALL_5:
        case BTL_OP_CALL_6: case BTL_OP_CALL_7: case BTL_OP_CALL_8: {
            int argc = op - BTL_OP_CALL_0;
            for (int i = 0; i < argc + 1; i++) APOP(); /* pop args + callee */
            APUSH(TYPE_UNKNOWN); /* result */
            break;
        }
        case BTL_OP_CALL: {
            uint8_t argc = code[ip++];
            for (int i = 0; i < argc + 1; i++) APOP();
            APUSH(TYPE_UNKNOWN);
            break;
        }
        case BTL_OP_TAIL_CALL_0: case BTL_OP_TAIL_CALL_1: case BTL_OP_TAIL_CALL_2:
        case BTL_OP_TAIL_CALL_3: case BTL_OP_TAIL_CALL_4: case BTL_OP_TAIL_CALL_5:
        case BTL_OP_TAIL_CALL_6: case BTL_OP_TAIL_CALL_7: case BTL_OP_TAIL_CALL_8: {
            int argc = op - BTL_OP_TAIL_CALL_0;
            for (int i = 0; i < argc + 1; i++) APOP();
            APUSH(TYPE_UNKNOWN);
            break;
        }
        case BTL_OP_TAIL_CALL: {
            uint8_t argc = code[ip++];
            for (int i = 0; i < argc + 1; i++) APOP();
            APUSH(TYPE_UNKNOWN);
            break;
        }

        /* ---- Return ---- */
        case BTL_OP_RETURN:
            APOP();
            break;

        /* ---- Property access: unknown result ---- */
        case BTL_OP_GET_PROPERTY_IC:
            ip += 2; APOP(); APUSH(TYPE_UNKNOWN); break;
        case BTL_OP_SET_PROPERTY_IC:
            ip += 2; APOP(); /* pop value, receiver stays */
            break;

        /* ---- Invoke: pop receiver+args, push unknown result ---- */
        case BTL_OP_INVOKE_IC: case BTL_OP_TAIL_INVOKE_IC:
            ip += 3; /* nameIdx, argc, icSlot */
            { uint8_t argc = code[ip - 2]; /* argc is the second byte */
              for (int i = 0; i < argc + 1; i++) APOP();
              APUSH(TYPE_UNKNOWN); }
            break;
        case BTL_OP_INVOKE_0: case BTL_OP_INVOKE_1: case BTL_OP_INVOKE_2:
        case BTL_OP_INVOKE_3: case BTL_OP_INVOKE_4: case BTL_OP_INVOKE_5:
        case BTL_OP_INVOKE_6: case BTL_OP_INVOKE_7: case BTL_OP_INVOKE_8: {
            int argc = op - BTL_OP_INVOKE_0;
            ip += 1; /* nameIdx */
            for (int i = 0; i < argc + 1; i++) APOP();
            APUSH(TYPE_UNKNOWN);
            break;
        }
        case BTL_OP_TAIL_INVOKE_0: case BTL_OP_TAIL_INVOKE_1: case BTL_OP_TAIL_INVOKE_2:
        case BTL_OP_TAIL_INVOKE_3: case BTL_OP_TAIL_INVOKE_4: case BTL_OP_TAIL_INVOKE_5:
        case BTL_OP_TAIL_INVOKE_6: case BTL_OP_TAIL_INVOKE_7: case BTL_OP_TAIL_INVOKE_8: {
            int argc = op - BTL_OP_TAIL_INVOKE_0;
            ip += 1;
            for (int i = 0; i < argc + 1; i++) APOP();
            APUSH(TYPE_UNKNOWN);
            break;
        }
        case BTL_OP_INVOKE: case BTL_OP_TAIL_INVOKE:
        case BTL_OP_SUPER_INVOKE: case BTL_OP_TAIL_SUPER_INVOKE:
            ip += 2; /* nameIdx + argc */
            adepth = 0; /* Too complex, reset */
            break;
        case BTL_OP_INVOKE_LONG: case BTL_OP_TAIL_INVOKE_LONG:
        case BTL_OP_SUPER_INVOKE_LONG: case BTL_OP_TAIL_SUPER_INVOKE_LONG:
            ip += 3;
            adepth = 0;
            break;
        case BTL_OP_SUPER_INVOKE_0: case BTL_OP_SUPER_INVOKE_1: case BTL_OP_SUPER_INVOKE_2:
        case BTL_OP_SUPER_INVOKE_3: case BTL_OP_SUPER_INVOKE_4: case BTL_OP_SUPER_INVOKE_5:
        case BTL_OP_SUPER_INVOKE_6: case BTL_OP_SUPER_INVOKE_7: case BTL_OP_SUPER_INVOKE_8:
        case BTL_OP_TAIL_SUPER_INVOKE_0: case BTL_OP_TAIL_SUPER_INVOKE_1: case BTL_OP_TAIL_SUPER_INVOKE_2:
        case BTL_OP_TAIL_SUPER_INVOKE_3: case BTL_OP_TAIL_SUPER_INVOKE_4: case BTL_OP_TAIL_SUPER_INVOKE_5:
        case BTL_OP_TAIL_SUPER_INVOKE_6: case BTL_OP_TAIL_SUPER_INVOKE_7: case BTL_OP_TAIL_SUPER_INVOKE_8:
            ip += 1;
            adepth = 0;
            break;

        /* ---- Object ops: skip operands ---- */
        case BTL_OP_CLASS: case BTL_OP_BUILD_LIST: case BTL_OP_BUILD_TABLE:
        case BTL_OP_IMPORT: case BTL_OP_DO_NEW: case BTL_OP_GET_SUPER:
        case BTL_OP_FIELD: case BTL_OP_GET_FIELD_THIS: case BTL_OP_SET_FIELD_THIS:
            ip += 1; adepth = 0; break;
        case BTL_OP_CLASS_LONG: case BTL_OP_IMPORT_LONG:
        case BTL_OP_GET_SUPER_LONG: case BTL_OP_METHOD: case BTL_OP_DO_INVOKE:
            ip += 2; adepth = 0; break;
        case BTL_OP_METHOD_LONG:
            ip += 3; adepth = 0; break;

        /* ---- Closure: complex operand, skip ---- */
        case BTL_OP_CLOSURE: {
            uint8_t fn_idx = code[ip++];
            BtlValue fn_val = fn->chunk.constants.values[fn_idx];
            if (IS_OBJ(fn_val) && OBJ_TYPE(fn_val) == BTL_OBJ_FUNCTION) {
                ObjFunction* child = AS_FUNCTION(fn_val);
                ip += child->upvalueCount * 3;
            }
            APUSH(TYPE_OBJECT);
            break;
        }
        case BTL_OP_CLOSURE_LONG: {
            uint16_t fn_idx = (uint16_t)((code[ip] << 8) | code[ip + 1]); ip += 2;
            BtlValue fn_val = fn->chunk.constants.values[fn_idx];
            if (IS_OBJ(fn_val) && OBJ_TYPE(fn_val) == BTL_OBJ_FUNCTION) {
                ObjFunction* child = AS_FUNCTION(fn_val);
                ip += child->upvalueCount * 3;
            }
            APUSH(TYPE_OBJECT);
            break;
        }

        /* ---- Anything we don't model: reset abstract stack ---- */
        default:
            adepth = 0;
            break;
        }
    }

    #undef APUSH
    #undef APOP
    #undef APEEK
    #undef RECORD_SLOT_WRITE

    /* Now check: for each slot that was assigned in the loop,
       if the type is consistent (INT or NUMBER), propagate it.
       For slots NOT assigned in the loop, check what type the
       initialization (before the loop) sets them to.
    */
    for (int s = 0; s < MAX_TRACKED_LOCALS; s++) {
        if (slot_assigned[s]) {
            AbstractType st = slot_types[s];
            /* Propagate all non-unknown types for loop type seeding */
            if (st != TYPE_UNKNOWN) {
                out->localTypes[s] = st;
            }
        }
        /* Also propagate pre-loop types for read-only slots (locals that are
           initialized before the loop and only read inside it) */
        if (!slot_assigned[s] && slot_types[s] != TYPE_UNKNOWN) {
            out->localTypes[s] = slot_types[s];
        }
    }
}

/* Analyze what type each local slot has when control reaches loop_header.
   Does a forward abstract interpretation from the function entry up to the
   loop header, tracking the abstract stack (which maps to local slots in
   BTL's stack-based local model). This correctly handles for-loop variables
   which are initialized by a bare push (no SET_LOCAL), as well as explicit
   SET_LOCAL_N_POP patterns.
   Populates out_types[0..MAX_TRACKED_LOCALS-1] with the determined types. */
static void analyze_pre_loop_types(ObjFunction* fn, int loop_header,
                                   AbstractType out_types[MAX_TRACKED_LOCALS]) {
    uint8_t* code = fn->chunk.code;
    int code_len = fn->chunk.count;

    for (int i = 0; i < MAX_TRACKED_LOCALS; i++)
        out_types[i] = TYPE_UNKNOWN;

    /* Track an abstract stack: local variables in BTL are stack slots.
       Slot 0 is the function itself, slot 1+ are locals.
       A PUSH that isn't consumed by POP effectively creates a new local. */
    AbstractType astack[32];
    int adepth = 0;

    /* Also track explicit local assignments */
    AbstractType local_types[MAX_TRACKED_LOCALS];
    bool local_set[MAX_TRACKED_LOCALS];
    for (int i = 0; i < MAX_TRACKED_LOCALS; i++) {
        local_types[i] = TYPE_UNKNOWN;
        local_set[i] = false;
    }

    #define PLT_PUSH(t) do { if (adepth < 32) astack[adepth++] = (t); } while(0)
    #define PLT_POP()   (adepth > 0 ? astack[--adepth] : TYPE_UNKNOWN)
    #define PLT_PEEK(n) (adepth > (n) ? astack[adepth - 1 - (n)] : TYPE_UNKNOWN)

    int ip = 0;
    while (ip < loop_header && ip < code_len) {
        uint8_t op = code[ip++];
        switch (op) {
        case BTL_OP_INT_0: case BTL_OP_INT_1: case BTL_OP_INT_2:
            PLT_PUSH(TYPE_INT); break;
        case BTL_OP_0: case BTL_OP_1: case BTL_OP_2:
            PLT_PUSH(TYPE_NUMBER); break;
        case BTL_OP_NULL:
            PLT_PUSH(TYPE_NIL); break;
        case BTL_OP_TRUE: case BTL_OP_FALSE:
            PLT_PUSH(TYPE_BOOL); break;
        case BTL_OP_CONSTANT: {
            uint8_t cidx = code[ip++];
            BtlValue cv = fn->chunk.constants.values[cidx];
            if (IS_INT(cv)) PLT_PUSH(TYPE_INT);
            else if (IS_NUMBER(cv)) PLT_PUSH(TYPE_NUMBER);
            else if (IS_STRING(cv)) PLT_PUSH(TYPE_STRING);
            else PLT_PUSH(TYPE_UNKNOWN);
            break;
        }
        case BTL_OP_CONSTANT_LONG:
            ip += 2; PLT_PUSH(TYPE_UNKNOWN); break;

        case BTL_OP_POP: PLT_POP(); break;
        case BTL_OP_POP_N: { uint8_t n = code[ip++]; for (int i = 0; i < n; i++) PLT_POP(); break; }
        case BTL_OP_DUP: { AbstractType dt = PLT_PEEK(0); PLT_PUSH(dt); break; }

        case BTL_OP_GET_LOCAL_0: case BTL_OP_GET_LOCAL_1: case BTL_OP_GET_LOCAL_2:
        case BTL_OP_GET_LOCAL_3: case BTL_OP_GET_LOCAL_4: case BTL_OP_GET_LOCAL_5:
        case BTL_OP_GET_LOCAL_6: case BTL_OP_GET_LOCAL_7: {
            int slot = op - BTL_OP_GET_LOCAL_0;
            PLT_PUSH(slot < MAX_TRACKED_LOCALS && local_set[slot] ? local_types[slot] : TYPE_UNKNOWN);
            break;
        }
        case BTL_OP_GET_LOCAL: {
            uint8_t slot = code[ip++];
            PLT_PUSH(slot < MAX_TRACKED_LOCALS && local_set[slot] ? local_types[slot] : TYPE_UNKNOWN);
            break;
        }
        case BTL_OP_SET_LOCAL_0: case BTL_OP_SET_LOCAL_1: case BTL_OP_SET_LOCAL_2:
        case BTL_OP_SET_LOCAL_3: case BTL_OP_SET_LOCAL_4: case BTL_OP_SET_LOCAL_5:
        case BTL_OP_SET_LOCAL_6: case BTL_OP_SET_LOCAL_7: {
            int slot = op - BTL_OP_SET_LOCAL_0;
            if (slot < MAX_TRACKED_LOCALS) { local_types[slot] = PLT_PEEK(0); local_set[slot] = true; }
            break;
        }
        case BTL_OP_SET_LOCAL: {
            uint8_t slot = code[ip++];
            if (slot < MAX_TRACKED_LOCALS) { local_types[slot] = PLT_PEEK(0); local_set[slot] = true; }
            break;
        }
        case BTL_OP_SET_LOCAL_0_POP: case BTL_OP_SET_LOCAL_1_POP:
        case BTL_OP_SET_LOCAL_2_POP: case BTL_OP_SET_LOCAL_3_POP:
        case BTL_OP_SET_LOCAL_4_POP: case BTL_OP_SET_LOCAL_5_POP:
        case BTL_OP_SET_LOCAL_6_POP: case BTL_OP_SET_LOCAL_7_POP: {
            int slot = op - BTL_OP_SET_LOCAL_0_POP;
            AbstractType wt = PLT_POP();
            if (slot < MAX_TRACKED_LOCALS) { local_types[slot] = wt; local_set[slot] = true; }
            break;
        }

        case BTL_OP_DEFINE_GLOBAL: ip += 1; PLT_POP(); break;
        case BTL_OP_DEFINE_GLOBAL_LONG: ip += 2; PLT_POP(); break;
        case BTL_OP_GET_GLOBAL: ip += 1; PLT_PUSH(TYPE_UNKNOWN); break;
        case BTL_OP_GET_GLOBAL_LONG: ip += 2; PLT_PUSH(TYPE_UNKNOWN); break;
        case BTL_OP_SET_GLOBAL: ip += 1; break; /* peek, no pop */
        case BTL_OP_SET_GLOBAL_LONG: ip += 2; break;

        case BTL_OP_GET_UPVALUE: case BTL_OP_GET_UPVALUE_OPEN:
        case BTL_OP_GET_UPVALUE_CLOSED: case BTL_OP_GET_UPVALUE_IMMUTABLE:
            ip += 1; PLT_PUSH(TYPE_UNKNOWN); break;
        case BTL_OP_SET_UPVALUE: case BTL_OP_SET_UPVALUE_OPEN:
        case BTL_OP_SET_UPVALUE_CLOSED:
            ip += 1; break;

        case BTL_OP_IMPORT: ip += 1; PLT_PUSH(TYPE_UNKNOWN); break;
        case BTL_OP_IMPORT_LONG: ip += 2; PLT_PUSH(TYPE_UNKNOWN); break;

        /* Jumps: skip operands. We don't follow branches for pre-loop analysis. */
        case BTL_OP_JUMP: case BTL_OP_LOOP: ip += 2; break;
        case BTL_OP_JUMP_IF_FALSE: case BTL_OP_JUMP_IF_TRUE: ip += 2; break;
        case BTL_OP_POP_JUMP_IF_FALSE: case BTL_OP_POP_JUMP_IF_TRUE: ip += 2; PLT_POP(); break;
        case BTL_OP_JUMP_IF_NOT_EQUAL: case BTL_OP_JUMP_IF_EQUAL: ip += 2; PLT_POP(); PLT_POP(); break;
        case BTL_OP_JUMP_IF_NOT_GREATER: case BTL_OP_JUMP_IF_NOT_LESS: ip += 2; PLT_POP(); PLT_POP(); break;

        /* Calls: unknown result */
        case BTL_OP_CALL_0: case BTL_OP_CALL_1: case BTL_OP_CALL_2:
        case BTL_OP_CALL_3: case BTL_OP_CALL_4: case BTL_OP_CALL_5:
        case BTL_OP_CALL_6: case BTL_OP_CALL_7: case BTL_OP_CALL_8: {
            int argc = op - BTL_OP_CALL_0;
            for (int i = 0; i < argc + 1; i++) PLT_POP();
            PLT_PUSH(TYPE_UNKNOWN); break;
        }
        case BTL_OP_CALL: { uint8_t argc = code[ip++]; for (int i = 0; i < argc + 1; i++) PLT_POP(); PLT_PUSH(TYPE_UNKNOWN); break; }

        /* Invoke: complex, just reset */
        case BTL_OP_INVOKE_IC: case BTL_OP_TAIL_INVOKE_IC: ip += 3; adepth = 0; break;

        /* Arithmetic */
        case BTL_OP_ADD: {
            AbstractType b = PLT_POP(), a = PLT_POP();
            if (a == TYPE_INT && b == TYPE_INT) PLT_PUSH(TYPE_INT);
            else if (a == TYPE_NUMBER && b == TYPE_NUMBER) PLT_PUSH(TYPE_NUMBER);
            else if (a == TYPE_STRING && b == TYPE_STRING) PLT_PUSH(TYPE_STRING);
            else PLT_PUSH(TYPE_UNKNOWN);
            break;
        }
        case BTL_OP_SUBTRACT: case BTL_OP_MULTIPLY:
        case BTL_OP_DIVIDE: case BTL_OP_MODULO: {
            AbstractType b = PLT_POP(), a = PLT_POP();
            if (a == TYPE_INT && b == TYPE_INT) PLT_PUSH(TYPE_INT);
            else if (a == TYPE_NUMBER && b == TYPE_NUMBER) PLT_PUSH(TYPE_NUMBER);
            else PLT_PUSH(TYPE_UNKNOWN);
            break;
        }
        case BTL_OP_LESS: case BTL_OP_GREATER: case BTL_OP_EQUAL:
            PLT_POP(); PLT_POP(); PLT_PUSH(TYPE_BOOL); break;
        case BTL_OP_NOT: PLT_POP(); PLT_PUSH(TYPE_BOOL); break;
        case BTL_OP_NEGATE: break; /* preserves type */
        case BTL_OP_INCREMENT: case BTL_OP_DECREMENT: break; /* preserves type */

        case BTL_OP_INC_LOCAL_POP: case BTL_OP_INC_LOCAL: {
            uint8_t slot = code[ip++];
            if (op == BTL_OP_INC_LOCAL) PLT_PUSH(slot < MAX_TRACKED_LOCALS ? local_types[slot] : TYPE_UNKNOWN);
            break;
        }

        case BTL_OP_CLOSURE: {
            uint8_t fn_idx = code[ip++];
            BtlValue fn_val = fn->chunk.constants.values[fn_idx];
            if (IS_OBJ(fn_val) && OBJ_TYPE(fn_val) == BTL_OBJ_FUNCTION) {
                ObjFunction* child = AS_FUNCTION(fn_val);
                ip += child->upvalueCount * 3;
            }
            PLT_PUSH(TYPE_OBJECT); break;
        }
        case BTL_OP_CLOSURE_LONG: {
            uint16_t fn_idx = (uint16_t)((code[ip] << 8) | code[ip + 1]); ip += 2;
            BtlValue fn_val = fn->chunk.constants.values[fn_idx];
            if (IS_OBJ(fn_val) && OBJ_TYPE(fn_val) == BTL_OBJ_FUNCTION) {
                ObjFunction* child = AS_FUNCTION(fn_val);
                ip += child->upvalueCount * 3;
            }
            PLT_PUSH(TYPE_OBJECT); break;
        }

        /* Anything else: bail conservatively */
        default:
            adepth = 0; break;
        }
    }

    #undef PLT_PUSH
    #undef PLT_POP
    #undef PLT_PEEK

    /* The abstract stack represents values pushed above the frame base.
       In BTL, slot 0 = closure (implicit, not pushed by bytecode),
       slot 1..arity = parameters (implicit, not pushed by bytecode).
       So astack[0] corresponds to slot (arity + 1), astack[1] to slot (arity + 2), etc.
       For the <script> function with arity=0, astack[0] → slot 1. */
    int slot_offset = fn->arity + 1;
    for (int i = 0; i < MAX_TRACKED_LOCALS; i++) {
        if (local_set[i]) {
            /* Explicit SET_LOCAL recorded the absolute slot number */
            out_types[i] = local_types[i];
        } else {
            /* Check if this slot was created by a bare push (for-loop init) */
            int stack_idx = i - slot_offset;
            if (stack_idx >= 0 && stack_idx < adepth) {
                out_types[i] = astack[stack_idx];
            }
        }
    }
}


// ================================================================
// Peephole pattern matching
//
// We look ahead in the bytecode to detect common multi-opcode
// sequences and fuse them into single C statements that avoid
// intermediate stack traffic.
// ================================================================

 /* Check if byte at offset is a GET_LOCAL_N opcode, return slot or -1*/
static int is_get_local_at(uint8_t* code, int ip, int code_len) {
    if (ip >= code_len) return -1;
    uint8_t op = code[ip];
    if (op >= BTL_OP_GET_LOCAL_0 && op <= BTL_OP_GET_LOCAL_7)
        return op - BTL_OP_GET_LOCAL_0;
    if (op == BTL_OP_GET_LOCAL && ip + 1 < code_len)
        return code[ip + 1]; /* slot in next byte, but return -2 to signal "has operand"*/
    return -1;
}

/* Check if byte at offset is a SET_LOCAL_N_POP, return slot or -1*/
static int is_set_local_pop_at(uint8_t* code, int ip, int code_len) {
    if (ip >= code_len) return -1;
    uint8_t op = code[ip];
    if (op >= BTL_OP_SET_LOCAL_0_POP && op <= BTL_OP_SET_LOCAL_7_POP)
        return op - BTL_OP_SET_LOCAL_0_POP;
    return -1;
}

// ================================================================
// SYNC/UNSYNC macros
//
// We keep sp as a C local for speed. Before calling into the VM
// (callValue, runtimeError, GC-triggering ops) we must sync.
// After returning, we reload.
// ================================================================

static void emit_sync(BtlTranspiler* t) {
    OUT(t, "    vm->stackTop = sp;\n");
}

static void emit_reload(BtlTranspiler* t) {
    OUT(t, "    sp = vm->stackTop;\n");
    OUT(t, "    frame = &vm->frames[vm->frameCount - 1];\n");
    OUT(t, "    slots = frame->slots;\n");
}

/* Light reload: only sync sp. Use for ops that modify vm->stackTop
// but never push new call frames (frame and slots stay valid).*/
static void emit_light_reload(BtlTranspiler* t) {
    OUT(t, "    sp = vm->stackTop;\n");
}

/* Full sync + reload bracketing for calls that modify the frame*/
static void emit_call_bracket_open(BtlTranspiler* t) {
    emit_sync(t);
}

static void emit_call_bracket_close(BtlTranspiler* t) {
    emit_reload(t);
}

/* Light bracket close: only reload sp. Use for helpers that modify
// vm->stackTop but never push new call frames (frame/slots stay valid).*/
static void emit_light_call_bracket_close(BtlTranspiler* t) {
    emit_light_reload(t);
}

/* Emit inlined super invoke: pop superclass from sp, get method entry,
// inline frame push + compiled handler dispatch. No slow path fallback
// needed since the compiler guarantees method indices are valid.*/
static void emit_inline_super_invoke(BtlTranspiler* t, int methodIdx, int argc) {
    OUT(t, "    { ObjClass* _sc = AS_CLASS(POP(sp));\n");
    OUT(t, "      if (__builtin_expect(%d >= _sc->methodCount || _sc->methods[%d].closure == NULL, 0)) {\n", methodIdx, methodIdx);
    OUT(t, "        vm->stackTop = sp; btl_runtime_error(vm, \"Undefined method in superclass.\"); return BTL_INTERPRET_RUNTIME_ERROR; }\n");
    OUT(t, "      BtlMethodEntry* _me = &_sc->methods[%d];\n", methodIdx);
    OUT(t, "      vm->stackTop = sp;\n");
    OUT(t, "      if (__builtin_expect(vm->frameCount >= vm->frameCapacity, 0)) {\n");
    OUT(t, "        if (!btl_ensure_frame_capacity(vm)) return BTL_INTERPRET_RUNTIME_ERROR;\n");
    OUT(t, "      }\n");
    OUT(t, "      { BtlCallFrame* _nf = &vm->frames[vm->frameCount++];\n");
    OUT(t, "      _nf->closure = _me->closure;\n");
    OUT(t, "      _nf->ip = _me->closure->function->chunk.code;\n");
    OUT(t, "      _nf->slots = vm->stackTop - %d;\n", argc + 1);
    OUT(t, "      _nf->openUpvalues = NULL;\n");
    OUT(t, "      BtlFnPtr _h = (BtlFnPtr)_me->closure->function->compiledHandler;\n");
    OUT(t, "      if (_h) {\n");
    OUT(t, "        BtlInterpretResult _r = _h(vm);\n");
    OUT(t, "        if (_r != BTL_INTERPRET_OK) return _r;\n");
    OUT(t, "      } else {\n");
    OUT(t, "        int _sf = vm->runFloor; vm->runFloor = vm->frameCount - 1;\n");
    OUT(t, "        BtlInterpretResult _r = btl_run(vm);\n");
    OUT(t, "        vm->runFloor = _sf;\n");
    OUT(t, "        if (_r != BTL_INTERPRET_OK) return _r;\n");
    OUT(t, "      } }\n");
    emit_reload(t);
    OUT(t, "    }\n");
}

/* Emit a tail call with self-recursive optimization.
// If the callee is the current function, shuffle args + goto btl_entry.
// Otherwise, fall back to emit_optimized_call.*/
static void emit_tail_call(BtlTranspiler* t, int argc) {
    OUT(t, "    { BtlValue _callee = vm->stackTop[-%d];\n", argc + 1);
    OUT(t, "      if (IS_OBJ(_callee) && OBJ_TYPE(_callee) == BTL_OBJ_CLOSURE\n");
    OUT(t, "          && AS_CLOSURE(_callee)->function == fn) {\n");
    OUT(t, "        /* Self-recursive tail call: shuffle args + goto*/\n");
    OUT(t, "        vm->stackTop = sp;\n");
    /* Only call close_upvalues if this function needs it*/
    if (function_needs_close_upvalues(t->current_fn)) {
        OUT(t, "        btl_compiled_close_upvalues(vm, frame);\n");
    }
    /* Save args from stack before overwriting slots*/
    if (argc > 0) {
        for (int i = 0; i < argc; i++) {
            OUT(t, "        BtlValue _ta%d = vm->stackTop[-%d];\n", i, argc - i);
        }
        for (int i = 0; i < argc; i++) {
            OUT(t, "        slots[%d] = _ta%d;\n", i + 1, i);
        }
    }
    OUT(t, "        sp = slots + %d + 1;\n", argc);
    OUT(t, "        frame->openUpvalues = NULL;\n");
    OUT(t, "        goto btl_entry;\n");
    OUT(t, "      }\n");
    /* Non-self tail call: fall back to regular optimized call*/
    OUT(t, "      { BtlInterpretResult _r;\n");
    OUT(t, "        if (IS_OBJ(_callee) && OBJ_TYPE(_callee) == BTL_OBJ_CLOSURE)\n");
    OUT(t, "            _r = btl_call_direct(vm, AS_CLOSURE(_callee), %d);\n", argc);
    OUT(t, "        else\n");
    OUT(t, "            _r = btl_call_and_run(vm, _callee, %d);\n", argc);
    OUT(t, "        if (_r != BTL_INTERPRET_OK) return _r; }\n");
    OUT(t, "    }\n");
}

/* Emit an optimized call: closure fast path with fully inlined frame setup,
// class fast path with cached init lookup,
// fallback to btl_call_and_run for other callees.
//
// Inlining the frame setup eliminates:
// 1. Function call overhead to btl_call_direct
// 2. Arity check (argc is a compile-time constant)
// 3. Branch prediction for the arity mismatch case
*/
static void emit_optimized_call(BtlTranspiler* t, int argc) {
    OUT(t, "    { BtlValue _callee = vm->stackTop[-%d];\n", argc + 1);
    /* Single IS_OBJ check, then branch on type — avoids redundant IS_OBJ for both paths */
    OUT(t, "      if (__builtin_expect(IS_OBJ(_callee), 1)) {\n");
    OUT(t, "      BtlObjType _ot = OBJ_TYPE(_callee);\n");
    /* Closure first (more common overall), class second */
    OUT(t, "      if (__builtin_expect(_ot == BTL_OBJ_CLOSURE, 1)) {\n");
    OUT(t, "        ObjClosure* _cl = AS_CLOSURE(_callee);\n");
    /* Arity check - argc is known at compile time */
    OUT(t, "        if (__builtin_expect(_cl->function->arity != %d, 0)) {\n", argc);
    OUT(t, "          btl_runtime_error(vm, \"Expected %%d arguments but got %d.\", _cl->function->arity);\n", argc);
    OUT(t, "          return BTL_INTERPRET_RUNTIME_ERROR;\n");
    OUT(t, "        }\n");
    /* Frame capacity check - rare, cold path */
    OUT(t, "        if (__builtin_expect(vm->frameCount >= vm->frameCapacity, 0)) {\n");
    OUT(t, "          if (!btl_ensure_frame_capacity(vm)) return BTL_INTERPRET_RUNTIME_ERROR;\n");
    OUT(t, "        }\n");
    /* Inline frame push */
    OUT(t, "        BtlCallFrame* _nf = &vm->frames[vm->frameCount++];\n");
    OUT(t, "        _nf->closure = _cl;\n");
    OUT(t, "        _nf->ip = _cl->function->chunk.code;\n");
    OUT(t, "        _nf->slots = vm->stackTop - %d;\n", argc + 1);
    OUT(t, "        _nf->openUpvalues = NULL;\n");
    /* Dispatch to compiled handler or interpreter */
    OUT(t, "        BtlFnPtr _h = (BtlFnPtr)_cl->function->compiledHandler;\n");
    OUT(t, "        if (_h) {\n");
    OUT(t, "          BtlInterpretResult _r = _h(vm);\n");
    OUT(t, "          if (_r != BTL_INTERPRET_OK) return _r;\n");
    OUT(t, "        } else {\n");
    OUT(t, "          int _sf = vm->runFloor; vm->runFloor = vm->frameCount - 1;\n");
    OUT(t, "          BtlInterpretResult _r = btl_run(vm);\n");
    OUT(t, "          vm->runFloor = _sf;\n");
    OUT(t, "          if (_r != BTL_INTERPRET_OK) return _r;\n");
    OUT(t, "        }\n");
    OUT(t, "      } else if (_ot == BTL_OBJ_CLASS) {\n");
    OUT(t, "        ObjClass* _klass = AS_CLASS(_callee);\n");
    OUT(t, "        if (__builtin_expect(_klass->nativeConstructor != NULL, 0)) {\n");
    OUT(t, "          BtlValue _r = _klass->nativeConstructor(vm, %d, vm->stackTop - %d);\n", argc, argc);
    OUT(t, "          vm->stackTop -= %d; vm->stackTop[-1] = _r;\n", argc);
    OUT(t, "        } else {\n");
    OUT(t, "        vm->stackTop[-%d] = OBJ_VAL(btl_instance_new(vm, _klass));\n", argc + 1);
    if (argc <= 8) {
        /* Use init cache */
        OUT(t, "        int _initIdx = _klass->initCache[%d];\n", argc);
        OUT(t, "        if (_initIdx == -1) {\n");
        OUT(t, "          /* Cache miss - populate */\n");
        OUT(t, "          static ObjString* _initSig = NULL;\n");
        OUT(t, "          if (!_initSig) { char _s[6] = \"init\"; _s[4] = %d; _initSig = btl_string_copy(vm, _s, 5); }\n", argc);
        OUT(t, "          BtlValue _idx;\n");
        OUT(t, "          if (btl_table_get(&_klass->methodIndices, OBJ_VAL(_initSig), &_idx)) {\n");
        OUT(t, "            _initIdx = (int)AS_NUMBER(_idx); _klass->initCache[%d] = _initIdx;\n", argc);
        OUT(t, "          } else { _klass->initCache[%d] = -2; _initIdx = -2; }\n", argc);
        OUT(t, "        }\n");
        OUT(t, "        if (_initIdx >= 0) {\n");
        OUT(t, "          ObjClosure* _init = _klass->methods[_initIdx].closure;\n");
        if (argc > 0) {
            /* Inline simple init: if init has exactly argc field ICs and they're warm,
               skip the function call and write fields directly */
            OUT(t, "          if (__builtin_expect(_init && _init->function->fieldICCount == %d\n", argc);
            OUT(t, "              && _init->fieldICs[0].fieldIndex >= 0, 1)) {\n");
            OUT(t, "            /* Inlined simple init — direct field writes via warm ICs */\n");
            OUT(t, "            ObjInstance* _inst = AS_INSTANCE(vm->stackTop[-%d]);\n", argc + 1);
            for (int i = 0; i < argc; i++) {
                OUT(t, "            _inst->fields[_init->fieldICs[%d].fieldIndex] = vm->stackTop[-%d];\n", i, argc - i);
            }
            OUT(t, "            vm->stackTop -= %d;\n", argc);
            OUT(t, "          } else if (_init) {\n");
        } else {
            OUT(t, "          if (_init) {\n");
        }
        OUT(t, "            if (__builtin_expect(vm->frameCount >= vm->frameCapacity, 0)) {\n");
        OUT(t, "              if (!btl_ensure_frame_capacity(vm)) return BTL_INTERPRET_RUNTIME_ERROR;\n");
        OUT(t, "            }\n");
        OUT(t, "            BtlCallFrame* _nf = &vm->frames[vm->frameCount++];\n");
        OUT(t, "            _nf->closure = _init;\n");
        OUT(t, "            _nf->ip = _init->function->chunk.code;\n");
        OUT(t, "            _nf->slots = vm->stackTop - %d;\n", argc + 1);
        OUT(t, "            _nf->openUpvalues = NULL;\n");
        OUT(t, "            BtlFnPtr _h = (BtlFnPtr)_init->function->compiledHandler;\n");
        OUT(t, "            if (_h) {\n");
        OUT(t, "              BtlInterpretResult _r = _h(vm);\n");
        OUT(t, "              if (_r != BTL_INTERPRET_OK) return _r;\n");
        OUT(t, "            } else {\n");
        OUT(t, "              int _sf = vm->runFloor; vm->runFloor = vm->frameCount - 1;\n");
        OUT(t, "              BtlInterpretResult _r = btl_run(vm);\n");
        OUT(t, "              vm->runFloor = _sf;\n");
        OUT(t, "              if (_r != BTL_INTERPRET_OK) return _r;\n");
        OUT(t, "            }\n");
        OUT(t, "          }\n");
        if (argc != 0) {
            OUT(t, "        } else if (_initIdx == -2) {\n");
            OUT(t, "          btl_runtime_error(vm, \"Expected 0 arguments but got %d.\");\n", argc);
            OUT(t, "          return BTL_INTERPRET_RUNTIME_ERROR;\n");
        }
        OUT(t, "        }\n");
    } else {
        OUT(t, "        if (!btl_compiled_call_class(vm, _klass, %d)) return BTL_INTERPRET_RUNTIME_ERROR;\n", argc);
    }
    OUT(t, "        } /* end else (non-native constructor) */\n");
    OUT(t, "      } else if (_ot == BTL_OBJ_BOUND_METHOD) {\n");
    OUT(t, "        ObjBoundMethod* _bm = AS_BOUND_METHOD(_callee);\n");
    OUT(t, "        vm->stackTop[-%d] = _bm->receiver;\n", argc + 1);
    OUT(t, "        ObjClosure* _cl = _bm->method;\n");
    /* Arity check */
    OUT(t, "        if (__builtin_expect(_cl->function->arity != %d, 0)) {\n", argc);
    OUT(t, "          btl_runtime_error(vm, \"Expected %%d arguments but got %d.\", _cl->function->arity);\n", argc);
    OUT(t, "          return BTL_INTERPRET_RUNTIME_ERROR;\n");
    OUT(t, "        }\n");
    /* Frame capacity check */
    OUT(t, "        if (__builtin_expect(vm->frameCount >= vm->frameCapacity, 0)) {\n");
    OUT(t, "          if (!btl_ensure_frame_capacity(vm)) return BTL_INTERPRET_RUNTIME_ERROR;\n");
    OUT(t, "        }\n");
    /* Inline frame push */
    OUT(t, "        BtlCallFrame* _nf = &vm->frames[vm->frameCount++];\n");
    OUT(t, "        _nf->closure = _cl;\n");
    OUT(t, "        _nf->ip = _cl->function->chunk.code;\n");
    OUT(t, "        _nf->slots = vm->stackTop - %d;\n", argc + 1);
    OUT(t, "        _nf->openUpvalues = NULL;\n");
    OUT(t, "        BtlFnPtr _h = (BtlFnPtr)_cl->function->compiledHandler;\n");
    OUT(t, "        if (_h) {\n");
    OUT(t, "          BtlInterpretResult _r = _h(vm);\n");
    OUT(t, "          if (_r != BTL_INTERPRET_OK) return _r;\n");
    OUT(t, "        } else {\n");
    OUT(t, "          int _sf = vm->runFloor; vm->runFloor = vm->frameCount - 1;\n");
    OUT(t, "          BtlInterpretResult _r = btl_run(vm);\n");
    OUT(t, "          vm->runFloor = _sf;\n");
    OUT(t, "          if (_r != BTL_INTERPRET_OK) return _r;\n");
    OUT(t, "        }\n");
    OUT(t, "      } else {\n");
    OUT(t, "        BtlInterpretResult _r = btl_call_and_run(vm, _callee, %d);\n", argc);
    OUT(t, "        if (_r != BTL_INTERPRET_OK) return _r;\n");
    OUT(t, "      }\n");
    OUT(t, "      } else {\n");
    OUT(t, "        BtlInterpretResult _r = btl_call_and_run(vm, _callee, %d);\n", argc);
    OUT(t, "        if (_r != BTL_INTERPRET_OK) return _r;\n");
    OUT(t, "      }\n");
    OUT(t, "    }\n");
}

/* Emit a call where we know the callee is slots[0] (self-recursive call).
// This skips the type check since slots[0] is always the current closure.
// We also skip the arity check since self-recursive calls have matching arity.
*/
static void emit_self_recursive_call(BtlTranspiler* t, int argc) {
    OUT(t, "    { /* self-recursive call via slots[0] - fully inlined */\n");
    OUT(t, "      ObjClosure* _cl = frame->closure;\n");
    /* Frame capacity check - rare, cold path */
    OUT(t, "      if (__builtin_expect(vm->frameCount >= vm->frameCapacity, 0)) {\n");
    OUT(t, "        if (!btl_ensure_frame_capacity(vm)) return BTL_INTERPRET_RUNTIME_ERROR;\n");
    OUT(t, "      }\n");
    /* Inline frame push */
    OUT(t, "      BtlCallFrame* _nf = &vm->frames[vm->frameCount++];\n");
    OUT(t, "      _nf->closure = _cl;\n");
    OUT(t, "      _nf->ip = _cl->function->chunk.code;\n");
    OUT(t, "      _nf->slots = vm->stackTop - %d;\n", argc + 1);
    OUT(t, "      _nf->openUpvalues = NULL;\n");
    /* For self-recursive, we know there's a compiled handler */
    OUT(t, "      BtlFnPtr _h = (BtlFnPtr)_cl->function->compiledHandler;\n");
    OUT(t, "      BtlInterpretResult _r = _h(vm);\n");
    OUT(t, "      if (_r != BTL_INTERPRET_OK) return _r;\n");
    OUT(t, "    }\n");
}

/* Check if the callee at a CALL instruction came from GET_LOCAL_0.
// Uses forward scanning from the last jump target before call_ip to find
// the instruction that pushed the callee. Returns true only if we can
// prove the callee is slots[0].
//
// Forward scanning is used instead of backward scanning because backward
// scanning can misinterpret operand bytes as opcodes, leading to incorrect
// self-recursive call optimization.
*/
static bool callee_is_local_0(uint8_t* code, int call_ip, int argc, bool* targets) {
    /* Find the start of the current basic block (last jump target before call_ip)*/
    int block_start = 0;
    for (int i = call_ip - 1; i >= 0; i--) {
        if (targets[i]) { block_start = i; break; }
    }

    /* Forward scan through instructions, tracking which instruction pushed what*/
    /* We maintain a small stack of instruction IPs to know what pushed each value*/
    int stack_ips[256];  /* IP of instruction that pushed each stack value*/
    int stack_depth = 0;

    int ip = block_start;
    while (ip < call_ip) {
        uint8_t op = code[ip];
        int inst_ip = ip;
        int inst_size = 1;  /* default: 1-byte instruction*/

        int net_push = 0;

        switch (op) {
        /* 1-byte, push 1*/
        case BTL_OP_GET_LOCAL_0: case BTL_OP_GET_LOCAL_1: case BTL_OP_GET_LOCAL_2:
        case BTL_OP_GET_LOCAL_3: case BTL_OP_GET_LOCAL_4: case BTL_OP_GET_LOCAL_5:
        case BTL_OP_GET_LOCAL_6: case BTL_OP_GET_LOCAL_7:
        case BTL_OP_0: case BTL_OP_1: case BTL_OP_2:
        case BTL_OP_INT_0: case BTL_OP_INT_1: case BTL_OP_INT_2:
        case BTL_OP_NULL: case BTL_OP_TRUE: case BTL_OP_FALSE:
        case BTL_OP_DUP:
            net_push = 1;
            break;

        /* 2-byte, push 1*/
        case BTL_OP_CONSTANT: case BTL_OP_GET_LOCAL:
        case BTL_OP_GET_GLOBAL: case BTL_OP_GET_UPVALUE:
            net_push = 1;
            inst_size = 2;
            break;

        /* 3-byte, push 1*/
        case BTL_OP_CONSTANT_LONG: case BTL_OP_GET_GLOBAL_LONG:
            net_push = 1;
            inst_size = 3;
            break;

        /* 1-byte binary ops: pop 2, push 1 (net -1)*/
        case BTL_OP_ADD: case BTL_OP_SUBTRACT: case BTL_OP_MULTIPLY:
        case BTL_OP_DIVIDE: case BTL_OP_MODULO:
        case BTL_OP_GREATER: case BTL_OP_LESS: case BTL_OP_EQUAL:
            net_push = -1;
            break;

        /* 1-byte unary ops: pop 1, push 1 (net 0)*/
        case BTL_OP_NEGATE: case BTL_OP_NOT:
        case BTL_OP_INCREMENT: case BTL_OP_DECREMENT:
            net_push = 0;
            break;

        /* 1-byte pop*/
        case BTL_OP_POP:
            net_push = -1;
            break;

        /* 2-byte set local (peek, no pop - net 0)*/
        case BTL_OP_SET_LOCAL:
            net_push = 0;
            inst_size = 2;
            break;

        /* 1-byte set local (peek, no pop - net 0)*/
        case BTL_OP_SET_LOCAL_0: case BTL_OP_SET_LOCAL_1: case BTL_OP_SET_LOCAL_2:
        case BTL_OP_SET_LOCAL_3: case BTL_OP_SET_LOCAL_4: case BTL_OP_SET_LOCAL_5:
        case BTL_OP_SET_LOCAL_6: case BTL_OP_SET_LOCAL_7:
            net_push = 0;
            break;

        /* 1-byte set-and-pop (net -1)*/
        case BTL_OP_SET_LOCAL_0_POP: case BTL_OP_SET_LOCAL_1_POP:
        case BTL_OP_SET_LOCAL_2_POP: case BTL_OP_SET_LOCAL_3_POP:
        case BTL_OP_SET_LOCAL_4_POP: case BTL_OP_SET_LOCAL_5_POP:
        case BTL_OP_SET_LOCAL_6_POP: case BTL_OP_SET_LOCAL_7_POP:
            net_push = -1;
            break;

        default:
            /* Unknown instruction - bail out conservatively*/
            return false;
        }

        /* Update stack tracking*/
        if (net_push < 0) {
            stack_depth += net_push;
            if (stack_depth < 0) return false;
        } else if (net_push > 0) {
            for (int i = 0; i < net_push; i++) {
                if (stack_depth < 256)
                    stack_ips[stack_depth] = inst_ip;
                stack_depth++;
            }
        }

        ip += inst_size;
    }

    /* The callee is at position (stack_depth - 1 - argc) from the bottom*/
    int callee_idx = stack_depth - 1 - argc;
    if (callee_idx < 0 || callee_idx >= 256) return false;

    /* Check if the instruction that pushed the callee was GET_LOCAL_0*/
    int callee_ip = stack_ips[callee_idx];
    return (code[callee_ip] == BTL_OP_GET_LOCAL_0);
}

// ================================================================
// File header emission
// ================================================================

static void emit_header(BtlTranspiler* t) {
    int fn_count = t->fns.count;

    OUT(t, "/*\n");
    OUT(t, "// Generated by BTL transpiler v2 (performance) - DO NOT EDIT\n");
    OUT(t, "*/\n\n");
    OUT(t, "#include \"compiled.h\"\n\n");
    OUT(t, "/* Inline stack operations - avoid function call overhead*/\n");
    OUT(t, "#define PUSH(sp, v)  (*(sp)++ = (v))\n");
    OUT(t, "#define POP(sp)      (*--(sp))\n");
    OUT(t, "#define PEEK(sp, n)  ((sp)[-(n)-1])\n");
    OUT(t, "#define PEEK_SET(sp, n, v) ((sp)[-(n)-1] = (v))\n");
    NL(t);

    /* Forward declare all transpiled functions*/
    for (int i = 0; i < fn_count; i++) {
        OUT(t, "static BtlInterpretResult btl_fn_%d(VM* vm);\n", i);
    }
    NL(t);

    /* ---- Direct dispatch table ----
    // Maps ObjFunction* (from compile()) to transpiled C function pointers.
    // Populated at startup by btl_register_functions(). Looked up at every
    // call site to bypass the interpreter dispatch loop.*/
    OUT(t, "/* ---- Direct dispatch table ----*/\n");
    OUT(t, "typedef BtlInterpretResult (*BtlFnPtr)(VM*);\n");
    OUT(t, "#define BTL_FN_COUNT %d\n", fn_count);
    OUT(t, "static struct { ObjFunction* fn; BtlFnPtr handler; } btl_dispatch[BTL_FN_COUNT];\n\n");

    /* Lookup: O(1) via compiledHandler field on ObjFunction*/
    OUT(t, "static inline BtlFnPtr btl_find_handler(ObjFunction* target) {\n");
    OUT(t, "    return (BtlFnPtr)target->compiledHandler;\n");
    OUT(t, "}\n\n");

    /* Registration: walk the constant pool tree (same order as btl_collect_functions)
    // and pair each ObjFunction* with its btl_fn_N handler.
    // Uses compiledHandler != NULL as O(1) duplicate check instead of linear scan.*/
    OUT(t, "static void btl_register_recursive(ObjFunction* fn, int* idx, BtlFnPtr* handlers) {\n");
    OUT(t, "    if (fn == NULL || fn->compiledHandler != NULL) return;\n");
    OUT(t, "    if (*idx >= BTL_FN_COUNT) return;\n");
    OUT(t, "    btl_dispatch[*idx].fn = fn;\n");
    OUT(t, "    btl_dispatch[*idx].handler = handlers[*idx];\n");
    OUT(t, "    fn->compiledHandler = (void*)handlers[*idx];\n");
    OUT(t, "    (*idx)++;\n");
    OUT(t, "    /* Recurse into constant pool for nested functions*/\n");
    OUT(t, "    for (int i = 0; i < fn->chunk.constants.count; i++) {\n");
    OUT(t, "        BtlValue v = fn->chunk.constants.values[i];\n");
    OUT(t, "        if (IS_OBJ(v) && OBJ_TYPE(v) == BTL_OBJ_FUNCTION)\n");
    OUT(t, "            btl_register_recursive(AS_FUNCTION(v), idx, handlers);\n");
    OUT(t, "    }\n");
    OUT(t, "}\n\n");

    OUT(t, "static void btl_register_functions(ObjFunction* root) {\n");
    OUT(t, "    static BtlFnPtr handlers[BTL_FN_COUNT] = {\n");
    for (int i = 0; i < fn_count; i++) {
        OUT(t, "        btl_fn_%d%s\n", i, (i < fn_count - 1) ? "," : "");
    }
    OUT(t, "    };\n");
    OUT(t, "    int idx = 0;\n");
    OUT(t, "    btl_register_recursive(root, &idx, handlers);\n");
    OUT(t, "}\n\n");

    /* ---- btl_call_and_run with direct dispatch ----
    // For closure calls: look up the function in the dispatch table.
    // If found, push the frame via callBtlValue then run the transpiled C
    // function directly instead of entering the interpreter loop.*/
    OUT(t, "/*\n");
    OUT(t, "// Frame-guarded call with direct dispatch.\n");
    OUT(t, "// If callee is a closure with a transpiled handler, we call it directly.\n");
    OUT(t, "// Otherwise fall back to btl_call_value() + run() (interpreter).\n");
    OUT(t, "*/\n");
    OUT(t, "static inline BtlInterpretResult btl_call_and_run(VM* vm, BtlValue callee, int argc) {\n");
    OUT(t, "    int fc = vm->frameCount;\n");
    OUT(t, "    if (!btl_call_value(vm, callee, argc)) return BTL_INTERPRET_RUNTIME_ERROR;\n");
    OUT(t, "    if (vm->frameCount > fc) {\n");
    OUT(t, "        /* A new frame was pushed -- need to execute it*/\n");
    OUT(t, "        ObjFunction* target = vm->frames[fc].closure->function;\n");
    OUT(t, "        BtlFnPtr handler = btl_find_handler(target);\n");
    OUT(t, "        if (handler) {\n");
    OUT(t, "            return handler(vm);\n");
    OUT(t, "        }\n");
    OUT(t, "        /* Unknown function -- fall back to interpreter*/\n");
    OUT(t, "        int savedFloor = vm->runFloor;\n");
    OUT(t, "        vm->runFloor = fc;\n");
    OUT(t, "        BtlInterpretResult r = btl_run(vm);\n");
    OUT(t, "        vm->runFloor = savedFloor;\n");
    OUT(t, "        if (r != BTL_INTERPRET_OK) return r;\n");
    OUT(t, "    }\n");
    OUT(t, "    return BTL_INTERPRET_OK;\n");
    OUT(t, "}\n\n");

    /* Fast path for closure calls: inline frame setup + dispatch table lookup.
    // Skips callValue's type dispatch entirely for the common closure case.*/
    OUT(t, "/*\n");
    OUT(t, "// Direct closure call: inline frame setup + transpiled dispatch.\n");
    OUT(t, "// Bypasses btl_call_value() entirely for known closure targets.\n");
    OUT(t, "*/\n");
    OUT(t, "static inline BtlInterpretResult btl_call_direct(VM* vm, ObjClosure* closure, int argc) {\n");
    OUT(t, "    if (__builtin_expect(argc != closure->function->arity, 0)) {\n");
    OUT(t, "        btl_runtime_error(vm, \"Expected %%d arguments but got %%d.\",\n");
    OUT(t, "                     closure->function->arity, argc);\n");
    OUT(t, "        return BTL_INTERPRET_RUNTIME_ERROR;\n");
    OUT(t, "    }\n");
    OUT(t, "    /* Grow frame stack if needed - inline the fast path*/\n");
    OUT(t, "    if (__builtin_expect(vm->frameCount >= vm->frameCapacity, 0)) {\n");
    OUT(t, "        if (!btl_ensure_frame_capacity(vm)) {\n");
    OUT(t, "            btl_runtime_error(vm, \"Out of memory: cannot grow call stack.\");\n");
    OUT(t, "            return BTL_INTERPRET_RUNTIME_ERROR;\n");
    OUT(t, "        }\n");
    OUT(t, "    }\n");
    OUT(t, "    /* Inline frame push (same as vm.c call())*/\n");
    OUT(t, "    BtlCallFrame* newFrame = &vm->frames[vm->frameCount++];\n");
    OUT(t, "    newFrame->closure = closure;\n");
    OUT(t, "    newFrame->ip = closure->function->chunk.code;\n");
    OUT(t, "    newFrame->slots = vm->stackTop - argc - 1;\n");
    OUT(t, "    newFrame->openUpvalues = NULL;\n");
    OUT(t, "    /* Dispatch to transpiled handler or interpreter*/\n");
    OUT(t, "    BtlFnPtr handler = btl_find_handler(closure->function);\n");
    OUT(t, "    if (handler) return handler(vm);\n");
    OUT(t, "    int savedFloor = vm->runFloor;\n");
    OUT(t, "    vm->runFloor = vm->frameCount - 1;\n");
    OUT(t, "    BtlInterpretResult r = btl_run(vm);\n");
    OUT(t, "    vm->runFloor = savedFloor;\n");
    OUT(t, "    return r;\n");
    OUT(t, "}\n\n");
}

// ================================================================
// Try to fuse a common opcode pattern. Returns number of bytes
// consumed (0 if no pattern matched, meaning fall through to
// single-opcode emit).
//
// PATTERN 1: GET_LOCAL(a) GET_LOCAL(b) <arith> SET_LOCAL_N_POP(c)
//   Ã¢â€ â€™ slots[c] = NUMBER_VAL(AS_NUMBER(slots[a]) OP AS_NUMBER(slots[b]))
//   Eliminates 4 push/pop operations.
//
// PATTERN 2: GET_LOCAL(a) GET_LOCAL(b) LESS/GREATER POP_JUMP_IF_FALSE
//   Ã¢â€ â€™ if (!(AS_NUMBER(slots[a]) < AS_NUMBER(slots[b]))) goto L;
//   Eliminates 3 push/pop + a falsey check.
//
// PATTERN 3: GET_LOCAL(a) <const> <arith> SET_LOCAL_N_POP(a)
//   Ã¢â€ â€™ slots[a] = NUMBER_VAL(AS_NUMBER(slots[a]) OP const)
//   Common in loops like `i = i + 1`.
// ================================================================

static int try_fuse(BtlTranspiler* t, uint8_t* code, int ip, int code_len,
    bool* targets, TypeState* ts) {
    /* Don't fuse across jump targets Ã¢â‚¬â€ any instruction that's a jump
       target must be emittable standalone*/

       /* Need at least 4 opcodes to fuse*/
    if (ip + 3 >= code_len) return 0;

    uint8_t op0 = code[ip];
    int slot_a, slot_b;

    /* ---- PATTERN 1 & 2: GET_LOCAL(a) GET_LOCAL(b) <op> <consume> ----*/
    slot_a = is_get_local_at(code, ip, code_len);
    if (slot_a < 0) return 0;

    int size0 = (op0 == BTL_OP_GET_LOCAL) ? 2 : 1;
    if (op0 == BTL_OP_GET_LOCAL) slot_a = code[ip + 1];

    int ip1 = ip + size0;
    if (ip1 >= code_len || targets[ip1]) return 0;

    slot_b = is_get_local_at(code, ip1, code_len);
    if (slot_b < 0) return 0;

    uint8_t op1 = code[ip1];
    int size1 = (op1 == BTL_OP_GET_LOCAL) ? 2 : 1;
    if (op1 == BTL_OP_GET_LOCAL) slot_b = code[ip1 + 1];

    int ip2 = ip1 + size1;
    if (ip2 >= code_len || targets[ip2]) return 0;

    uint8_t op2 = code[ip2];

    /* Check if op2 is an arithmetic/comparison op*/
    const char* c_op = NULL;
    bool is_comparison = false;
    switch (op2) {
    case BTL_OP_ADD:      c_op = "+"; break;
    case BTL_OP_SUBTRACT: c_op = "-"; break;
    case BTL_OP_MULTIPLY: c_op = "*"; break;
    case BTL_OP_DIVIDE:   c_op = "/"; break;
    case BTL_OP_MODULO:   c_op = "%"; break;
    case BTL_OP_LESS:     c_op = "<";  is_comparison = true; break;
    case BTL_OP_GREATER:  c_op = ">";  is_comparison = true; break;
    case BTL_OP_EQUAL:    c_op = NULL; is_comparison = true; break;
    default: return 0;
    }

    int ip3 = ip2 + 1;
    if (ip3 >= code_len || targets[ip3]) return 0;

    uint8_t op3 = code[ip3];

    /* PATTERN 1: arith + SET_LOCAL_POP*/
    bool is_mod = (op2 == BTL_OP_MODULO);
    bool is_div_or_mod = (op2 == BTL_OP_DIVIDE || op2 == BTL_OP_MODULO);
    if (!is_comparison) {
        int slot_c = is_set_local_pop_at(code, ip3, code_len);
        if (slot_c >= 0) {
            AbstractType ta = (slot_a < MAX_TRACKED_LOCALS) ? ts->locals[slot_a].type : TYPE_UNKNOWN;
            AbstractType tb = (slot_b < MAX_TRACKED_LOCALS) ? ts->locals[slot_b].type : TYPE_UNKNOWN;
            emit_comment(t, ip, "FUSED: arith assign");
            if (ta == TYPE_INT && tb == TYPE_INT) {
                if (is_div_or_mod)
                    OUT(t, "    { int64_t _b = AS_INT(slots[%d]); if (__builtin_expect(_b == 0, 0)) return btl_error_div_zero(vm, sp);\n      slots[%d] = INT_VAL(AS_INT(slots[%d]) %s _b); }\n", slot_b, slot_c, slot_a, c_op);
                else
                    OUT(t, "    slots[%d] = INT_VAL(AS_INT(slots[%d]) %s AS_INT(slots[%d]));\n", slot_c, slot_a, c_op, slot_b);
            } else if (ta == TYPE_NUMBER && tb == TYPE_NUMBER && !is_mod) {
                OUT(t, "    slots[%d] = NUMBER_VAL(AS_NUMBER(slots[%d]) %s AS_NUMBER(slots[%d]));\n", slot_c, slot_a, c_op, slot_b);
            } else if (ta == TYPE_NUMBER && tb == TYPE_NUMBER && is_mod) {
                OUT(t, "    slots[%d] = NUMBER_VAL(fmod(AS_NUMBER(slots[%d]), AS_NUMBER(slots[%d])));\n", slot_c, slot_a, slot_b);
            } else if (!is_mod) {
                OUT(t, "    { BtlValue _fa = slots[%d], _fb = slots[%d];\n", slot_a, slot_b);
                if (op2 == BTL_OP_DIVIDE) {
                    OUT(t, "      if (__builtin_expect(IS_INT(_fa) & IS_INT(_fb), 1)) {\n");
                    OUT(t, "        int64_t _bi = AS_INT(_fb); if (__builtin_expect(_bi == 0, 0)) return btl_error_div_zero(vm, sp);\n");
                    OUT(t, "        slots[%d] = INT_VAL(AS_INT(_fa) / _bi);\n", slot_c);
                    OUT(t, "      } else\n");
                } else {
                    OUT(t, "      if (__builtin_expect(IS_INT(_fa) & IS_INT(_fb), 1))\n");
                    OUT(t, "        slots[%d] = INT_VAL(AS_INT(_fa) %s AS_INT(_fb));\n", slot_c, c_op);
                    OUT(t, "      else\n");
                }
                OUT(t, "        slots[%d] = NUMBER_VAL(btl_numeric_to_double(_fa) %s btl_numeric_to_double(_fb));\n", slot_c, c_op);
                OUT(t, "    }\n");
            } else {
                /* modulo with unknown types */
                OUT(t, "    { BtlValue _fa = slots[%d], _fb = slots[%d];\n", slot_a, slot_b);
                OUT(t, "      if (__builtin_expect(IS_INT(_fa) & IS_INT(_fb), 1)) {\n");
                OUT(t, "        int64_t _bi = AS_INT(_fb); if (__builtin_expect(_bi == 0, 0)) return btl_error_div_zero(vm, sp);\n");
                OUT(t, "        slots[%d] = INT_VAL(AS_INT(_fa) %s _bi);\n", slot_c, c_op);
                OUT(t, "      } else\n");
                OUT(t, "        slots[%d] = NUMBER_VAL(fmod(btl_numeric_to_double(_fa), btl_numeric_to_double(_fb)));\n", slot_c);
                OUT(t, "    }\n");
            }
            /* Update TypeState: local[c] gets the result type */
            if (slot_c < MAX_TRACKED_LOCALS) {
                if (ta == TYPE_INT && tb == TYPE_INT)
                    ts->locals[slot_c] = tracked_type(TYPE_INT);
                else if ((ta == TYPE_NUMBER || ta == TYPE_INT) && (tb == TYPE_NUMBER || tb == TYPE_INT))
                    ts->locals[slot_c] = tracked_type(TYPE_NUMBER);
                else
                    ts->locals[slot_c] = tracked_unknown();
            }
            return (ip3 + 1) - ip; /* total bytes consumed*/
        }
    }

    /* PATTERN 2: comparison + POP_JUMP_IF_FALSE*/
    if (is_comparison && op3 == BTL_OP_POP_JUMP_IF_FALSE && ip3 + 3 <= code_len) {
        uint16_t offset = (uint16_t) ((code[ip3 + 1] << 8) | code[ip3 + 2]);
        int target_ip = ip3 + 3 + offset;
        AbstractType ta = (slot_a < MAX_TRACKED_LOCALS) ? ts->locals[slot_a].type : TYPE_UNKNOWN;
        AbstractType tb = (slot_b < MAX_TRACKED_LOCALS) ? ts->locals[slot_b].type : TYPE_UNKNOWN;
        emit_comment(t, ip, "FUSED: compare+branch");
        if (op2 == BTL_OP_EQUAL) {
            if (ta == TYPE_INT && tb == TYPE_INT)
                OUT(t, "    if (AS_INT(slots[%d]) != AS_INT(slots[%d])) goto L_%04d;\n", slot_a, slot_b, target_ip);
            else if (ta == TYPE_NIL)
                OUT(t, "    if (!btl_compiled_is_null_like(slots[%d])) goto L_%04d; /* nil==? */\n", slot_b, target_ip);
            else if (tb == TYPE_NIL)
                OUT(t, "    if (!btl_compiled_is_null_like(slots[%d])) goto L_%04d; /* ?==nil */\n", slot_a, target_ip);
            else if (ta == TYPE_STRING && tb == TYPE_STRING)
                OUT(t, "    if (slots[%d] != slots[%d]) goto L_%04d; /* string==string */\n", slot_a, slot_b, target_ip);
            else
                OUT(t, "    if (!btl_compiled_equal(slots[%d], slots[%d])) goto L_%04d;\n", slot_a, slot_b, target_ip);
        } else {
            if (ta == TYPE_INT && tb == TYPE_INT)
                OUT(t, "    if (!(AS_INT(slots[%d]) %s AS_INT(slots[%d]))) goto L_%04d;\n", slot_a, c_op, slot_b, target_ip);
            else if (ta == TYPE_NUMBER && tb == TYPE_NUMBER)
                OUT(t, "    if (!(AS_NUMBER(slots[%d]) %s AS_NUMBER(slots[%d]))) goto L_%04d;\n", slot_a, c_op, slot_b, target_ip);
            else if (ta == TYPE_INT) {
                /* Left known INT, right unknown — one-sided check */
                OUT(t, "    { int64_t _a = AS_INT(slots[%d]);\n", slot_a);
                OUT(t, "      if (__builtin_expect(IS_INT(slots[%d]), 1)) {\n", slot_b);
                OUT(t, "        if (!(_a %s AS_INT(slots[%d]))) goto L_%04d;\n", c_op, slot_b, target_ip);
                OUT(t, "      } else if (__builtin_expect(IS_NUMBER(slots[%d]), 1)) {\n", slot_b);
                OUT(t, "        if (!((double)_a %s AS_NUMBER(slots[%d]))) goto L_%04d;\n", c_op, slot_b, target_ip);
                OUT(t, "      } else return btl_error_not_numbers(vm, sp); }\n");
            } else if (tb == TYPE_INT) {
                /* Right known INT, left unknown — one-sided check */
                OUT(t, "    { int64_t _b = AS_INT(slots[%d]);\n", slot_b);
                OUT(t, "      if (__builtin_expect(IS_INT(slots[%d]), 1)) {\n", slot_a);
                OUT(t, "        if (!(AS_INT(slots[%d]) %s _b)) goto L_%04d;\n", slot_a, c_op, target_ip);
                OUT(t, "      } else if (__builtin_expect(IS_NUMBER(slots[%d]), 1)) {\n", slot_a);
                OUT(t, "        if (!(AS_NUMBER(slots[%d]) %s (double)_b)) goto L_%04d;\n", slot_a, c_op, target_ip);
                OUT(t, "      } else return btl_error_not_numbers(vm, sp); }\n");
            } else {
                OUT(t, "    { if (__builtin_expect(IS_NUMERIC(slots[%d]) & IS_NUMERIC(slots[%d]), 1)) {\n", slot_a, slot_b);
                OUT(t, "        double _fa = btl_numeric_to_double(slots[%d]), _fb = btl_numeric_to_double(slots[%d]);\n", slot_a, slot_b);
                OUT(t, "        if (!(_fa %s _fb)) goto L_%04d;\n", c_op, target_ip);
                OUT(t, "      } else return btl_error_not_numbers(vm, sp); }\n");
            }
        }
        return (ip3 + 3) - ip;
    }

    /* PATTERN 2b: comparison + POP_JUMP_IF_TRUE*/
    if (is_comparison && op3 == BTL_OP_POP_JUMP_IF_TRUE && ip3 + 3 <= code_len) {
        uint16_t offset = (uint16_t) ((code[ip3 + 1] << 8) | code[ip3 + 2]);
        int target_ip = ip3 + 3 + offset;
        AbstractType ta = (slot_a < MAX_TRACKED_LOCALS) ? ts->locals[slot_a].type : TYPE_UNKNOWN;
        AbstractType tb = (slot_b < MAX_TRACKED_LOCALS) ? ts->locals[slot_b].type : TYPE_UNKNOWN;
        emit_comment(t, ip, "FUSED: compare+branch_true");
        if (op2 == BTL_OP_EQUAL) {
            if (ta == TYPE_INT && tb == TYPE_INT)
                OUT(t, "    if (AS_INT(slots[%d]) == AS_INT(slots[%d])) goto L_%04d;\n", slot_a, slot_b, target_ip);
            else if (ta == TYPE_NIL)
                OUT(t, "    if (btl_compiled_is_null_like(slots[%d])) goto L_%04d; /* nil==? */\n", slot_b, target_ip);
            else if (tb == TYPE_NIL)
                OUT(t, "    if (btl_compiled_is_null_like(slots[%d])) goto L_%04d; /* ?==nil */\n", slot_a, target_ip);
            else if (ta == TYPE_STRING && tb == TYPE_STRING)
                OUT(t, "    if (slots[%d] == slots[%d]) goto L_%04d; /* string==string */\n", slot_a, slot_b, target_ip);
            else
                OUT(t, "    if (btl_compiled_equal(slots[%d], slots[%d])) goto L_%04d;\n", slot_a, slot_b, target_ip);
        } else {
            if (ta == TYPE_INT && tb == TYPE_INT)
                OUT(t, "    if (AS_INT(slots[%d]) %s AS_INT(slots[%d])) goto L_%04d;\n", slot_a, c_op, slot_b, target_ip);
            else if (ta == TYPE_NUMBER && tb == TYPE_NUMBER)
                OUT(t, "    if (AS_NUMBER(slots[%d]) %s AS_NUMBER(slots[%d])) goto L_%04d;\n", slot_a, c_op, slot_b, target_ip);
            else if (ta == TYPE_INT) {
                /* Left known INT, right unknown — one-sided check */
                OUT(t, "    { int64_t _a = AS_INT(slots[%d]);\n", slot_a);
                OUT(t, "      if (__builtin_expect(IS_INT(slots[%d]), 1)) {\n", slot_b);
                OUT(t, "        if (_a %s AS_INT(slots[%d])) goto L_%04d;\n", c_op, slot_b, target_ip);
                OUT(t, "      } else if (__builtin_expect(IS_NUMBER(slots[%d]), 1)) {\n", slot_b);
                OUT(t, "        if ((double)_a %s AS_NUMBER(slots[%d])) goto L_%04d;\n", c_op, slot_b, target_ip);
                OUT(t, "      } else return btl_error_not_numbers(vm, sp); }\n");
            } else if (tb == TYPE_INT) {
                /* Right known INT, left unknown — one-sided check */
                OUT(t, "    { int64_t _b = AS_INT(slots[%d]);\n", slot_b);
                OUT(t, "      if (__builtin_expect(IS_INT(slots[%d]), 1)) {\n", slot_a);
                OUT(t, "        if (AS_INT(slots[%d]) %s _b) goto L_%04d;\n", slot_a, c_op, target_ip);
                OUT(t, "      } else if (__builtin_expect(IS_NUMBER(slots[%d]), 1)) {\n", slot_a);
                OUT(t, "        if (AS_NUMBER(slots[%d]) %s (double)_b) goto L_%04d;\n", slot_a, c_op, target_ip);
                OUT(t, "      } else return btl_error_not_numbers(vm, sp); }\n");
            } else {
                OUT(t, "    { if (__builtin_expect(IS_NUMERIC(slots[%d]) & IS_NUMERIC(slots[%d]), 1)) {\n", slot_a, slot_b);
                OUT(t, "        double _fa = btl_numeric_to_double(slots[%d]), _fb = btl_numeric_to_double(slots[%d]);\n", slot_a, slot_b);
                OUT(t, "        if (_fa %s _fb) goto L_%04d;\n", c_op, target_ip);
                OUT(t, "      } else return btl_error_not_numbers(vm, sp); }\n");
            }
        }
        return (ip3 + 3) - ip;
    }

    return 0;
}

// ================================================================
// PATTERN 3: GET_LOCAL(a) <const_push> <arith> SET_LOCAL_N_POP(c)
//   → slots[c] = NUMBER_VAL(AS_NUMBER(slots[a]) OP <const>)
//   Common in loops like `i = i + 1`, `x = x// 2`.
//
// PATTERN 4: GET_LOCAL(a) <const_push> <compare> POP_JUMP_IF_FALSE
//   → if (!(AS_NUMBER(slots[a]) < <const>)) goto L;
//   Common in `while (i < 10)` style loops.
// ================================================================

static int try_fuse_local_const(BtlTranspiler* t, ObjFunction* fn, uint8_t* code,
    int ip, int code_len, bool* targets, TypeState* ts) {
    if (ip + 3 >= code_len) return 0;

    uint8_t op0 = code[ip];
    int slot_a = is_get_local_at(code, ip, code_len);
    if (slot_a < 0) return 0;

    int size0 = (op0 == BTL_OP_GET_LOCAL) ? 2 : 1;
    if (op0 == BTL_OP_GET_LOCAL) slot_a = code[ip + 1];

    int ip1 = ip + size0;
    if (ip1 >= code_len || targets[ip1]) return 0;

    /* Try to match a constant push at ip1*/
    uint8_t cop = code[ip1];
    char const_expr[64];
    int const_size;
    bool const_is_int = false;
    bool const_is_zero = false;

    if (cop == BTL_OP_INT_0) {
        snprintf(const_expr, sizeof(const_expr), "0");
        const_size = 1; const_is_int = true; const_is_zero = true;
    } else if (cop == BTL_OP_INT_1) {
        snprintf(const_expr, sizeof(const_expr), "1");
        const_size = 1; const_is_int = true;
    } else if (cop == BTL_OP_INT_2) {
        snprintf(const_expr, sizeof(const_expr), "2");
        const_size = 1; const_is_int = true;
    } else if (cop == BTL_OP_0) {
        snprintf(const_expr, sizeof(const_expr), "0.0");
        const_size = 1; const_is_zero = true;
    } else if (cop == BTL_OP_1) {
        snprintf(const_expr, sizeof(const_expr), "1.0");
        const_size = 1;
    } else if (cop == BTL_OP_2) {
        snprintf(const_expr, sizeof(const_expr), "2.0");
        const_size = 1;
    } else if (cop == BTL_OP_CONSTANT && ip1 + 1 < code_len) {
        uint8_t cidx = code[ip1 + 1];
        BtlValue cval = fn->chunk.constants.values[cidx];
        if (IS_INT(cval)) {
            snprintf(const_expr, sizeof(const_expr), "%" PRId64, AS_INT(cval));
            const_is_int = true;
            if (AS_INT(cval) == 0) const_is_zero = true;
        } else if (IS_NUMBER(cval)) {
            snprintf(const_expr, sizeof(const_expr), "%.17g", AS_NUMBER(cval));
            if (AS_NUMBER(cval) == 0.0) const_is_zero = true;
        } else {
            return 0;
        }
        const_size = 2;
    } else if (cop == BTL_OP_CONSTANT_LONG && ip1 + 2 < code_len) {
        uint16_t cidx = (uint16_t)((code[ip1 + 1] << 8) | code[ip1 + 2]);
        BtlValue cval = fn->chunk.constants.values[cidx];
        if (IS_INT(cval)) {
            snprintf(const_expr, sizeof(const_expr), "%" PRId64, AS_INT(cval));
            const_is_int = true;
            if (AS_INT(cval) == 0) const_is_zero = true;
        } else if (IS_NUMBER(cval)) {
            snprintf(const_expr, sizeof(const_expr), "%.17g", AS_NUMBER(cval));
            if (AS_NUMBER(cval) == 0.0) const_is_zero = true;
        } else {
            return 0;
        }
        const_size = 3;
    } else {
        return 0;
    }

    int ip2 = ip1 + const_size;
    if (ip2 >= code_len || targets[ip2]) return 0;

    uint8_t arith_op = code[ip2];
    const char* c_op = NULL;
    bool is_comparison = false;
    switch (arith_op) {
    case BTL_OP_ADD:      c_op = "+"; break;
    case BTL_OP_SUBTRACT: c_op = "-"; break;
    case BTL_OP_MULTIPLY: c_op = "*"; break;
    case BTL_OP_DIVIDE:   c_op = "/"; break;
    case BTL_OP_MODULO:   c_op = "%"; break;
    case BTL_OP_LESS:     c_op = "<";  is_comparison = true; break;
    case BTL_OP_GREATER:  c_op = ">";  is_comparison = true; break;
    case BTL_OP_EQUAL:    c_op = "=="; is_comparison = true; break;
    default: return 0;
    }

    int ip3 = ip2 + 1;
    if (ip3 >= code_len || targets[ip3]) return 0;

    /* PATTERN 3: arith + SET_LOCAL_POP*/
    bool is_modulo = (arith_op == BTL_OP_MODULO);
    bool is_div_or_mod_lc = (arith_op == BTL_OP_DIVIDE || arith_op == BTL_OP_MODULO);
    if (!is_comparison) {
        int slot_c = is_set_local_pop_at(code, ip3, code_len);
        if (slot_c >= 0) {
            /* Division/modulo by known zero constant — emit error at transpile time */
            if (is_div_or_mod_lc && const_is_zero) return 0; /* Fall through to non-fused path which has zero checks */
            AbstractType ta = (slot_a < MAX_TRACKED_LOCALS) ? ts->locals[slot_a].type : TYPE_UNKNOWN;
            emit_comment(t, ip, "FUSED: local op= const");
            if (const_is_int && ta == TYPE_INT) {
                /* Both operands known int — direct int arithmetic (divisor known non-zero) */
                OUT(t, "    slots[%d] = INT_VAL(AS_INT(slots[%d]) %s (int64_t)%s);\n", slot_c, slot_a, c_op, const_expr);
            } else if (ta == TYPE_NUMBER && !is_modulo) {
                /* Local known number — direct double arithmetic (not modulo) */
                OUT(t, "    slots[%d] = NUMBER_VAL(AS_NUMBER(slots[%d]) %s %s%s);\n",
                    slot_c, slot_a, c_op, const_expr, const_is_int ? ".0" : "");
            } else if (ta == TYPE_NUMBER && is_modulo) {
                /* Modulo on doubles requires fmod() */
                OUT(t, "    slots[%d] = NUMBER_VAL(fmod(AS_NUMBER(slots[%d]), %s%s));\n",
                    slot_c, slot_a, const_expr, const_is_int ? ".0" : "");
            } else if (const_is_int && !is_modulo) {
                OUT(t, "    { BtlValue _fa = slots[%d];\n", slot_a);
                OUT(t, "      if (__builtin_expect(IS_INT(_fa), 1))\n");
                OUT(t, "        slots[%d] = INT_VAL(AS_INT(_fa) %s (int64_t)%s);\n", slot_c, c_op, const_expr);
                OUT(t, "      else\n");
                OUT(t, "        slots[%d] = NUMBER_VAL(btl_numeric_to_double(_fa) %s %s.0);\n", slot_c, c_op, const_expr);
                OUT(t, "    }\n");
            } else if (const_is_int && is_modulo) {
                OUT(t, "    { BtlValue _fa = slots[%d];\n", slot_a);
                OUT(t, "      if (__builtin_expect(IS_INT(_fa), 1))\n");
                OUT(t, "        slots[%d] = INT_VAL(AS_INT(_fa) %s (int64_t)%s);\n", slot_c, c_op, const_expr);
                OUT(t, "      else\n");
                OUT(t, "        slots[%d] = NUMBER_VAL(fmod(btl_numeric_to_double(_fa), %s.0));\n", slot_c, const_expr);
                OUT(t, "    }\n");
            } else if (!is_modulo) {
                OUT(t, "    slots[%d] = NUMBER_VAL(btl_numeric_to_double(slots[%d]) %s %s);\n",
                    slot_c, slot_a, c_op, const_expr);
            } else {
                OUT(t, "    slots[%d] = NUMBER_VAL(fmod(btl_numeric_to_double(slots[%d]), %s));\n",
                    slot_c, slot_a, const_expr);
            }
            /* Update TypeState: local[c] gets the result type */
            if (slot_c < MAX_TRACKED_LOCALS) {
                if (const_is_int && ta == TYPE_INT)
                    ts->locals[slot_c] = tracked_type(TYPE_INT);
                else if (ta == TYPE_NUMBER || ta == TYPE_INT)
                    ts->locals[slot_c] = tracked_type(TYPE_NUMBER);
                else
                    ts->locals[slot_c] = tracked_unknown();
            }
            return (ip3 + 1) - ip;
        }
    }

    /* PATTERN 4: comparison + POP_JUMP_IF_FALSE*/
    uint8_t op3 = code[ip3];
    if (is_comparison && op3 == BTL_OP_POP_JUMP_IF_FALSE && ip3 + 3 <= code_len) {
        uint16_t offset = (uint16_t) ((code[ip3 + 1] << 8) | code[ip3 + 2]);
        int target_ip = ip3 + 3 + offset;
        AbstractType ta4 = (slot_a < MAX_TRACKED_LOCALS) ? ts->locals[slot_a].type : TYPE_UNKNOWN;
        emit_comment(t, ip, "FUSED: local cmp const + branch");
        if (ta4 == TYPE_INT && const_is_int)
            OUT(t, "    if (!(AS_INT(slots[%d]) %s (int64_t)%s)) goto L_%04d;\n",
                slot_a, c_op, const_expr, target_ip);
        else if (ta4 == TYPE_NUMBER)
            OUT(t, "    if (!(AS_NUMBER(slots[%d]) %s %s%s)) goto L_%04d;\n",
                slot_a, c_op, const_expr, const_is_int ? ".0" : "", target_ip);
        else if (const_is_int) {
            /* Const is INT, local unknown — IS_INT fast check */
            OUT(t, "    { if (__builtin_expect(IS_INT(slots[%d]), 1)) {\n", slot_a);
            OUT(t, "        if (!(AS_INT(slots[%d]) %s (int64_t)%s)) goto L_%04d;\n", slot_a, c_op, const_expr, target_ip);
            OUT(t, "      } else if (__builtin_expect(IS_NUMBER(slots[%d]), 1)) {\n", slot_a);
            OUT(t, "        if (!(AS_NUMBER(slots[%d]) %s %s.0)) goto L_%04d;\n", slot_a, c_op, const_expr, target_ip);
            OUT(t, "      } else return btl_error_not_numbers(vm, sp); }\n");
        } else {
            OUT(t, "    { if (__builtin_expect(IS_NUMERIC(slots[%d]), 1)) {\n", slot_a);
            OUT(t, "        if (!(btl_numeric_to_double(slots[%d]) %s %s)) goto L_%04d;\n",
                slot_a, c_op, const_expr, target_ip);
            OUT(t, "      } else return btl_error_not_numbers(vm, sp); }\n");
        }
        return (ip3 + 3) - ip;
    }

    /* PATTERN 4b: comparison + POP_JUMP_IF_TRUE*/
    if (is_comparison && op3 == BTL_OP_POP_JUMP_IF_TRUE && ip3 + 3 <= code_len) {
        uint16_t offset = (uint16_t) ((code[ip3 + 1] << 8) | code[ip3 + 2]);
        int target_ip = ip3 + 3 + offset;
        AbstractType ta4b = (slot_a < MAX_TRACKED_LOCALS) ? ts->locals[slot_a].type : TYPE_UNKNOWN;
        emit_comment(t, ip, "FUSED: local cmp const + branch_true");
        if (ta4b == TYPE_INT && const_is_int)
            OUT(t, "    if (AS_INT(slots[%d]) %s (int64_t)%s) goto L_%04d;\n",
                slot_a, c_op, const_expr, target_ip);
        else if (ta4b == TYPE_NUMBER)
            OUT(t, "    if (AS_NUMBER(slots[%d]) %s %s%s) goto L_%04d;\n",
                slot_a, c_op, const_expr, const_is_int ? ".0" : "", target_ip);
        else if (const_is_int) {
            /* Const is INT, local unknown — IS_INT fast check */
            OUT(t, "    { if (__builtin_expect(IS_INT(slots[%d]), 1)) {\n", slot_a);
            OUT(t, "        if (AS_INT(slots[%d]) %s (int64_t)%s) goto L_%04d;\n", slot_a, c_op, const_expr, target_ip);
            OUT(t, "      } else if (__builtin_expect(IS_NUMBER(slots[%d]), 1)) {\n", slot_a);
            OUT(t, "        if (AS_NUMBER(slots[%d]) %s %s.0) goto L_%04d;\n", slot_a, c_op, const_expr, target_ip);
            OUT(t, "      } else return btl_error_not_numbers(vm, sp); }\n");
        } else {
            OUT(t, "    { if (__builtin_expect(IS_NUMERIC(slots[%d]), 1)) {\n", slot_a);
            OUT(t, "        if (btl_numeric_to_double(slots[%d]) %s %s) goto L_%04d;\n",
                slot_a, c_op, const_expr, target_ip);
            OUT(t, "      } else return btl_error_not_numbers(vm, sp); }\n");
        }
        return (ip3 + 3) - ip;
    }

    return 0;
}

// ================================================================
// Simple 2-opcode fusion patterns
//
// PATTERN 5: <value_push> SET_LOCAL_N_POP(slot)
//   -> slots[slot] = <value>;
//   Eliminates push followed by immediate pop.
//
// PATTERN 6: GET_LOCAL_N(slot) OP_RETURN
//   -> { BtlValue _result = slots[slot]; ... return; }
//   Eliminates push to sp and immediate pop for return.
//
// PATTERN 7: GET_LOCAL_N(slot) POP_JUMP_IF_FALSE(offset)
//   -> if (btl_compiled_is_falsey(slots[slot])) goto L;
//   Eliminates push+pop for conditional branch on local.
// ================================================================

static int try_fuse_simple(BtlTranspiler* t, ObjFunction* fn, uint8_t* code,
    int ip, int code_len, bool* targets, TypeState* ts) {
    if (ip + 1 >= code_len) return 0;

    uint8_t op0 = code[ip];
    int ip1;

    /* ---- PATTERN 7: GET_LOCAL_N + POP_JUMP_IF_FALSE/TRUE ----*/
    int slot = is_get_local_at(code, ip, code_len);
    if (slot >= 0) {
        int size0 = (op0 == BTL_OP_GET_LOCAL) ? 2 : 1;
        if (op0 == BTL_OP_GET_LOCAL) slot = code[ip + 1];
        ip1 = ip + size0;
        if (ip1 < code_len && !targets[ip1]) {
            uint8_t op1 = code[ip1];

            /* PATTERN 7a: GET_LOCAL + POP_JUMP_IF_FALSE*/
            if (op1 == BTL_OP_POP_JUMP_IF_FALSE && ip1 + 3 <= code_len) {
                uint16_t offset = (uint16_t)((code[ip1 + 1] << 8) | code[ip1 + 2]);
                int target_ip = ip1 + 3 + offset;
                AbstractType st = (slot < MAX_TRACKED_LOCALS) ? ts->locals[slot].type : TYPE_UNKNOWN;
                emit_comment(t, ip, "FUSED: local branch");
                if (st == TYPE_BOOL)
                    OUT(t, "    if (!AS_BOOL(slots[%d])) goto L_%04d;\n", slot, target_ip);
                else if (st == TYPE_INT)
                    OUT(t, "    if (AS_INT(slots[%d]) == 0) goto L_%04d;\n", slot, target_ip);
                else if (st == TYPE_NUMBER)
                    OUT(t, "    if (AS_NUMBER(slots[%d]) == 0.0) goto L_%04d;\n", slot, target_ip);
                else
                    OUT(t, "    if (btl_compiled_is_falsey(slots[%d])) goto L_%04d;\n", slot, target_ip);
                return (ip1 + 3) - ip;
            }

            /* PATTERN 7b: GET_LOCAL + POP_JUMP_IF_TRUE*/
            if (op1 == BTL_OP_POP_JUMP_IF_TRUE && ip1 + 3 <= code_len) {
                uint16_t offset = (uint16_t)((code[ip1 + 1] << 8) | code[ip1 + 2]);
                int target_ip = ip1 + 3 + offset;
                AbstractType st = (slot < MAX_TRACKED_LOCALS) ? ts->locals[slot].type : TYPE_UNKNOWN;
                emit_comment(t, ip, "FUSED: local branch_true");
                if (st == TYPE_BOOL)
                    OUT(t, "    if (AS_BOOL(slots[%d])) goto L_%04d;\n", slot, target_ip);
                else if (st == TYPE_INT)
                    OUT(t, "    if (AS_INT(slots[%d]) != 0) goto L_%04d;\n", slot, target_ip);
                else if (st == TYPE_NUMBER)
                    OUT(t, "    if (AS_NUMBER(slots[%d]) != 0.0) goto L_%04d;\n", slot, target_ip);
                else
                    OUT(t, "    if (!btl_compiled_is_falsey(slots[%d])) goto L_%04d;\n", slot, target_ip);
                return (ip1 + 3) - ip;
            }

            /* PATTERN 6: GET_LOCAL + RETURN*/
            if (op1 == BTL_OP_RETURN) {
                emit_comment(t, ip, "FUSED: local return");
                OUT(t, "    {\n");
                OUT(t, "        BtlValue _result = slots[%d];\n", slot);
                OUT(t, "        vm->stackTop = sp;\n");
                /* Only call close_upvalues if this function needs it*/
                if (function_needs_close_upvalues(fn)) {
                    OUT(t, "        btl_compiled_close_upvalues(vm, frame);\n");
                }
                OUT(t, "        vm->frameCount--;\n");
                OUT(t, "        if (vm->frameCount == 0) { vm->lastReturnValue = _result; vm->stackTop--; return BTL_INTERPRET_OK; }\n");
                OUT(t, "        vm->stackTop = frame->slots;\n");
                OUT(t, "        *vm->stackTop++ = _result; /* inline push*/\n");
                OUT(t, "        return BTL_INTERPRET_OK;\n");
                OUT(t, "    }\n");
                return (ip1 + 1) - ip;
            }

            /* PATTERN 8: GET_LOCAL + CONST + JUMP_IF_NOT_LESS/GREATER
            // Fuses patterns like: slots[1] < 2 ? jump : continue
            // Avoids pushing/popping from stack entirely.*/
            if ((op1 == BTL_OP_0 || op1 == BTL_OP_1 || op1 == BTL_OP_2 ||
                 op1 == BTL_OP_INT_0 || op1 == BTL_OP_INT_1 || op1 == BTL_OP_INT_2 ||
                 op1 == BTL_OP_CONSTANT)) {
                int const_size = (op1 == BTL_OP_CONSTANT) ? 2 : 1;
                int ip2 = ip1 + const_size;
                if (ip2 + 2 < code_len && !targets[ip2]) {
                    uint8_t cmp_op = code[ip2];
                    if (cmp_op == BTL_OP_JUMP_IF_NOT_LESS || cmp_op == BTL_OP_JUMP_IF_NOT_GREATER) {
                        uint16_t offset = (uint16_t)((code[ip2 + 1] << 8) | code[ip2 + 2]);
                        int target_ip = ip2 + 3 + offset;

                        /* Get the constant value and track if it's an int */
                        char const_val[64];
                        char const_val_int[64];
                        bool p8_const_is_int = false;
                        if (op1 == BTL_OP_INT_0) { snprintf(const_val, sizeof(const_val), "0.0"); snprintf(const_val_int, sizeof(const_val_int), "0"); p8_const_is_int = true; }
                        else if (op1 == BTL_OP_INT_1) { snprintf(const_val, sizeof(const_val), "1.0"); snprintf(const_val_int, sizeof(const_val_int), "1"); p8_const_is_int = true; }
                        else if (op1 == BTL_OP_INT_2) { snprintf(const_val, sizeof(const_val), "2.0"); snprintf(const_val_int, sizeof(const_val_int), "2"); p8_const_is_int = true; }
                        else if (op1 == BTL_OP_0) snprintf(const_val, sizeof(const_val), "0.0");
                        else if (op1 == BTL_OP_1) snprintf(const_val, sizeof(const_val), "1.0");
                        else if (op1 == BTL_OP_2) snprintf(const_val, sizeof(const_val), "2.0");
                        else {
                            uint8_t cidx = code[ip1 + 1];
                            BtlValue cv = fn->chunk.constants.values[cidx];
                            if (IS_INT(cv)) {
                                snprintf(const_val, sizeof(const_val), "%" PRId64 ".0", AS_INT(cv));
                                snprintf(const_val_int, sizeof(const_val_int), "(int64_t)%" PRId64, AS_INT(cv));
                                p8_const_is_int = true;
                            }
                            else if (IS_NUMBER(cv))
                                snprintf(const_val, sizeof(const_val), "%.17g", AS_NUMBER(cv));
                            else goto skip_pattern8;
                        }

                        const char* cmp_str = (cmp_op == BTL_OP_JUMP_IF_NOT_LESS) ? "<" : ">";
                        AbstractType st8 = (slot < MAX_TRACKED_LOCALS) ? ts->locals[slot].type : TYPE_UNKNOWN;
                        emit_comment(t, ip, "FUSED: local cmp const jump");
                        if (st8 == TYPE_INT && p8_const_is_int)
                            OUT(t, "    if (!(AS_INT(slots[%d]) %s %s)) goto L_%04d;\n",
                                slot, cmp_str, const_val_int, target_ip);
                        else if (st8 == TYPE_NUMBER)
                            OUT(t, "    if (!(AS_NUMBER(slots[%d]) %s %s)) goto L_%04d;\n",
                                slot, cmp_str, const_val, target_ip);
                        else if (p8_const_is_int) {
                            OUT(t, "    { BtlValue _v = slots[%d];\n", slot);
                            OUT(t, "      if (__builtin_expect(IS_INT(_v), 1)) { if (!(AS_INT(_v) %s %s)) goto L_%04d; }\n", cmp_str, const_val_int, target_ip);
                            OUT(t, "      else if (__builtin_expect(IS_NUMBER(_v), 1)) { if (!(AS_NUMBER(_v) %s %s)) goto L_%04d; }\n", cmp_str, const_val, target_ip);
                            OUT(t, "      else return btl_error_not_numbers(vm, sp); }\n");
                        } else {
                            OUT(t, "    { if (__builtin_expect(IS_NUMERIC(slots[%d]), 1)) {\n", slot);
                            OUT(t, "        if (!(btl_numeric_to_double(slots[%d]) %s %s)) goto L_%04d;\n",
                                slot, cmp_str, const_val, target_ip);
                            OUT(t, "      } else return btl_error_not_numbers(vm, sp); }\n");
                        }
                        return (ip2 + 3) - ip;
                    }
                }
            }
            skip_pattern8:;
        }
    }

    /* ---- PATTERN 5: <value_push> + SET_LOCAL_N_POP ----*/
    /* Check for a value-producing opcode followed by SET_LOCAL_N_POP*/
    int push_size = 0;
    char value_expr[128];
    AbstractType val_type = TYPE_UNKNOWN;

    if (op0 == BTL_OP_NULL) {
        snprintf(value_expr, sizeof(value_expr), "BTL_NULL_VAL");
        push_size = 1; val_type = TYPE_NIL;
    } else if (op0 == BTL_OP_TRUE) {
        snprintf(value_expr, sizeof(value_expr), "BTL_TRUE_VAL");
        push_size = 1; val_type = TYPE_BOOL;
    } else if (op0 == BTL_OP_FALSE) {
        snprintf(value_expr, sizeof(value_expr), "BTL_FALSE_VAL");
        push_size = 1; val_type = TYPE_BOOL;
    } else if (op0 == BTL_OP_INT_0) {
        snprintf(value_expr, sizeof(value_expr), "INT_VAL(0)");
        push_size = 1; val_type = TYPE_INT;
    } else if (op0 == BTL_OP_INT_1) {
        snprintf(value_expr, sizeof(value_expr), "INT_VAL(1)");
        push_size = 1; val_type = TYPE_INT;
    } else if (op0 == BTL_OP_INT_2) {
        snprintf(value_expr, sizeof(value_expr), "INT_VAL(2)");
        push_size = 1; val_type = TYPE_INT;
    } else if (op0 == BTL_OP_0) {
        snprintf(value_expr, sizeof(value_expr), "NUMBER_VAL(0.0)");
        push_size = 1; val_type = TYPE_NUMBER;
    } else if (op0 == BTL_OP_1) {
        snprintf(value_expr, sizeof(value_expr), "NUMBER_VAL(1.0)");
        push_size = 1; val_type = TYPE_NUMBER;
    } else if (op0 == BTL_OP_2) {
        snprintf(value_expr, sizeof(value_expr), "NUMBER_VAL(2.0)");
        push_size = 1; val_type = TYPE_NUMBER;
    } else if (op0 == BTL_OP_CONSTANT && ip + 1 < code_len) {
        uint8_t cidx = code[ip + 1];
        BtlValue cv = fn->chunk.constants.values[cidx];
        if (IS_INT(cv)) {
            snprintf(value_expr, sizeof(value_expr), "INT_VAL(%" PRId64 ")", AS_INT(cv));
            val_type = TYPE_INT;
        } else if (IS_NUMBER(cv)) {
            snprintf(value_expr, sizeof(value_expr), "NUMBER_VAL(%.17g)", AS_NUMBER(cv));
            val_type = TYPE_NUMBER;
        } else {
            snprintf(value_expr, sizeof(value_expr), "fn->chunk.constants.values[%d]", cidx);
        }
        push_size = 2;
    }

    if (push_size > 0) {
        ip1 = ip + push_size;
        if (ip1 < code_len && !targets[ip1]) {
            int dest_slot = is_set_local_pop_at(code, ip1, code_len);
            if (dest_slot >= 0) {
                emit_comment(t, ip, "FUSED: const assign");
                OUT(t, "    slots[%d] = %s;\n", dest_slot, value_expr);
                /* Update TypeState: local[dest_slot] gets the assigned type */
                if (dest_slot < MAX_TRACKED_LOCALS) {
                    ts->locals[dest_slot] = tracked_type(val_type);
                }
                return (ip1 + 1) - ip;
            }
        }
    }

    return 0;
}

// ================================================================
// Emit a single function body
// ================================================================

static void emit_function(BtlTranspiler* t, ObjFunction* fn, int fn_id) {
    BtlChunk* chunk = &fn->chunk;
    uint8_t* code = chunk->code;
    int code_len = chunk->count;

    t->current_fn = fn;
    t->current_fn_id = fn_id;

    /* First pass: collect jump targets and loop info*/
    bool* targets = calloc(code_len + 1, sizeof(bool));
    LoopTable loops;
    collect_jump_targets(fn, targets, code_len, &loops);

    /* Second pass: analyze each loop for LICM candidates*/
    HoistInfo* loop_hoists = NULL;
    LoopTypeInfo* loop_types = NULL;
    if (loops.count > 0) {
        loop_hoists = calloc(loops.count, sizeof(HoistInfo));
        loop_types = calloc(loops.count, sizeof(LoopTypeInfo));
        for (int i = 0; i < loops.count; i++) {
            analyze_loop_for_licm(t, fn, loops.loops[i].header, loops.loops[i].end, &loop_hoists[i]);
            analyze_loop_types(fn, loops.loops[i].header, loops.loops[i].end, &loop_types[i]);

            /* Pre-loop type verification is now done inside analyze_loop_types
               via analyze_pre_loop_types (batch version). The analysis seeds initial
               slot types from pre-loop context before scanning the loop body. */
        }
    }

    /* Function signature*/
    const char* name = fn->name ? fn->name->chars : "<script>";
    OUT(t, "/* BTL function: %s (arity=%d, upvalues=%d)*/\n", name, fn->arity, fn->upvalueCount);
    OUT(t, "static BtlInterpretResult btl_fn_%d(VM* vm) {\n", fn_id);

    /* ---- Cached state in C locals ----
    // These live in registers. We sync back to vm->stackTop only
    // at call boundaries.
    //
    // 'sp' is our local stack pointer. It shadows vm->stackTop.
    // 'slots' is frame->slots (base of current frame's stack window).
    // 'frame' is the current CallFrame pointer.
    */
    OUT(t, "    register BtlValue* sp = vm->stackTop;\n");
    OUT(t, "    BtlCallFrame* frame = &vm->frames[vm->frameCount - 1];\n");
    OUT(t, "    BtlValue* slots = frame->slots;\n");
    OUT(t, "    ObjFunction* fn = frame->closure->function;\n");
    OUT(t, "    ObjModule* mod = fn->module;\n");

    /* Emit declarations for hoisted loop-invariant globals
    // Note: We don't initialize them here because the globals might not be
    // defined yet (e.g., loop in script that runs after function definitions).
    // Instead, we'll initialize them right before each loop header.
    // But we use BTL_EMPTY_VAL as a sentinel to detect first-time initialization.
    */
    if (loop_hoists) {
        for (int li = 0; li < loops.count; li++) {
            for (int hi = 0; hi < loop_hoists[li].count; hi++) {
                int slot = loop_hoists[li].candidates[hi].slot;
                OUT(t, "    BtlValue _hoist_g%d_%d = BTL_EMPTY_VAL; /* hoisted: %s*/\n",
                    li, slot, loop_hoists[li].candidates[hi].name);
            }
        }
    }

    OUT(t, "    (void)fn; (void)mod; (void)slots;\n");
    NL(t);
    OUT(t, "#ifdef _MSC_VER\n");
    OUT(t, "  btl_entry: (void)0;\n");
    OUT(t, "#else\n");
    OUT(t, "  btl_entry: (void)&&btl_entry;\n");
    OUT(t, "#endif\n");
    NL(t);

    /* Code generation pass*/
    int ip = 0;
    int current_loop = -1;  /* Track which loop we're inside, -1 = none*/

    /* Type state tracking for specialization*/
    TypeState ts;
    type_state_init(&ts);
    /* Function arguments start at slot 1 (slot 0 is the closure itself)*/
    /* We don't know their types initially*/

    /* Dead code elimination: track if current code is reachable*/
    bool reachable = true;

    while (ip < code_len) {
        /* Check if we're entering or exiting a loop*/
        for (int li = 0; li < loops.count; li++) {
            if (ip == loops.loops[li].header) {
                current_loop = li;
            } else if (current_loop == li && ip == loops.loops[li].end) {
                current_loop = -1;
            }
        }

        /* Emit label if this is a jump target*/
        if (targets[ip]) {
            /* Note: LICM hoisting is done lazily at the use site, not here.
            // This avoids issues with unreachable code before loop headers.*/
            emit_label(t, ip);
            /* Reset type state at jump targets since we can't track across branches*/
            type_state_init(&ts);

            /* If this IP is inside a loop, seed the type state with stable local
            // types from the loop type analysis. This avoids redundant type checks
            // at ALL jump targets within a loop, not just the header. A for-loop
            // generates multiple jump targets (condition, increment, body start). */
            if (loop_types) {
                for (int li = 0; li < loops.count; li++) {
                    if (ip >= loops.loops[li].header && ip < loops.loops[li].end) {
                        for (int s = 0; s < MAX_TRACKED_LOCALS; s++) {
                            if (loop_types[li].localTypes[s] != TYPE_UNKNOWN) {
                                ts.locals[s] = tracked_type(loop_types[li].localTypes[s]);
                                if (t->config.emit_comments) {
                                    OUT(t, "    /* LOOP_TYPE: slot[%d] = %s */\n", s, type_name(loop_types[li].localTypes[s]));
                                }
                            }
                        }
                        break;
                    }
                }
            }

            /* Jump targets are reachable (someone jumps here)*/
            reachable = true;
        }

        int start_ip = ip;

        /* ---- Try fused patterns first ----*/
        if (!targets[ip]) {  /* can't fuse if this ip is a jump target*/
            /* Try 4-opcode local+local patterns (longest match first)*/
            int fused = try_fuse(t, code, ip, code_len, targets, &ts);
            if (fused > 0) {
                ip += fused;
                continue;
            }
            /* Try local+const patterns (i = i + 1, while i < 10, etc.)*/
            fused = try_fuse_local_const(t, fn, code, ip, code_len, targets, &ts);
            if (fused > 0) {
                ip += fused;
                continue;
            }
            /* Try simple 2-opcode patterns last*/
            fused = try_fuse_simple(t, fn, code, ip, code_len, targets, &ts);
            if (fused > 0) {
                ip += fused;
                continue;
            }
        }

        uint8_t op = code[ip++];

        /* Dead code elimination: skip unreachable opcodes (but still advance ip)*/
        if (!reachable) {
            /* Skip operand bytes based on opcode*/
            switch (op) {
            case BTL_OP_CONSTANT: case BTL_OP_GET_LOCAL: case BTL_OP_SET_LOCAL:
            case BTL_OP_GET_GLOBAL: case BTL_OP_DEFINE_GLOBAL: case BTL_OP_SET_GLOBAL:
            case BTL_OP_GET_UPVALUE: case BTL_OP_GET_UPVALUE_OPEN:
            case BTL_OP_GET_UPVALUE_CLOSED: case BTL_OP_GET_UPVALUE_IMMUTABLE:
            case BTL_OP_SET_UPVALUE: case BTL_OP_SET_UPVALUE_OPEN:
            case BTL_OP_SET_UPVALUE_CLOSED:
            case BTL_OP_INC_LOCAL_POP: case BTL_OP_INC_LOCAL:
            case BTL_OP_CALL: case BTL_OP_TAIL_CALL:
            case BTL_OP_POP_N: case BTL_OP_FIELD:
            case BTL_OP_GET_FIELD_THIS: case BTL_OP_SET_FIELD_THIS:
            case BTL_OP_CLASS: case BTL_OP_BUILD_LIST: case BTL_OP_BUILD_TABLE:
            case BTL_OP_IMPORT: case BTL_OP_DO_NEW: case BTL_OP_GET_SUPER:
                ip += 1; break;
            case BTL_OP_CONSTANT_LONG:
            case BTL_OP_GET_GLOBAL_LONG: case BTL_OP_DEFINE_GLOBAL_LONG:
            case BTL_OP_SET_GLOBAL_LONG: case BTL_OP_GET_SUPER_LONG:
            case BTL_OP_CLASS_LONG: case BTL_OP_IMPORT_LONG:
            case BTL_OP_GET_PROPERTY_IC: case BTL_OP_SET_PROPERTY_IC:
            case BTL_OP_METHOD: case BTL_OP_DO_INVOKE:
            case BTL_OP_INVOKE: case BTL_OP_TAIL_INVOKE:
            case BTL_OP_SUPER_INVOKE: case BTL_OP_TAIL_SUPER_INVOKE:
            case BTL_OP_JUMP: case BTL_OP_JUMP_IF_FALSE: case BTL_OP_POP_JUMP_IF_FALSE:
            case BTL_OP_JUMP_IF_TRUE: case BTL_OP_POP_JUMP_IF_TRUE:
            case BTL_OP_JUMP_IF_NOT_EQUAL: case BTL_OP_JUMP_IF_EQUAL:
            case BTL_OP_JUMP_IF_NOT_GREATER: case BTL_OP_JUMP_IF_NOT_LESS:
            case BTL_OP_LOOP:
                ip += 2; break;
            case BTL_OP_INVOKE_LONG: case BTL_OP_TAIL_INVOKE_LONG:
            case BTL_OP_SUPER_INVOKE_LONG: case BTL_OP_TAIL_SUPER_INVOKE_LONG:
            case BTL_OP_INVOKE_IC: case BTL_OP_TAIL_INVOKE_IC:
            case BTL_OP_METHOD_LONG:
                ip += 3; break;
            case BTL_OP_CLOSURE: {
                uint8_t fn_idx = code[ip++];
                BtlValue fn_val = fn->chunk.constants.values[fn_idx];
                if (IS_OBJ(fn_val) && OBJ_TYPE(fn_val) == BTL_OBJ_FUNCTION) {
                    ObjFunction* child = AS_FUNCTION(fn_val);
                    ip += child->upvalueCount * 3;
                }
                break;
            }
            case BTL_OP_CLOSURE_LONG: {
                uint16_t fn_idx = (uint16_t)((code[ip] << 8) | code[ip + 1]); ip += 2;
                BtlValue fn_val = fn->chunk.constants.values[fn_idx];
                if (IS_OBJ(fn_val) && OBJ_TYPE(fn_val) == BTL_OBJ_FUNCTION) {
                    ObjFunction* child = AS_FUNCTION(fn_val);
                    ip += child->upvalueCount * 3;
                }
                break;
            }
            default:
                break; /* Zero-operand opcodes*/
            }
            continue; /* Skip to next opcode*/
        }

        switch (op) {

        // CONSTANTS & LITERALS - Direct stack writes
        case BTL_OP_CONSTANT: {
            uint8_t idx = code[ip++];
            emit_comment(t, start_ip, "OP_CONSTANT");
            BtlValue cval = fn->chunk.constants.values[idx];
            if (IS_INT(cval)) {
                OUT(t, "    PUSH(sp, INT_VAL(%" PRId64 "));\n", AS_INT(cval));
                type_push_tv(&ts, tracked_int(AS_INT(cval)));
            } else if (IS_NUMBER(cval)) {
                OUT(t, "    PUSH(sp, NUMBER_VAL(%.17g));\n", AS_NUMBER(cval));
                type_push_tv(&ts, tracked_number(AS_NUMBER(cval)));
            } else if (IS_STRING(cval)) {
                OUT(t, "    PUSH(sp, fn->chunk.constants.values[%d]);\n", idx);
                type_push_tv(&ts, tracked_string_const(idx));
            } else {
                OUT(t, "    PUSH(sp, fn->chunk.constants.values[%d]);\n", idx);
                type_push(&ts, TYPE_UNKNOWN);
            }
            break;
        }
        case BTL_OP_CONSTANT_LONG: {
            uint16_t idx = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            emit_comment(t, start_ip, "OP_CONSTANT_LONG");
            BtlValue cval_l = fn->chunk.constants.values[idx];
            if (IS_INT(cval_l)) {
                OUT(t, "    PUSH(sp, INT_VAL(%" PRId64 "));\n", AS_INT(cval_l));
                type_push_tv(&ts, tracked_int(AS_INT(cval_l)));
            } else if (IS_NUMBER(cval_l)) {
                OUT(t, "    PUSH(sp, NUMBER_VAL(%.17g));\n", AS_NUMBER(cval_l));
                type_push_tv(&ts, tracked_number(AS_NUMBER(cval_l)));
            } else if (IS_STRING(cval_l)) {
                OUT(t, "    PUSH(sp, fn->chunk.constants.values[%d]);\n", idx);
                type_push_tv(&ts, tracked_string_const(idx));
            } else {
                OUT(t, "    PUSH(sp, fn->chunk.constants.values[%d]);\n", idx);
                type_push(&ts, TYPE_UNKNOWN);
            }
            break;
        }
        case BTL_OP_NULL:
            emit_comment(t, start_ip, "OP_NULL");
            OUT(t, "    PUSH(sp, BTL_NULL_VAL);\n");
            type_push(&ts, TYPE_NIL);
            break;
        case BTL_OP_TRUE:
            emit_comment(t, start_ip, "OP_TRUE");
            OUT(t, "    PUSH(sp, BTL_TRUE_VAL);\n");
            type_push_tv(&ts, tracked_bool(true));
            break;
        case BTL_OP_FALSE:
            emit_comment(t, start_ip, "OP_FALSE");
            OUT(t, "    PUSH(sp, BTL_FALSE_VAL);\n");
            type_push_tv(&ts, tracked_bool(false));
            break;
        case BTL_OP_0:
            emit_comment(t, start_ip, "OP_0");
            OUT(t, "    PUSH(sp, NUMBER_VAL(0.0));\n");
            type_push_tv(&ts, tracked_number(0.0));
            break;
        case BTL_OP_1:
            emit_comment(t, start_ip, "OP_1");
            OUT(t, "    PUSH(sp, NUMBER_VAL(1.0));\n");
            type_push_tv(&ts, tracked_number(1.0));
            break;
        case BTL_OP_2:
            emit_comment(t, start_ip, "OP_2");
            OUT(t, "    PUSH(sp, NUMBER_VAL(2.0));\n");
            type_push_tv(&ts, tracked_number(2.0));
            break;
        case BTL_OP_INT_0:
            emit_comment(t, start_ip, "OP_INT_0");
            OUT(t, "    PUSH(sp, INT_VAL(0));\n");
            type_push_tv(&ts, tracked_int(0));
            break;
        case BTL_OP_INT_1:
            emit_comment(t, start_ip, "OP_INT_1");
            OUT(t, "    PUSH(sp, INT_VAL(1));\n");
            type_push_tv(&ts, tracked_int(1));
            break;
        case BTL_OP_INT_2:
            emit_comment(t, start_ip, "OP_INT_2");
            OUT(t, "    PUSH(sp, INT_VAL(2));\n");
            type_push_tv(&ts, tracked_int(2));
            break;

        // ================================================================
        // STACK MANIPULATION
        // ================================================================
        case BTL_OP_POP:
            emit_comment(t, start_ip, "OP_POP");
            OUT(t, "    sp--;\n");
            type_pop(&ts);
            break;
        case BTL_OP_POP_N: {
            uint8_t n = code[ip++];
            emit_comment(t, start_ip, "OP_POP_N");
            OUT(t, "    sp -= %d;\n", n);
            for (int i = 0; i < n; i++) type_pop(&ts);
            break;
        }
        case BTL_OP_DUP: {
            emit_comment(t, start_ip, "OP_DUP");
            OUT(t, "    sp[0] = sp[-1]; sp++;\n");
            AbstractType tt = type_peek(&ts, 0);
            type_push(&ts, tt);
            break;
        }
        case BTL_OP_SWAP: {
            emit_comment(t, start_ip, "OP_SWAP");
            OUT(t, "    { BtlValue _t = sp[-1]; sp[-1] = sp[-2]; sp[-2] = _t; }\n");
            AbstractType t0 = type_peek(&ts, 0);
            AbstractType t1 = type_peek(&ts, 1);
            type_pop(&ts); type_pop(&ts);
            type_push(&ts, t0);
            type_push(&ts, t1);
            break;
        }

        // LOCAL VARIABLES - Direct slots[] access
        case BTL_OP_GET_LOCAL: {
            uint8_t slot = code[ip++];
            emit_comment(t, start_ip, "OP_GET_LOCAL");
            OUT(t, "    PUSH(sp, slots[%d]);\n", slot);
            /* Push known type if tracked, otherwise unknown*/
            if (slot < MAX_TRACKED_LOCALS) {
                type_push_tv(&ts, ts.locals[slot]);
            } else {
                type_push(&ts, TYPE_UNKNOWN);
            }
            break;
        }
        case BTL_OP_GET_LOCAL_0: case BTL_OP_GET_LOCAL_1: case BTL_OP_GET_LOCAL_2:
        case BTL_OP_GET_LOCAL_3: case BTL_OP_GET_LOCAL_4: case BTL_OP_GET_LOCAL_5:
        case BTL_OP_GET_LOCAL_6: case BTL_OP_GET_LOCAL_7: {
            int slot = op - BTL_OP_GET_LOCAL_0;
            emit_comment(t, start_ip, "OP_GET_LOCAL_N");
            OUT(t, "    PUSH(sp, slots[%d]);\n", slot);
            /* Push known type if tracked, otherwise unknown*/
            if (slot < MAX_TRACKED_LOCALS) {
                type_push_tv(&ts, ts.locals[slot]);
            } else {
                type_push(&ts, TYPE_UNKNOWN);
            }
            break;
        }
        case BTL_OP_SET_LOCAL: {
            uint8_t slot = code[ip++];
            emit_comment(t, start_ip, "OP_SET_LOCAL");
            OUT(t, "    slots[%d] = sp[-1];\n", slot);
            /* Record the type being assigned*/
            if (slot < MAX_TRACKED_LOCALS) {
                ts.locals[slot] = type_peek_tv(&ts, 0);
            }
            break;
        }
        case BTL_OP_SET_LOCAL_0: case BTL_OP_SET_LOCAL_1: case BTL_OP_SET_LOCAL_2:
        case BTL_OP_SET_LOCAL_3: case BTL_OP_SET_LOCAL_4: case BTL_OP_SET_LOCAL_5:
        case BTL_OP_SET_LOCAL_6: case BTL_OP_SET_LOCAL_7: {
            int slot = op - BTL_OP_SET_LOCAL_0;
            emit_comment(t, start_ip, "OP_SET_LOCAL_N");
            OUT(t, "    slots[%d] = sp[-1];\n", slot);
            /* Record the type being assigned*/
            if (slot < MAX_TRACKED_LOCALS) {
                ts.locals[slot] = type_peek_tv(&ts, 0);
            }
            break;
        }
        case BTL_OP_SET_LOCAL_0_POP: case BTL_OP_SET_LOCAL_1_POP: case BTL_OP_SET_LOCAL_2_POP:
        case BTL_OP_SET_LOCAL_3_POP: case BTL_OP_SET_LOCAL_4_POP: case BTL_OP_SET_LOCAL_5_POP:
        case BTL_OP_SET_LOCAL_6_POP: case BTL_OP_SET_LOCAL_7_POP: {
            int slot = op - BTL_OP_SET_LOCAL_0_POP;
            emit_comment(t, start_ip, "OP_SET_LOCAL_N_POP");
            /* Record the type being assigned before popping*/
            if (slot < MAX_TRACKED_LOCALS) {
                ts.locals[slot] = type_peek_tv(&ts, 0);
            }
            OUT(t, "    slots[%d] = POP(sp);\n", slot);
            type_pop(&ts);
            break;
        }

                               // ================================================================
                               // INCREMENT / DECREMENT
                               // ================================================================
        case BTL_OP_INC_LOCAL_POP: {
            uint8_t slot = code[ip++];
            emit_comment(t, start_ip, "OP_INC_LOCAL_POP");
            if (slot < MAX_TRACKED_LOCALS && ts.locals[slot].type == TYPE_INT) {
                OUT(t, "    slots[%d] = INT_VAL(AS_INT(slots[%d]) + 1);\n", slot, slot);
                ts.locals[slot] = tracked_type(TYPE_INT);
            } else if (slot < MAX_TRACKED_LOCALS && ts.locals[slot].type == TYPE_NUMBER) {
                OUT(t, "    slots[%d] = NUMBER_VAL(AS_NUMBER(slots[%d]) + 1.0);\n", slot, slot);
                ts.locals[slot] = tracked_type(TYPE_NUMBER);
            } else {
                OUT(t, "    { BtlValue _v = slots[%d];\n", slot);
                OUT(t, "      if (IS_INT(_v)) slots[%d] = INT_VAL(AS_INT(_v) + 1);\n", slot);
                OUT(t, "      else if (__builtin_expect(IS_NUMBER(_v), 1)) slots[%d] = NUMBER_VAL(AS_NUMBER(_v) + 1.0);\n", slot);
                OUT(t, "      else return btl_error_not_number(vm, sp); }\n");
                if (slot < MAX_TRACKED_LOCALS) {
                    ts.locals[slot] = tracked_type(TYPE_UNKNOWN);
                }
            }
            break;
        }
        case BTL_OP_INC_LOCAL: {
            uint8_t slot = code[ip++];
            emit_comment(t, start_ip, "OP_INC_LOCAL");
            if (slot < MAX_TRACKED_LOCALS && ts.locals[slot].type == TYPE_INT) {
                OUT(t, "    { int64_t _v = AS_INT(slots[%d]) + 1; slots[%d] = INT_VAL(_v); PUSH(sp, INT_VAL(_v)); }\n", slot, slot);
                ts.locals[slot] = tracked_type(TYPE_INT);
                type_push(&ts, TYPE_INT);
            } else if (slot < MAX_TRACKED_LOCALS && ts.locals[slot].type == TYPE_NUMBER) {
                OUT(t, "    { double _v = AS_NUMBER(slots[%d]) + 1.0; slots[%d] = NUMBER_VAL(_v); PUSH(sp, NUMBER_VAL(_v)); }\n", slot, slot);
                ts.locals[slot] = tracked_type(TYPE_NUMBER);
                type_push(&ts, TYPE_NUMBER);
            } else {
                OUT(t, "    { BtlValue _v = slots[%d];\n", slot);
                OUT(t, "      if (IS_INT(_v)) { int64_t _n = AS_INT(_v) + 1; slots[%d] = INT_VAL(_n); PUSH(sp, INT_VAL(_n)); }\n", slot);
                OUT(t, "      else if (__builtin_expect(IS_NUMBER(_v), 1)) { double _n = AS_NUMBER(_v) + 1.0; slots[%d] = NUMBER_VAL(_n); PUSH(sp, NUMBER_VAL(_n)); }\n", slot);
                OUT(t, "      else return btl_error_not_number(vm, sp); }\n");
                if (slot < MAX_TRACKED_LOCALS) {
                    ts.locals[slot] = tracked_type(TYPE_UNKNOWN);
                }
                type_push(&ts, TYPE_UNKNOWN);
            }
            break;
        }
        case BTL_OP_INCREMENT: {
            emit_comment(t, start_ip, "OP_INCREMENT");
            TrackedValue tv_inc = type_peek_tv(&ts, 0);
            if (tv_inc.type == TYPE_INT) {
                OUT(t, "    sp[-1] = INT_VAL(AS_INT(sp[-1]) + 1);\n");
                type_set_top(&ts, TYPE_INT);
            } else if (tv_inc.type == TYPE_NUMBER) {
                OUT(t, "    sp[-1] = NUMBER_VAL(AS_NUMBER(sp[-1]) + 1.0);\n");
                type_set_top(&ts, TYPE_NUMBER);
            } else {
                OUT(t, "    { BtlValue _v = sp[-1];\n");
                OUT(t, "      if (IS_INT(_v)) sp[-1] = INT_VAL(AS_INT(_v) + 1);\n");
                OUT(t, "      else if (__builtin_expect(IS_NUMBER(_v), 1)) sp[-1] = NUMBER_VAL(AS_NUMBER(_v) + 1.0);\n");
                OUT(t, "      else return btl_error_not_number(vm, sp); }\n");
                type_set_top(&ts, TYPE_UNKNOWN);
            }
            break;
        }
        case BTL_OP_DECREMENT: {
            emit_comment(t, start_ip, "OP_DECREMENT");
            TrackedValue tv_dec = type_peek_tv(&ts, 0);
            if (tv_dec.type == TYPE_INT) {
                OUT(t, "    sp[-1] = INT_VAL(AS_INT(sp[-1]) - 1);\n");
                type_set_top(&ts, TYPE_INT);
            } else if (tv_dec.type == TYPE_NUMBER) {
                OUT(t, "    sp[-1] = NUMBER_VAL(AS_NUMBER(sp[-1]) - 1.0);\n");
                type_set_top(&ts, TYPE_NUMBER);
            } else {
                OUT(t, "    { BtlValue _v = sp[-1];\n");
                OUT(t, "      if (IS_INT(_v)) sp[-1] = INT_VAL(AS_INT(_v) - 1);\n");
                OUT(t, "      else if (__builtin_expect(IS_NUMBER(_v), 1)) sp[-1] = NUMBER_VAL(AS_NUMBER(_v) - 1.0);\n");
                OUT(t, "      else return btl_error_not_number(vm, sp); }\n");
                type_set_top(&ts, TYPE_UNKNOWN);
            }
            break;
        }

            // ================================================================
            // GLOBAL VARIABLES
            //
            // Must sync sp before runtimeError (it accesses the stack).
            // ================================================================
        case BTL_OP_GET_GLOBAL: {
            uint8_t idx = code[ip++];
            const char* name = find_global_name(t, idx);
            emit_comment(t, start_ip, "OP_GET_GLOBAL");
            /* Check if this global was hoisted for current loop*/
            int hoist_idx = -1;
            if (current_loop >= 0 && loop_hoists) {
                hoist_idx = find_hoist_candidate(&loop_hoists[current_loop], idx);
            }
            if (hoist_idx >= 0) {
                /* Use hoisted value - lazy init on first access*/
                OUT(t, "    { if (__builtin_expect(IS_EMPTY(_hoist_g%d_%d), 0)) {\n", current_loop, idx);
                OUT(t, "        _hoist_g%d_%d = mod->globalValues.values[%d];\n", current_loop, idx, idx);
                OUT(t, "        if (__builtin_expect(IS_EMPTY(_hoist_g%d_%d), 0)) return btl_error_undefined(vm, sp, \"%s\");\n", current_loop, idx, name);
                OUT(t, "      }\n");
                OUT(t, "      PUSH(sp, _hoist_g%d_%d); } /* LICM: %s*/\n", current_loop, idx, name);
            } else {
                OUT(t, "    { BtlValue _g = mod->globalValues.values[%d];\n", idx);
                OUT(t, "      if (__builtin_expect(IS_EMPTY(_g), 0)) return btl_error_undefined(vm, sp, \"%s\");\n", name);
                OUT(t, "      PUSH(sp, _g); }\n");
            }
            type_push(&ts, TYPE_UNKNOWN);  /* Global value is unknown type */
            break;
        }
        case BTL_OP_GET_GLOBAL_LONG: {
            uint16_t idx = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            const char* name = find_global_name(t, idx);
            emit_comment(t, start_ip, "OP_GET_GLOBAL_LONG");
            /* Check if this global was hoisted for current loop*/
            int hoist_idx = -1;
            if (current_loop >= 0 && loop_hoists && idx < 256) {
                hoist_idx = find_hoist_candidate(&loop_hoists[current_loop], idx);
            }
            if (hoist_idx >= 0) {
                /* Use hoisted value - lazy init on first access*/
                OUT(t, "    { if (__builtin_expect(IS_EMPTY(_hoist_g%d_%d), 0)) {\n", current_loop, idx);
                OUT(t, "        _hoist_g%d_%d = mod->globalValues.values[%d];\n", current_loop, idx, idx);
                OUT(t, "        if (__builtin_expect(IS_EMPTY(_hoist_g%d_%d), 0)) return btl_error_undefined(vm, sp, \"%s\");\n", current_loop, idx, name);
                OUT(t, "      }\n");
                OUT(t, "      PUSH(sp, _hoist_g%d_%d); } /* LICM: %s*/\n", current_loop, idx, name);
            } else {
                OUT(t, "    { BtlValue _g = mod->globalValues.values[%d];\n", idx);
                OUT(t, "      if (__builtin_expect(IS_EMPTY(_g), 0)) return btl_error_undefined(vm, sp, \"%s\");\n", name);
                OUT(t, "      PUSH(sp, _g); }\n");
            }
            type_push(&ts, TYPE_UNKNOWN);  /* Global value is unknown type */
            break;
        }
        case BTL_OP_DEFINE_GLOBAL: {
            uint8_t idx = code[ip++];
            emit_comment(t, start_ip, "OP_DEFINE_GLOBAL");
            OUT(t, "    mod->globalValues.values[%d] = POP(sp);\n", idx);
            type_pop(&ts);  /* Pop value being defined */
            break;
        }
        case BTL_OP_DEFINE_GLOBAL_LONG: {
            uint16_t idx = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            emit_comment(t, start_ip, "OP_DEFINE_GLOBAL_LONG");
            OUT(t, "    mod->globalValues.values[%d] = POP(sp);\n", idx);
            type_pop(&ts);  /* Pop value being defined */
            break;
        }
        case BTL_OP_SET_GLOBAL: {
            uint8_t idx = code[ip++];
            const char* name = find_global_name(t, idx);
            emit_comment(t, start_ip, "OP_SET_GLOBAL");
            OUT(t, "    if (__builtin_expect(IS_EMPTY(mod->globalValues.values[%d]), 0)) return btl_error_undefined(vm, sp, \"%s\");\n", idx, name);
            OUT(t, "    { BtlValue _v = sp[-1]; mod->globalValues.values[%d] = _v;\n", idx);
            OUT(t, "      if (IS_OBJ(_v)) btl_gc_write_barrier(vm, (BtlObj*)mod, _v); }\n");
            /* SET_GLOBAL leaves value on stack, so type state unchanged */
            break;
        }
        case BTL_OP_SET_GLOBAL_LONG: {
            uint16_t idx = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            const char* name = find_global_name(t, idx);
            emit_comment(t, start_ip, "OP_SET_GLOBAL_LONG");
            OUT(t, "    if (__builtin_expect(IS_EMPTY(mod->globalValues.values[%d]), 0)) return btl_error_undefined(vm, sp, \"%s\");\n", idx, name);
            OUT(t, "    { BtlValue _v = sp[-1]; mod->globalValues.values[%d] = _v;\n", idx);
            OUT(t, "      if (IS_OBJ(_v)) btl_gc_write_barrier(vm, (BtlObj*)mod, _v); }\n");
            /* SET_GLOBAL leaves value on stack, so type state unchanged */
            break;
        }

                               // ================================================================
                               // UPVALUES
                               // ================================================================
        case BTL_OP_GET_UPVALUE: {
            uint8_t slot = code[ip++];
            emit_comment(t, start_ip, "OP_GET_UPVALUE");
            OUT(t, "    { BtlRuntimeUpvalue* _uv = &frame->closure->upvalues[%d];\n", slot);
            OUT(t, "      PUSH(sp, _uv->isOpen ? *_uv->loc.stack : (_uv->isMutable ? _uv->loc.box->closed : _uv->loc.immValue)); }\n");
            type_push(&ts, TYPE_UNKNOWN);
            break;
        }
        case BTL_OP_GET_UPVALUE_OPEN: {
            uint8_t slot = code[ip++];
            emit_comment(t, start_ip, "OP_GET_UPVALUE_OPEN");
            OUT(t, "    PUSH(sp, *frame->closure->upvalues[%d].loc.stack);\n", slot);
            type_push(&ts, TYPE_UNKNOWN);
            break;
        }
        case BTL_OP_GET_UPVALUE_CLOSED: {
            uint8_t slot = code[ip++];
            emit_comment(t, start_ip, "OP_GET_UPVALUE_CLOSED");
            OUT(t, "    PUSH(sp, frame->closure->upvalues[%d].loc.box->closed);\n", slot);
            type_push(&ts, TYPE_UNKNOWN);
            break;
        }
        case BTL_OP_GET_UPVALUE_IMMUTABLE: {
            uint8_t slot = code[ip++];
            emit_comment(t, start_ip, "OP_GET_UPVALUE_IMMUTABLE");
            OUT(t, "    PUSH(sp, frame->closure->upvalues[%d].loc.immValue);\n", slot);
            type_push(&ts, TYPE_UNKNOWN);
            break;
        }
        case BTL_OP_SET_UPVALUE: {
            uint8_t slot = code[ip++];
            emit_comment(t, start_ip, "OP_SET_UPVALUE");
            OUT(t, "    { BtlRuntimeUpvalue* _uv = &frame->closure->upvalues[%d];\n", slot);
            OUT(t, "      if (_uv->isOpen) *_uv->loc.stack = sp[-1]; else _uv->loc.box->closed = sp[-1]; }\n");
            break;
        }
        case BTL_OP_SET_UPVALUE_OPEN: {
            uint8_t slot = code[ip++];
            emit_comment(t, start_ip, "OP_SET_UPVALUE_OPEN");
            OUT(t, "    *frame->closure->upvalues[%d].loc.stack = sp[-1];\n", slot);
            break;
        }
        case BTL_OP_SET_UPVALUE_CLOSED: {
            uint8_t slot = code[ip++];
            emit_comment(t, start_ip, "OP_SET_UPVALUE_CLOSED");
            OUT(t, "    frame->closure->upvalues[%d].loc.box->closed = sp[-1];\n", slot);
            break;
        }

                                  /* Specialized upvalue slot 0-3 GET*/
        case BTL_OP_GET_UPVALUE_0: case BTL_OP_GET_UPVALUE_OPEN_0:
        case BTL_OP_GET_UPVALUE_CLOSED_0: case BTL_OP_GET_UPVALUE_IMMUTABLE_0:
        case BTL_OP_GET_UPVALUE_1: case BTL_OP_GET_UPVALUE_OPEN_1:
        case BTL_OP_GET_UPVALUE_CLOSED_1: case BTL_OP_GET_UPVALUE_IMMUTABLE_1:
        case BTL_OP_GET_UPVALUE_2: case BTL_OP_GET_UPVALUE_OPEN_2:
        case BTL_OP_GET_UPVALUE_CLOSED_2: case BTL_OP_GET_UPVALUE_IMMUTABLE_2:
        case BTL_OP_GET_UPVALUE_3: case BTL_OP_GET_UPVALUE_OPEN_3:
        case BTL_OP_GET_UPVALUE_CLOSED_3: case BTL_OP_GET_UPVALUE_IMMUTABLE_3: {
            /* Each slot has 7 opcodes: GET, GET_OPEN, GET_CLOSED, GET_IMMUTABLE,
            // SET, SET_OPEN, SET_CLOSED. Decode slot and variant.*/
            int base = op - BTL_OP_GET_UPVALUE_0;
            int slot = base / 7;
            int variant = base % 7;  /* 0=generic, 1=OPEN, 2=CLOSED, 3=IMMUTABLE*/
            emit_comment(t, start_ip, "OP_GET_UPVALUE_N");
            switch (variant) {
            case 1: /* OPEN*/
                OUT(t, "    PUSH(sp, *frame->closure->upvalues[%d].loc.stack);\n", slot);
                break;
            case 2: /* CLOSED*/
                OUT(t, "    PUSH(sp, frame->closure->upvalues[%d].loc.box->closed);\n", slot);
                break;
            case 3: /* IMMUTABLE*/
                OUT(t, "    PUSH(sp, frame->closure->upvalues[%d].loc.immValue);\n", slot);
                break;
            default: /* generic*/
                OUT(t, "    { BtlRuntimeUpvalue* _uv = &frame->closure->upvalues[%d];\n", slot);
                OUT(t, "      PUSH(sp, _uv->isOpen ? *_uv->loc.stack : (_uv->isMutable ? _uv->loc.box->closed : _uv->loc.immValue)); }\n");
                break;
            }
            type_push(&ts, TYPE_UNKNOWN);
            break;
        }

                                    /* Specialized upvalue slot 0-3 SET*/
        case BTL_OP_SET_UPVALUE_0: case BTL_OP_SET_UPVALUE_OPEN_0: case BTL_OP_SET_UPVALUE_CLOSED_0:
        case BTL_OP_SET_UPVALUE_1: case BTL_OP_SET_UPVALUE_OPEN_1: case BTL_OP_SET_UPVALUE_CLOSED_1:
        case BTL_OP_SET_UPVALUE_2: case BTL_OP_SET_UPVALUE_OPEN_2: case BTL_OP_SET_UPVALUE_CLOSED_2:
        case BTL_OP_SET_UPVALUE_3: case BTL_OP_SET_UPVALUE_OPEN_3: case BTL_OP_SET_UPVALUE_CLOSED_3: {
            /* SET variants are at offsets 4, 5, 6 within each slot group of 7*/
            int base = op - BTL_OP_GET_UPVALUE_0;
            int slot = base / 7;
            int variant = base % 7;  /* 4=generic, 5=OPEN, 6=CLOSED*/
            emit_comment(t, start_ip, "OP_SET_UPVALUE_N");
            switch (variant) {
            case 5: /* OPEN*/
                OUT(t, "    *frame->closure->upvalues[%d].loc.stack = sp[-1];\n", slot);
                break;
            case 6: /* CLOSED*/
                OUT(t, "    frame->closure->upvalues[%d].loc.box->closed = sp[-1];\n", slot);
                break;
            default: /* generic*/
                OUT(t, "    { BtlRuntimeUpvalue* _uv = &frame->closure->upvalues[%d];\n", slot);
                OUT(t, "      if (_uv->isOpen) *_uv->loc.stack = sp[-1]; else _uv->loc.box->closed = sp[-1]; }\n");
                break;
            }
            break;
        }

                             // ================================================================
                             // FIELDS & PROPERTIES
                             // ================================================================
        case BTL_OP_FIELD: {
            uint8_t nameIdx = code[ip++];
            emit_comment(t, start_ip, "OP_FIELD");
            emit_sync(t);
            OUT(t, "    btl_compiled_field(vm, frame, %d);\n", nameIdx);
            emit_light_reload(t);
            break;
        }
        case BTL_OP_GET_FIELD_THIS: {
            uint8_t idx = code[ip++];
            emit_comment(t, start_ip, "OP_GET_FIELD_THIS");
            OUT(t, "    { ObjInstance* _inst = AS_INSTANCE(slots[0]);\n");
            OUT(t, "      if (__builtin_expect(_inst->klass->nativeGetters != NULL && _inst->klass->nativeGetters[%d] != NULL, 0))\n", idx);
            OUT(t, "        PUSH(sp, _inst->klass->nativeGetters[%d](vm, _inst->nativeData, %d));\n", idx, idx);
            OUT(t, "      else PUSH(sp, _inst->fields[%d]); }\n", idx);
            type_push(&ts, TYPE_UNKNOWN);
            break;
        }
        case BTL_OP_SET_FIELD_THIS: {
            uint8_t idx = code[ip++];
            emit_comment(t, start_ip, "OP_SET_FIELD_THIS");
            OUT(t, "    { ObjInstance* _inst = AS_INSTANCE(slots[0]); BtlValue _v = sp[-1];\n");
            OUT(t, "      if (__builtin_expect(_inst->klass->nativeSetters != NULL && _inst->klass->nativeSetters[%d] != NULL, 0))\n", idx);
            OUT(t, "        _inst->klass->nativeSetters[%d](vm, _inst->nativeData, %d, _v);\n", idx, idx);
            OUT(t, "      else { _inst->fields[%d] = _v; if (IS_OBJ(_v)) btl_gc_write_barrier(vm, (BtlObj*)_inst, _v); } }\n", idx);
            break;
        }
        case BTL_OP_GET_PROPERTY_IC: {
            uint8_t nameIdx = code[ip++];
            uint8_t icSlot = code[ip++];
            emit_comment(t, start_ip, "OP_GET_PROPERTY_IC");
            /* Inline IC fast path for instance field access */
            OUT(t, "    { BtlValue _recv = sp[-1];\n");
            OUT(t, "      if (__builtin_expect(IS_INSTANCE(_recv), 1)) {\n");
            OUT(t, "        ObjInstance* _inst = AS_INSTANCE(_recv);\n");
            OUT(t, "        BtlFieldIC* _ic = &frame->closure->fieldICs[%d];\n", icSlot);
            OUT(t, "        if (__builtin_expect(_ic->cachedClass == _inst->klass && _ic->fieldIndex >= 0, 1)) {\n");
            OUT(t, "          int _fi = _ic->fieldIndex;\n");
            OUT(t, "          if (__builtin_expect(_inst->klass->nativeGetters != NULL && _inst->klass->nativeGetters[_fi] != NULL, 0))\n");
            OUT(t, "            sp[-1] = _inst->klass->nativeGetters[_fi](vm, _inst->nativeData, _fi);\n");
            OUT(t, "          else sp[-1] = _inst->fields[_fi];\n");
            OUT(t, "          goto L_get_prop_%d_done;\n", start_ip);
            OUT(t, "        }\n");
            OUT(t, "      }\n");
            OUT(t, "      /* IC miss or non-instance: slow path */\n");
            emit_sync(t);
            OUT(t, "      if (!btl_compiled_get_property(vm, frame, %d, %d)) return BTL_INTERPRET_RUNTIME_ERROR;\n", nameIdx, icSlot);
            emit_light_reload(t);  /* Property access never pushes frames */
            OUT(t, "    L_get_prop_%d_done:; }\n", start_ip);
            type_pop(&ts);  /* Pop receiver */
            type_push(&ts, TYPE_UNKNOWN);  /* Push property value (unknown type) */
            break;
        }
        case BTL_OP_SET_PROPERTY_IC: {
            uint8_t nameIdx = code[ip++];
            uint8_t icSlot = code[ip++];
            emit_comment(t, start_ip, "OP_SET_PROPERTY_IC");
            /* Inline IC fast path for instance field assignment */
            OUT(t, "    { BtlValue _recv = sp[-2];\n");
            OUT(t, "      if (__builtin_expect(IS_INSTANCE(_recv), 1)) {\n");
            OUT(t, "        ObjInstance* _inst = AS_INSTANCE(_recv);\n");
            OUT(t, "        BtlFieldIC* _ic = &frame->closure->fieldICs[%d];\n", icSlot);
            OUT(t, "        if (__builtin_expect(_ic->cachedClass == _inst->klass && _ic->fieldIndex >= 0, 1)) {\n");
            OUT(t, "          int _fi = _ic->fieldIndex; BtlValue _val = sp[-1];\n");
            OUT(t, "          if (__builtin_expect(_inst->klass->nativeSetters != NULL && _inst->klass->nativeSetters[_fi] != NULL, 0))\n");
            OUT(t, "            _inst->klass->nativeSetters[_fi](vm, _inst->nativeData, _fi, _val);\n");
            OUT(t, "          else { _inst->fields[_fi] = _val; if (IS_OBJ(_val)) btl_gc_write_barrier(vm, (BtlObj*)_inst, _val); }\n");
            OUT(t, "          sp[-2] = _val; sp--;\n");
            OUT(t, "          goto L_set_prop_%d_done;\n", start_ip);
            OUT(t, "        }\n");
            OUT(t, "      }\n");
            OUT(t, "      /* IC miss or non-instance: slow path */\n");
            emit_sync(t);
            OUT(t, "      if (!btl_compiled_set_property(vm, frame, %d, %d)) return BTL_INTERPRET_RUNTIME_ERROR;\n", nameIdx, icSlot);
            emit_light_reload(t);  /* Property access never pushes frames */
            OUT(t, "    L_set_prop_%d_done:; }\n", start_ip);
            type_pop(&ts); type_pop(&ts);  /* Pop receiver and value */
            type_push(&ts, TYPE_UNKNOWN);  /* Push result value */
            break;
        }
        case BTL_OP_GET_SUPER: {
            uint8_t nameIdx = code[ip++];
            emit_comment(t, start_ip, "OP_GET_SUPER");
            emit_call_bracket_open(t);
            OUT(t, "    if (!btl_compiled_get_super(vm, frame, %d)) return BTL_INTERPRET_RUNTIME_ERROR;\n", nameIdx);
            emit_light_call_bracket_close(t);  /* Super property binding never pushes frames*/
            type_pop(&ts);                    /* superclass popped */
            type_pop(&ts);                    /* instance popped (will be replaced) */
            type_push(&ts, TYPE_OBJECT);      /* bound method pushed */
            break;
        }
        case BTL_OP_GET_SUPER_LONG: {
            uint16_t nameIdx = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            emit_comment(t, start_ip, "OP_GET_SUPER_LONG");
            emit_call_bracket_open(t);
            OUT(t, "    if (!btl_compiled_get_super_long(vm, frame, %d)) return BTL_INTERPRET_RUNTIME_ERROR;\n", nameIdx);
            emit_light_call_bracket_close(t);  /* Super property binding never pushes frames*/
            type_pop(&ts);                    /* superclass popped */
            type_pop(&ts);                    /* instance popped (will be replaced) */
            type_push(&ts, TYPE_OBJECT);      /* bound method pushed */
            break;
        }

                              // ================================================================
                              // ARITHMETIC Ã¢â‚¬â€ in-place on sp, no function calls
                              // ================================================================
        case BTL_OP_ADD: {
            emit_comment(t, start_ip, "OP_ADD");
            TrackedValue tvb = type_peek_tv(&ts, 0);
            TrackedValue tva = type_peek_tv(&ts, 1);
            type_pop_tv(&ts); type_pop_tv(&ts);
            if (tva.isConstant && tvb.isConstant && tva.type == TYPE_INT && tvb.type == TYPE_INT) {
                /* Both int constants - constant fold at compile time*/
                int64_t result = tva.intValue + tvb.intValue;
                OUT(t, "    sp -= 2; PUSH(sp, INT_VAL(%" PRId64 ")); /* const-folded int: %" PRId64 " + %" PRId64 "*/\n", result, tva.intValue, tvb.intValue);
                type_push_tv(&ts, tracked_int(result));
            } else if (tva.isConstant && tvb.isConstant && tva.type == TYPE_NUMBER && tvb.type == TYPE_NUMBER) {
                /* Both constants - constant fold at compile time*/
                double result = tva.numValue + tvb.numValue;
                OUT(t, "    sp -= 2; PUSH(sp, NUMBER_VAL(%.17g)); /* const-folded: %.17g + %.17g*/\n", result, tva.numValue, tvb.numValue);
                type_push_tv(&ts, tracked_number(result));
            } else if (tva.type == TYPE_INT && tvb.type == TYPE_INT) {
                /* Both known to be ints - skip type check entirely*/
                OUT(t, "    { int64_t _b = AS_INT(sp[-1]); sp[-2] = INT_VAL(AS_INT(sp[-2]) + _b); sp--; } /* type-specialized int*/\n");
                type_push(&ts, TYPE_INT);
            } else if (tva.type == TYPE_NUMBER && tvb.type == TYPE_NUMBER) {
                /* Both known to be numbers - skip type check entirely*/
                OUT(t, "    { double _b = AS_NUMBER(sp[-1]); sp[-2] = NUMBER_VAL(AS_NUMBER(sp[-2]) + _b); sp--; } /* type-specialized*/\n");
                type_push(&ts, TYPE_NUMBER);
            } else if (tva.type == TYPE_INT) {
                /* Left known INT, right unknown — one-sided fast path */
                OUT(t, "    { int64_t _a = AS_INT(sp[-2]); BtlValue _b = sp[-1];\n");
                OUT(t, "      if (__builtin_expect(IS_INT(_b), 1))\n");
                OUT(t, "        { sp[-2] = INT_VAL(_a + AS_INT(_b)); sp--; }\n");
                OUT(t, "      else if (__builtin_expect(IS_NUMBER(_b), 1))\n");
                OUT(t, "        { sp[-2] = NUMBER_VAL((double)_a + AS_NUMBER(_b)); sp--; }\n");
                OUT(t, "      else return btl_error_not_numbers(vm, sp); }\n");
                type_push(&ts, TYPE_UNKNOWN);
            } else if (tvb.type == TYPE_INT) {
                /* Right known INT, left unknown — one-sided fast path */
                OUT(t, "    { BtlValue _a = sp[-2]; int64_t _b = AS_INT(sp[-1]);\n");
                OUT(t, "      if (__builtin_expect(IS_INT(_a), 1))\n");
                OUT(t, "        { sp[-2] = INT_VAL(AS_INT(_a) + _b); sp--; }\n");
                OUT(t, "      else if (__builtin_expect(IS_NUMBER(_a), 1))\n");
                OUT(t, "        { sp[-2] = NUMBER_VAL(AS_NUMBER(_a) + (double)_b); sp--; }\n");
                OUT(t, "      else return btl_error_not_numbers(vm, sp); }\n");
                type_push(&ts, TYPE_UNKNOWN);
            } else if (tva.stringConstIdx >= 0 && tvb.stringConstIdx >= 0
                       && tva.type == TYPE_STRING && tvb.type == TYPE_STRING) {
                /* Both known string constants — fold at transpile time! */
                ObjString* sa = AS_STRING(fn->chunk.constants.values[tva.stringConstIdx]);
                ObjString* sb = AS_STRING(fn->chunk.constants.values[tvb.stringConstIdx]);
                int newLen = sa->length + sb->length;
                /* Emit a single interned string from literal — one alloc instead of alloc+2 memcpy+concat */
                OUT(t, "    { sp -= 2; vm->stackTop = sp;\n");
                OUT(t, "      ObjString* _fs = btl_string_copy(vm, \"");
                /* Write string contents, escaping special chars */
                for (int ci = 0; ci < sa->length; ci++) {
                    char ch = sa->chars[ci];
                    if (ch == '"') OUT(t, "\\\"");
                    else if (ch == '\\') OUT(t, "\\\\");
                    else if (ch == '\n') OUT(t, "\\n");
                    else if (ch == '\r') OUT(t, "\\r");
                    else if (ch == '\t') OUT(t, "\\t");
                    else OUT(t, "%c", ch);
                }
                for (int ci = 0; ci < sb->length; ci++) {
                    char ch = sb->chars[ci];
                    if (ch == '"') OUT(t, "\\\"");
                    else if (ch == '\\') OUT(t, "\\\\");
                    else if (ch == '\n') OUT(t, "\\n");
                    else if (ch == '\r') OUT(t, "\\r");
                    else if (ch == '\t') OUT(t, "\\t");
                    else OUT(t, "%c", ch);
                }
                OUT(t, "\", %d);\n", newLen);
                OUT(t, "      sp = vm->stackTop;\n");
                OUT(t, "      PUSH(sp, OBJ_VAL(_fs)); } /* const-folded string */\n");
                type_push(&ts, TYPE_STRING);
            } else if (tva.type == TYPE_STRING && tvb.type == TYPE_STRING) {
                /* Both known to be strings - try to fuse with following string ADDs */
                /* Scan ahead: look for patterns of <push_string> OP_ADD */
                int extra_strings = 0;
                int scan_ip = ip;
                int extra_bytes[16]; /* byte offsets consumed for each extra string */
                while (extra_strings < 16 && scan_ip < code_len) {
                    /* Check: next instruction pushes a string? */
                    int push_size = 0;
                    bool is_string_push = false;
                    uint8_t next_op = code[scan_ip];
                    if (next_op == BTL_OP_CONSTANT && scan_ip + 1 < code_len) {
                        uint8_t cidx = code[scan_ip + 1];
                        if (IS_STRING(fn->chunk.constants.values[cidx])) {
                            push_size = 2;
                            is_string_push = true;
                        }
                    }
                    if (!is_string_push) break;
                    /* Check: instruction after push is OP_ADD? */
                    int add_ip = scan_ip + push_size;
                    if (add_ip >= code_len || code[add_ip] != BTL_OP_ADD) break;
                    /* Check: no jump targets in the middle */
                    if (targets[scan_ip] || targets[add_ip]) break;
                    /* Found one more string to fuse */
                    extra_bytes[extra_strings] = push_size + 1; /* push + ADD */
                    extra_strings++;
                    scan_ip = add_ip + 1;
                }

                if (extra_strings > 0) {
                    /* Fused multi-string concat: 2 base + extra_strings operands */
                    int total_ops = 2 + extra_strings;
                    OUT(t, "    { /* fused %d-way string concat */\n", total_ops);
                    /* Extract all string pointers */
                    for (int si = 0; si < 2; si++) {
                        OUT(t, "      ObjString* _s%d = AS_STRING(sp[-%d]);\n", si, 2 - si);
                    }
                    /* Push the extra string constants (they're not on the stack yet, read from constants) */
                    int scan2 = ip;
                    for (int si = 0; si < extra_strings; si++) {
                        uint8_t cidx = code[scan2 + 1]; /* CONSTANT operand */
                        OUT(t, "      ObjString* _s%d = AS_STRING(fn->chunk.constants.values[%d]);\n", si + 2, cidx);
                        scan2 += extra_bytes[si];
                    }
                    /* Compute total length */
                    OUT(t, "      int _total_len = ");
                    for (int si = 0; si < total_ops; si++) {
                        if (si > 0) OUT(t, " + ");
                        OUT(t, "_s%d->length", si);
                    }
                    OUT(t, ";\n");
                    /* Single allocation */
                    OUT(t, "      vm->stackTop = sp;\n");
                    OUT(t, "      char* _chars = BTL_ALLOCATE(vm, char, _total_len + 1);\n");
                    /* Memcpy each string */
                    OUT(t, "      int _off = 0;\n");
                    for (int si = 0; si < total_ops; si++) {
                        OUT(t, "      memcpy(_chars + _off, _s%d->chars, _s%d->length); _off += _s%d->length;\n", si, si, si);
                    }
                    OUT(t, "      _chars[_total_len] = '\\0';\n");
                    OUT(t, "      ObjString* _res = btl_string_take(vm, _chars, _total_len);\n");
                    OUT(t, "      sp = vm->stackTop;\n");
                    OUT(t, "      sp -= 2; PUSH(sp, OBJ_VAL(_res)); } /* fused %d-way string concat */\n", total_ops);
                    /* Advance ip past the consumed bytes */
                    for (int si = 0; si < extra_strings; si++) {
                        ip += extra_bytes[si];
                    }
                } else {
                    /* Regular binary string concat */
                    OUT(t, "    { ObjString* _sa = AS_STRING(sp[-2]), *_sb = AS_STRING(sp[-1]);\n");
                    OUT(t, "      int _len = _sa->length + _sb->length;\n");
                    OUT(t, "      vm->stackTop = sp;\n");
                    OUT(t, "      char* _chars = BTL_ALLOCATE(vm, char, _len + 1);\n");
                    OUT(t, "      memcpy(_chars, _sa->chars, _sa->length);\n");
                    OUT(t, "      memcpy(_chars + _sa->length, _sb->chars, _sb->length);\n");
                    OUT(t, "      _chars[_len] = '\\0';\n");
                    OUT(t, "      ObjString* _res = btl_string_take(vm, _chars, _len);\n");
                    OUT(t, "      sp = vm->stackTop;\n");
                    OUT(t, "      sp[-2] = OBJ_VAL(_res); sp--; } /* type-specialized string*/\n");
                }
                type_push(&ts, TYPE_STRING);
            } else if (tva.type == TYPE_STRING || tvb.type == TYPE_STRING) {
                /* One side known STRING — skip numeric checks, go straight to string concat */
                OUT(t, "    { BtlValue _b = sp[-1], _a = sp[-2];\n");
                OUT(t, "      if (__builtin_expect(IS_STRING(_a) & IS_STRING(_b), 1)) {\n");
                OUT(t, "        ObjString* _sa = AS_STRING(_a), *_sb = AS_STRING(_b);\n");
                OUT(t, "        int _len = _sa->length + _sb->length;\n");
                OUT(t, "        vm->stackTop = sp;\n");
                OUT(t, "        char* _chars = BTL_ALLOCATE(vm, char, _len + 1);\n");
                OUT(t, "        memcpy(_chars, _sa->chars, _sa->length);\n");
                OUT(t, "        memcpy(_chars + _sa->length, _sb->chars, _sb->length);\n");
                OUT(t, "        _chars[_len] = '\\0';\n");
                OUT(t, "        ObjString* _res = btl_string_take(vm, _chars, _len);\n");
                OUT(t, "        sp = vm->stackTop;\n");
                OUT(t, "        sp[-2] = OBJ_VAL(_res); sp--;\n");
                OUT(t, "      } else {\n");
                emit_sync(t);
                OUT(t, "        if (!btl_compiled_add(vm)) return BTL_INTERPRET_RUNTIME_ERROR;\n");
                emit_light_reload(t);
                OUT(t, "    } }\n");
                type_push(&ts, TYPE_STRING);
            } else {
                /* Fast path: both ints, then both numbers (common case).
                   Second fast path: both strings (inline concat).
                   Slow path: mixed types via btl_compiled_add. */
                OUT(t, "    { BtlValue _b = sp[-1], _a = sp[-2];\n");
                OUT(t, "      if (__builtin_expect(IS_INT(_a) & IS_INT(_b), 1))\n");
                OUT(t, "        { sp[-2] = INT_VAL(AS_INT(_a) + AS_INT(_b)); sp--; }\n");
                OUT(t, "      else if (__builtin_expect(IS_NUMBER(_a) & IS_NUMBER(_b), 1))\n");
                OUT(t, "        { sp[-2] = NUMBER_VAL(AS_NUMBER(_a) + AS_NUMBER(_b)); sp--; }\n");
                OUT(t, "      else if (__builtin_expect(IS_NUMERIC(_a) & IS_NUMERIC(_b), 1))\n");
                OUT(t, "        { sp[-2] = NUMBER_VAL(btl_numeric_to_double(_a) + btl_numeric_to_double(_b)); sp--; }\n");
                OUT(t, "      else if (__builtin_expect(IS_STRING(_a) & IS_STRING(_b), 1)) {\n");
                OUT(t, "        /* Inline string concat */\n");
                OUT(t, "        ObjString* _sa = AS_STRING(_a), *_sb = AS_STRING(_b);\n");
                OUT(t, "        int _len = _sa->length + _sb->length;\n");
                OUT(t, "        vm->stackTop = sp;\n");
                OUT(t, "        char* _chars = BTL_ALLOCATE(vm, char, _len + 1);\n");
                OUT(t, "        memcpy(_chars, _sa->chars, _sa->length);\n");
                OUT(t, "        memcpy(_chars + _sa->length, _sb->chars, _sb->length);\n");
                OUT(t, "        _chars[_len] = '\\0';\n");
                OUT(t, "        ObjString* _res = btl_string_take(vm, _chars, _len);\n");
                OUT(t, "        sp = vm->stackTop;\n");
                OUT(t, "        sp[-2] = OBJ_VAL(_res); sp--;\n");
                OUT(t, "      } else {\n");
                emit_sync(t);
                OUT(t, "        if (!btl_compiled_add(vm)) return BTL_INTERPRET_RUNTIME_ERROR;\n");
                emit_light_reload(t);  /* String concat never pushes frames */
                OUT(t, "    } }\n");
                /* Track string type if either operand is known string */
                if (tva.type == TYPE_STRING || tvb.type == TYPE_STRING)
                    type_push(&ts, TYPE_STRING);
                else
                    type_push(&ts, TYPE_UNKNOWN);
            }
            break;
        }
        case BTL_OP_SUBTRACT: {
            emit_comment(t, start_ip, "OP_SUBTRACT");
            TrackedValue tvb = type_peek_tv(&ts, 0);
            TrackedValue tva = type_peek_tv(&ts, 1);
            type_pop_tv(&ts); type_pop_tv(&ts);
            if (tva.isConstant && tvb.isConstant && tva.type == TYPE_INT && tvb.type == TYPE_INT) {
                int64_t result = tva.intValue - tvb.intValue;
                OUT(t, "    sp -= 2; PUSH(sp, INT_VAL(%" PRId64 ")); /* const-folded int: %" PRId64 " - %" PRId64 "*/\n", result, tva.intValue, tvb.intValue);
                type_push_tv(&ts, tracked_int(result));
            } else if (tva.isConstant && tvb.isConstant && tva.type == TYPE_NUMBER && tvb.type == TYPE_NUMBER) {
                /* Both constants - constant fold at compile time*/
                double result = tva.numValue - tvb.numValue;
                OUT(t, "    sp -= 2; PUSH(sp, NUMBER_VAL(%.17g)); /* const-folded: %.17g - %.17g*/\n", result, tva.numValue, tvb.numValue);
                type_push_tv(&ts, tracked_number(result));
            } else if (tva.type == TYPE_INT && tvb.type == TYPE_INT) {
                OUT(t, "    { int64_t _b = AS_INT(sp[-1]); sp[-2] = INT_VAL(AS_INT(sp[-2]) - _b); sp--; } /* type-specialized int*/\n");
                type_push(&ts, TYPE_INT);
            } else if (tva.type == TYPE_NUMBER && tvb.type == TYPE_NUMBER) {
                /* Both known to be numbers - skip type check entirely*/
                OUT(t, "    { double _b = AS_NUMBER(sp[-1]); sp[-2] = NUMBER_VAL(AS_NUMBER(sp[-2]) - _b); sp--; } /* type-specialized*/\n");
                type_push(&ts, TYPE_NUMBER);
            } else if (tva.type == TYPE_INT) {
                /* Left known INT, right unknown — one-sided fast path */
                OUT(t, "    { int64_t _a = AS_INT(sp[-2]); BtlValue _b = sp[-1];\n");
                OUT(t, "      if (__builtin_expect(IS_INT(_b), 1))\n");
                OUT(t, "        { sp[-2] = INT_VAL(_a - AS_INT(_b)); sp--; }\n");
                OUT(t, "      else if (__builtin_expect(IS_NUMBER(_b), 1))\n");
                OUT(t, "        { sp[-2] = NUMBER_VAL((double)_a - AS_NUMBER(_b)); sp--; }\n");
                OUT(t, "      else return btl_error_not_numbers(vm, sp); }\n");
                type_push(&ts, TYPE_UNKNOWN);
            } else if (tvb.type == TYPE_INT) {
                /* Right known INT, left unknown — one-sided fast path */
                OUT(t, "    { BtlValue _a = sp[-2]; int64_t _b = AS_INT(sp[-1]);\n");
                OUT(t, "      if (__builtin_expect(IS_INT(_a), 1))\n");
                OUT(t, "        { sp[-2] = INT_VAL(AS_INT(_a) - _b); sp--; }\n");
                OUT(t, "      else if (__builtin_expect(IS_NUMBER(_a), 1))\n");
                OUT(t, "        { sp[-2] = NUMBER_VAL(AS_NUMBER(_a) - (double)_b); sp--; }\n");
                OUT(t, "      else return btl_error_not_numbers(vm, sp); }\n");
                type_push(&ts, TYPE_UNKNOWN);
            } else {
                OUT(t, "    { BtlValue _b = sp[-1], _a = sp[-2];\n");
                OUT(t, "      if (__builtin_expect(IS_INT(_a) & IS_INT(_b), 1))\n");
                OUT(t, "        { sp[-2] = INT_VAL(AS_INT(_a) - AS_INT(_b)); sp--; }\n");
                OUT(t, "      else if (__builtin_expect(IS_NUMBER(_a) & IS_NUMBER(_b), 1))\n");
                OUT(t, "        { sp[-2] = NUMBER_VAL(AS_NUMBER(_a) - AS_NUMBER(_b)); sp--; }\n");
                OUT(t, "      else if (__builtin_expect(IS_NUMERIC(_a) & IS_NUMERIC(_b), 1))\n");
                OUT(t, "        { sp[-2] = NUMBER_VAL(btl_numeric_to_double(_a) - btl_numeric_to_double(_b)); sp--; }\n");
                OUT(t, "      else return btl_error_not_numbers(vm, sp); }\n");
                type_push(&ts, TYPE_UNKNOWN);
            }
            break;
        }
        case BTL_OP_MULTIPLY: {
            emit_comment(t, start_ip, "OP_MULTIPLY");
            TrackedValue tvb = type_peek_tv(&ts, 0);
            TrackedValue tva = type_peek_tv(&ts, 1);
            type_pop_tv(&ts); type_pop_tv(&ts);
            if (tva.isConstant && tvb.isConstant && tva.type == TYPE_INT && tvb.type == TYPE_INT) {
                int64_t result = tva.intValue * tvb.intValue;
                OUT(t, "    sp -= 2; PUSH(sp, INT_VAL(%" PRId64 ")); /* const-folded int: %" PRId64 " * %" PRId64 " */\n", result, tva.intValue, tvb.intValue);
                type_push_tv(&ts, tracked_int(result));
            } else if (tva.isConstant && tvb.isConstant && tva.type == TYPE_NUMBER && tvb.type == TYPE_NUMBER) {
                /* Both constants - constant fold at compile time*/
                double result = tva.numValue * tvb.numValue;
                OUT(t, "    sp -= 2; PUSH(sp, NUMBER_VAL(%.17g)); /* const-folded: %.17g * %.17g */\n", result, tva.numValue, tvb.numValue);
                type_push_tv(&ts, tracked_number(result));
            } else if (tva.type == TYPE_INT && tvb.type == TYPE_INT) {
                OUT(t, "    { int64_t _b = AS_INT(sp[-1]); sp[-2] = INT_VAL(AS_INT(sp[-2]) * _b); sp--; } /* type-specialized int */\n");
                type_push(&ts, TYPE_INT);
            } else if (tva.type == TYPE_NUMBER && tvb.type == TYPE_NUMBER) {
                /* Both known to be numbers - skip type check entirely*/
                OUT(t, "    { double _b = AS_NUMBER(sp[-1]); sp[-2] = NUMBER_VAL(AS_NUMBER(sp[-2]) * _b); sp--; } /* type-specialized */\n");
                type_push(&ts, TYPE_NUMBER);
            } else if (tva.type == TYPE_INT) {
                /* Left known INT, right unknown — one-sided fast path */
                OUT(t, "    { int64_t _a = AS_INT(sp[-2]); BtlValue _b = sp[-1];\n");
                OUT(t, "      if (__builtin_expect(IS_INT(_b), 1))\n");
                OUT(t, "        { sp[-2] = INT_VAL(_a * AS_INT(_b)); sp--; }\n");
                OUT(t, "      else if (__builtin_expect(IS_NUMBER(_b), 1))\n");
                OUT(t, "        { sp[-2] = NUMBER_VAL((double)_a * AS_NUMBER(_b)); sp--; }\n");
                OUT(t, "      else return btl_error_not_numbers(vm, sp); }\n");
                type_push(&ts, TYPE_UNKNOWN);
            } else if (tvb.type == TYPE_INT) {
                /* Right known INT, left unknown — one-sided fast path */
                OUT(t, "    { BtlValue _a = sp[-2]; int64_t _b = AS_INT(sp[-1]);\n");
                OUT(t, "      if (__builtin_expect(IS_INT(_a), 1))\n");
                OUT(t, "        { sp[-2] = INT_VAL(AS_INT(_a) * _b); sp--; }\n");
                OUT(t, "      else if (__builtin_expect(IS_NUMBER(_a), 1))\n");
                OUT(t, "        { sp[-2] = NUMBER_VAL(AS_NUMBER(_a) * (double)_b); sp--; }\n");
                OUT(t, "      else return btl_error_not_numbers(vm, sp); }\n");
                type_push(&ts, TYPE_UNKNOWN);
            } else {
                OUT(t, "    { BtlValue _b = sp[-1], _a = sp[-2];\n");
                OUT(t, "      if (__builtin_expect(IS_INT(_a) & IS_INT(_b), 1))\n");
                OUT(t, "        { sp[-2] = INT_VAL(AS_INT(_a) * AS_INT(_b)); sp--; }\n");
                OUT(t, "      else if (__builtin_expect(IS_NUMBER(_a) & IS_NUMBER(_b), 1))\n");
                OUT(t, "        { sp[-2] = NUMBER_VAL(AS_NUMBER(_a) * AS_NUMBER(_b)); sp--; }\n");
                OUT(t, "      else if (__builtin_expect(IS_NUMERIC(_a) & IS_NUMERIC(_b), 1))\n");
                OUT(t, "        { sp[-2] = NUMBER_VAL(btl_numeric_to_double(_a) * btl_numeric_to_double(_b)); sp--; }\n");
                OUT(t, "      else return btl_error_not_numbers(vm, sp); }\n");
                type_push(&ts, TYPE_UNKNOWN);
            }
            break;
        }
        case BTL_OP_DIVIDE: {
            emit_comment(t, start_ip, "OP_DIVIDE");
            TrackedValue tvb = type_peek_tv(&ts, 0);
            TrackedValue tva = type_peek_tv(&ts, 1);
            type_pop_tv(&ts); type_pop_tv(&ts);
            if (tva.isConstant && tvb.isConstant && tva.type == TYPE_INT && tvb.type == TYPE_INT && tvb.intValue != 0) {
                int64_t result = tva.intValue / tvb.intValue;
                OUT(t, "    sp -= 2; PUSH(sp, INT_VAL(%" PRId64 ")); /* const-folded int: %" PRId64 " / %" PRId64 "*/\n", result, tva.intValue, tvb.intValue);
                type_push_tv(&ts, tracked_int(result));
            } else if (tva.isConstant && tvb.isConstant && tva.type == TYPE_NUMBER && tvb.type == TYPE_NUMBER && tvb.numValue != 0.0) {
                /* Both constants (non-zero divisor) - constant fold at compile time*/
                double result = tva.numValue / tvb.numValue;
                OUT(t, "    sp -= 2; PUSH(sp, NUMBER_VAL(%.17g)); /* const-folded: %.17g / %.17g*/\n", result, tva.numValue, tvb.numValue);
                type_push_tv(&ts, tracked_number(result));
            } else if (tva.type == TYPE_INT && tvb.type == TYPE_INT) {
                OUT(t, "    { int64_t _b = AS_INT(sp[-1]); if (__builtin_expect(_b == 0, 0)) return btl_error_div_zero(vm, sp);\n");
                OUT(t, "      sp[-2] = INT_VAL(AS_INT(sp[-2]) / _b); sp--; } /* type-specialized int*/\n");
                type_push(&ts, TYPE_INT);
            } else if (tva.type == TYPE_NUMBER && tvb.type == TYPE_NUMBER) {
                /* Both known to be numbers - skip type check entirely*/
                OUT(t, "    { double _b = AS_NUMBER(sp[-1]); sp[-2] = NUMBER_VAL(AS_NUMBER(sp[-2]) / _b); sp--; } /* type-specialized*/\n");
                type_push(&ts, TYPE_NUMBER);
            } else if (tva.type == TYPE_INT) {
                /* Left known INT, right unknown — one-sided fast path */
                OUT(t, "    { int64_t _a = AS_INT(sp[-2]); BtlValue _b = sp[-1];\n");
                OUT(t, "      if (__builtin_expect(IS_INT(_b), 1)) {\n");
                OUT(t, "        int64_t _bi = AS_INT(_b); if (__builtin_expect(_bi == 0, 0)) return btl_error_div_zero(vm, sp);\n");
                OUT(t, "        sp[-2] = INT_VAL(_a / _bi); sp--;\n");
                OUT(t, "      } else\n");
                OUT(t, "        { sp[-2] = NUMBER_VAL((double)_a / btl_numeric_to_double(_b)); sp--; } }\n");
                type_push(&ts, TYPE_UNKNOWN);
            } else if (tvb.type == TYPE_INT) {
                /* Right known INT, left unknown — one-sided fast path */
                OUT(t, "    { BtlValue _a = sp[-2]; int64_t _b = AS_INT(sp[-1]);\n");
                OUT(t, "      if (__builtin_expect(_b == 0, 0)) return btl_error_div_zero(vm, sp);\n");
                OUT(t, "      if (__builtin_expect(IS_INT(_a), 1))\n");
                OUT(t, "        { sp[-2] = INT_VAL(AS_INT(_a) / _b); sp--; }\n");
                OUT(t, "      else\n");
                OUT(t, "        { sp[-2] = NUMBER_VAL(btl_numeric_to_double(_a) / (double)_b); sp--; } }\n");
                type_push(&ts, TYPE_UNKNOWN);
            } else {
                OUT(t, "    { BtlValue _b = sp[-1], _a = sp[-2];\n");
                OUT(t, "      if (__builtin_expect(IS_INT(_a) & IS_INT(_b), 1)) {\n");
                OUT(t, "        int64_t _bi = AS_INT(_b); if (__builtin_expect(_bi == 0, 0)) return btl_error_div_zero(vm, sp);\n");
                OUT(t, "        sp[-2] = INT_VAL(AS_INT(_a) / _bi); sp--;\n");
                OUT(t, "      } else if (__builtin_expect(IS_NUMBER(_a) & IS_NUMBER(_b), 1))\n");
                OUT(t, "        { sp[-2] = NUMBER_VAL(AS_NUMBER(_a) / AS_NUMBER(_b)); sp--; }\n");
                OUT(t, "      else if (__builtin_expect(IS_NUMERIC(_a) & IS_NUMERIC(_b), 1))\n");
                OUT(t, "        { sp[-2] = NUMBER_VAL(btl_numeric_to_double(_a) / btl_numeric_to_double(_b)); sp--; }\n");
                OUT(t, "      else return btl_error_not_numbers(vm, sp); }\n");
                type_push(&ts, TYPE_UNKNOWN);
            }
            break;
        }
        case BTL_OP_MODULO: {
            emit_comment(t, start_ip, "OP_MODULO");
            TrackedValue tvb = type_peek_tv(&ts, 0);
            TrackedValue tva = type_peek_tv(&ts, 1);
            type_pop_tv(&ts); type_pop_tv(&ts);
            if (tva.isConstant && tvb.isConstant && tva.type == TYPE_INT && tvb.type == TYPE_INT && tvb.intValue != 0) {
                int64_t result = tva.intValue % tvb.intValue;
                OUT(t, "    sp -= 2; PUSH(sp, INT_VAL(%" PRId64 ")); /* const-folded int: %" PRId64 " %% %" PRId64 "*/\n", result, tva.intValue, tvb.intValue);
                type_push_tv(&ts, tracked_int(result));
            } else if (tva.isConstant && tvb.isConstant && tva.type == TYPE_NUMBER && tvb.type == TYPE_NUMBER && tvb.numValue != 0.0) {
                /* Both constants (non-zero divisor) - constant fold at compile time*/
                double result = fmod(tva.numValue, tvb.numValue);
                OUT(t, "    sp -= 2; PUSH(sp, NUMBER_VAL(%.17g)); /* const-folded: fmod(%.17g, %.17g)*/\n", result, tva.numValue, tvb.numValue);
                type_push_tv(&ts, tracked_number(result));
            } else if (tva.type == TYPE_INT && tvb.type == TYPE_INT) {
                OUT(t, "    { int64_t _b = AS_INT(sp[-1]); if (__builtin_expect(_b == 0, 0)) return btl_error_div_zero(vm, sp);\n");
                OUT(t, "      sp[-2] = INT_VAL(AS_INT(sp[-2]) %% _b); sp--; } /* type-specialized int*/\n");
                type_push(&ts, TYPE_INT);
            } else if (tva.type == TYPE_NUMBER && tvb.type == TYPE_NUMBER) {
                /* Both known to be numbers - skip type check entirely*/
                OUT(t, "    { double _b = AS_NUMBER(sp[-1]); sp[-2] = NUMBER_VAL(fmod(AS_NUMBER(sp[-2]), _b)); sp--; } /* type-specialized*/\n");
                type_push(&ts, TYPE_NUMBER);
            } else if (tva.type == TYPE_INT) {
                /* Left known INT, right unknown — one-sided fast path */
                OUT(t, "    { int64_t _a = AS_INT(sp[-2]); BtlValue _b = sp[-1];\n");
                OUT(t, "      if (__builtin_expect(IS_INT(_b), 1)) {\n");
                OUT(t, "        int64_t _bi = AS_INT(_b); if (__builtin_expect(_bi == 0, 0)) return btl_error_div_zero(vm, sp);\n");
                OUT(t, "        sp[-2] = INT_VAL(_a %% _bi); sp--;\n");
                OUT(t, "      } else\n");
                OUT(t, "        { sp[-2] = NUMBER_VAL(fmod((double)_a, btl_numeric_to_double(_b))); sp--; } }\n");
                type_push(&ts, TYPE_UNKNOWN);
            } else if (tvb.type == TYPE_INT) {
                /* Right known INT, left unknown — one-sided fast path */
                OUT(t, "    { BtlValue _a = sp[-2]; int64_t _b = AS_INT(sp[-1]);\n");
                OUT(t, "      if (__builtin_expect(_b == 0, 0)) return btl_error_div_zero(vm, sp);\n");
                OUT(t, "      if (__builtin_expect(IS_INT(_a), 1))\n");
                OUT(t, "        { sp[-2] = INT_VAL(AS_INT(_a) %% _b); sp--; }\n");
                OUT(t, "      else\n");
                OUT(t, "        { sp[-2] = NUMBER_VAL(fmod(btl_numeric_to_double(_a), (double)_b)); sp--; } }\n");
                type_push(&ts, TYPE_UNKNOWN);
            } else {
                OUT(t, "    { BtlValue _b = sp[-1], _a = sp[-2];\n");
                OUT(t, "      if (__builtin_expect(IS_INT(_a) & IS_INT(_b), 1)) {\n");
                OUT(t, "        int64_t _bi = AS_INT(_b); if (__builtin_expect(_bi == 0, 0)) return btl_error_div_zero(vm, sp);\n");
                OUT(t, "        sp[-2] = INT_VAL(AS_INT(_a) %% _bi); sp--;\n");
                OUT(t, "      } else if (__builtin_expect(IS_NUMBER(_a) & IS_NUMBER(_b), 1))\n");
                OUT(t, "        { sp[-2] = NUMBER_VAL(fmod(AS_NUMBER(_a), AS_NUMBER(_b))); sp--; }\n");
                OUT(t, "      else if (__builtin_expect(IS_NUMERIC(_a) & IS_NUMERIC(_b), 1))\n");
                OUT(t, "        { sp[-2] = NUMBER_VAL(fmod(btl_numeric_to_double(_a), btl_numeric_to_double(_b))); sp--; }\n");
                OUT(t, "      else return btl_error_not_numbers(vm, sp); }\n");
                type_push(&ts, TYPE_UNKNOWN);
            }
            break;
        }
        case BTL_OP_NEGATE: {
            emit_comment(t, start_ip, "OP_NEGATE");
            TrackedValue tva = type_peek_tv(&ts, 0);
            if (tva.isConstant && tva.type == TYPE_INT) {
                int64_t result = -tva.intValue;
                OUT(t, "    sp[-1] = INT_VAL(%" PRId64 "); /* const-folded int: -%" PRId64 "*/\n", result, tva.intValue);
                type_set_top_tv(&ts, tracked_int(result));
            } else if (tva.isConstant && tva.type == TYPE_NUMBER) {
                /* Constant - fold at compile time*/
                double result = -tva.numValue;
                OUT(t, "    sp[-1] = NUMBER_VAL(%.17g); /* const-folded: -%.17g*/\n", result, tva.numValue);
                type_set_top_tv(&ts, tracked_number(result));
            } else if (tva.type == TYPE_INT) {
                OUT(t, "    sp[-1] = INT_VAL(-AS_INT(sp[-1])); /* type-specialized int*/\n");
                type_set_top(&ts, TYPE_INT);
            } else if (tva.type == TYPE_NUMBER) {
                /* Known to be a number - skip type check entirely*/
                OUT(t, "    sp[-1] = NUMBER_VAL(-AS_NUMBER(sp[-1])); /* type-specialized*/\n");
                type_set_top(&ts, TYPE_NUMBER);
            } else {
                OUT(t, "    if (IS_INT(sp[-1])) sp[-1] = INT_VAL(-AS_INT(sp[-1]));\n");
                OUT(t, "    else if (__builtin_expect(IS_NUMBER(sp[-1]), 1)) sp[-1] = NUMBER_VAL(-AS_NUMBER(sp[-1]));\n");
                OUT(t, "    else return btl_error_not_number(vm, sp);\n");
                type_set_top(&ts, TYPE_UNKNOWN);
            }
            break;
        }
        case BTL_OP_NOT: {
            TrackedValue tv_not = type_peek_tv(&ts, 0);
            emit_comment(t, start_ip, "OP_NOT");
            if (tv_not.isConstant && tv_not.type == TYPE_BOOL) {
                bool result = tv_not.numValue == 0.0;
                OUT(t, "    sp[-1] = %s; /* const-folded: !%s */\n", result ? "BTL_TRUE_VAL" : "BTL_FALSE_VAL", tv_not.numValue != 0.0 ? "true" : "false");
                type_set_top_tv(&ts, tracked_bool(result));
            } else if (tv_not.isConstant && tv_not.type == TYPE_INT) {
                bool result = tv_not.intValue == 0;
                OUT(t, "    sp[-1] = %s; /* const-folded: !%" PRId64 " */\n", result ? "BTL_TRUE_VAL" : "BTL_FALSE_VAL", tv_not.intValue);
                type_set_top_tv(&ts, tracked_bool(result));
            } else if (tv_not.isConstant && tv_not.type == TYPE_NUMBER) {
                bool result = tv_not.numValue == 0.0;
                OUT(t, "    sp[-1] = %s; /* const-folded: !%.17g */\n", result ? "BTL_TRUE_VAL" : "BTL_FALSE_VAL", tv_not.numValue);
                type_set_top_tv(&ts, tracked_bool(result));
            } else if (tv_not.type == TYPE_NIL) {
                OUT(t, "    sp[-1] = BTL_TRUE_VAL; /* !nil = true*/\n");
                type_set_top_tv(&ts, tracked_bool(true));
            } else if (tv_not.type == TYPE_STRING || tv_not.type == TYPE_OBJECT) {
                OUT(t, "    sp[-1] = BTL_FALSE_VAL; /* !string/object = false*/\n");
                type_set_top_tv(&ts, tracked_bool(false));
            } else if (tv_not.type == TYPE_BOOL) {
                OUT(t, "    sp[-1] = BOOL_VAL(!AS_BOOL(sp[-1]));\n");
                type_set_top(&ts, TYPE_BOOL);
            } else if (tv_not.type == TYPE_INT) {
                OUT(t, "    sp[-1] = BOOL_VAL(AS_INT(sp[-1]) == 0);\n");
                type_set_top(&ts, TYPE_BOOL);
            } else if (tv_not.type == TYPE_NUMBER) {
                OUT(t, "    sp[-1] = BOOL_VAL(AS_NUMBER(sp[-1]) == 0.0);\n");
                type_set_top(&ts, TYPE_BOOL);
            } else {
                OUT(t, "    sp[-1] = BOOL_VAL(btl_compiled_is_falsey(sp[-1]));\n");
                type_set_top(&ts, TYPE_BOOL);
            }
            break;
        }

            // ================================================================
            // COMPARISON
            // ================================================================
        case BTL_OP_EQUAL: {
            emit_comment(t, start_ip, "OP_EQUAL");
            TrackedValue tv_ea = type_peek_tv(&ts, 1);
            TrackedValue tv_eb = type_peek_tv(&ts, 0);
            AbstractType ta = tv_ea.type;
            AbstractType tb = tv_eb.type;
            if (tv_ea.isConstant && tv_eb.isConstant && ta == TYPE_INT && tb == TYPE_INT) {
                bool result = tv_ea.intValue == tv_eb.intValue;
                OUT(t, "    sp -= 2; PUSH(sp, %s); /* const-folded int: %" PRId64 " == %" PRId64 " */\n", result ? "BTL_TRUE_VAL" : "BTL_FALSE_VAL", tv_ea.intValue, tv_eb.intValue);
                type_pop(&ts); type_pop(&ts);
                type_push_tv(&ts, tracked_bool(result));
                break;
            }
            if (tv_ea.isConstant && tv_eb.isConstant && ta == TYPE_NUMBER && tb == TYPE_NUMBER) {
                bool result = tv_ea.numValue == tv_eb.numValue;
                OUT(t, "    sp -= 2; PUSH(sp, %s); /* const-folded: %.17g == %.17g */\n", result ? "BTL_TRUE_VAL" : "BTL_FALSE_VAL", tv_ea.numValue, tv_eb.numValue);
                type_pop(&ts); type_pop(&ts);
                type_push_tv(&ts, tracked_bool(result));
                break;
            }
            if (tv_ea.isConstant && tv_eb.isConstant && ta == TYPE_BOOL && tb == TYPE_BOOL) {
                bool result = (tv_ea.numValue != 0.0) == (tv_eb.numValue != 0.0);
                OUT(t, "    sp -= 2; PUSH(sp, %s); /* const-folded bool */\n", result ? "BTL_TRUE_VAL" : "BTL_FALSE_VAL");
                type_pop(&ts); type_pop(&ts);
                type_push_tv(&ts, tracked_bool(result));
                break;
            }
            if (ta == TYPE_INT && tb == TYPE_INT) {
                OUT(t, "    { int64_t _b = AS_INT(sp[-1]); sp[-2] = BOOL_VAL(AS_INT(sp[-2]) == _b); sp--; } /* type-specialized int*/\n");
            } else if (ta == TYPE_NUMBER && tb == TYPE_NUMBER) {
                OUT(t, "    { double _b = AS_NUMBER(sp[-1]); sp[-2] = BOOL_VAL(AS_NUMBER(sp[-2]) == _b); sp--; } /* type-specialized number*/\n");
            } else if (ta == TYPE_BOOL && tb == TYPE_BOOL) {
                OUT(t, "    { sp[-2] = BOOL_VAL(sp[-2] == sp[-1]); sp--; } /* type-specialized bool*/\n");
            } else if (ta == TYPE_NIL && tb == TYPE_NIL) {
                OUT(t, "    sp -= 2; PUSH(sp, BTL_TRUE_VAL); /* nil==nil */\n");
            } else if (ta == TYPE_NIL) {
                OUT(t, "    { sp[-2] = BOOL_VAL(btl_compiled_is_null_like(sp[-1])); sp--; } /* nil==? */\n");
            } else if (tb == TYPE_NIL) {
                OUT(t, "    { sp[-2] = BOOL_VAL(btl_compiled_is_null_like(sp[-2])); sp--; } /* ?==nil */\n");
            } else if (ta == TYPE_STRING && tb == TYPE_STRING) {
                OUT(t, "    { sp[-2] = BOOL_VAL(sp[-2] == sp[-1]); sp--; } /* interned string==string */\n");
            } else if (ta == TYPE_INT) {
                OUT(t, "    { BtlValue _b = sp[-1]; sp[-2] = BOOL_VAL(__builtin_expect(IS_INT(_b), 1) ? (AS_INT(sp[-2]) == AS_INT(_b)) : btl_compiled_equal(sp[-2], _b)); sp--; } /* int==? */\n");
            } else if (tb == TYPE_INT) {
                OUT(t, "    { BtlValue _a = sp[-2]; sp[-2] = BOOL_VAL(__builtin_expect(IS_INT(_a), 1) ? (AS_INT(_a) == AS_INT(sp[-1])) : btl_compiled_equal(_a, sp[-1])); sp--; } /* ?==int */\n");
            } else {
                OUT(t, "    { BtlValue _b = POP(sp); sp[-1] = BOOL_VAL(btl_compiled_equal(sp[-1], _b)); }\n");
            }
            type_pop(&ts); type_pop(&ts);
            type_push(&ts, TYPE_BOOL);
            break;
        }
        case BTL_OP_GREATER: {
            emit_comment(t, start_ip, "OP_GREATER");
            TrackedValue tvb = type_peek_tv(&ts, 0);
            TrackedValue tva = type_peek_tv(&ts, 1);
            type_pop_tv(&ts); type_pop_tv(&ts);
            if (tva.isConstant && tvb.isConstant && tva.type == TYPE_INT && tvb.type == TYPE_INT) {
                bool result = tva.intValue > tvb.intValue;
                OUT(t, "    sp -= 2; PUSH(sp, %s); /* const-folded int: %" PRId64 " > %" PRId64 "*/\n", result ? "BTL_TRUE_VAL" : "BTL_FALSE_VAL", tva.intValue, tvb.intValue);
                type_push_tv(&ts, tracked_bool(result));
            } else if (tva.isConstant && tvb.isConstant && tva.type == TYPE_NUMBER && tvb.type == TYPE_NUMBER) {
                /* Both constants - constant fold at compile time*/
                bool result = tva.numValue > tvb.numValue;
                OUT(t, "    sp -= 2; PUSH(sp, %s); /* const-folded: %.17g > %.17g*/\n", result ? "BTL_TRUE_VAL" : "BTL_FALSE_VAL", tva.numValue, tvb.numValue);
                type_push_tv(&ts, tracked_bool(result));
            } else if (tva.type == TYPE_INT && tvb.type == TYPE_INT) {
                OUT(t, "    { int64_t _b = AS_INT(sp[-1]); sp[-2] = BOOL_VAL(AS_INT(sp[-2]) > _b); sp--; } /* type-specialized int*/\n");
                type_push(&ts, TYPE_BOOL);
            } else if (tva.type == TYPE_NUMBER && tvb.type == TYPE_NUMBER) {
                /* Both known to be numbers - skip type check entirely*/
                OUT(t, "    { double _b = AS_NUMBER(sp[-1]); sp[-2] = BOOL_VAL(AS_NUMBER(sp[-2]) > _b); sp--; } /* type-specialized*/\n");
                type_push(&ts, TYPE_BOOL);
            } else if (tva.type == TYPE_INT) {
                /* Left known INT, right unknown — one-sided type check*/
                OUT(t, "    { int64_t _a = AS_INT(sp[-2]); BtlValue _b = sp[-1];\n");
                OUT(t, "      sp[-2] = BOOL_VAL(__builtin_expect(IS_INT(_b), 1) ?\n");
                OUT(t, "        (_a > AS_INT(_b)) : ((double)_a > btl_numeric_to_double(_b))); sp--; }\n");
                type_push(&ts, TYPE_BOOL);
            } else if (tvb.type == TYPE_INT) {
                /* Right known INT, left unknown — one-sided type check*/
                OUT(t, "    { BtlValue _a = sp[-2]; int64_t _b = AS_INT(sp[-1]);\n");
                OUT(t, "      sp[-2] = BOOL_VAL(__builtin_expect(IS_INT(_a), 1) ?\n");
                OUT(t, "        (AS_INT(_a) > _b) : (btl_numeric_to_double(_a) > (double)_b)); sp--; }\n");
                type_push(&ts, TYPE_BOOL);
            } else {
                OUT(t, "    { BtlValue _b = sp[-1], _a = sp[-2];\n");
                OUT(t, "      if (__builtin_expect(IS_INT(_a) & IS_INT(_b), 1))\n");
                OUT(t, "        { sp[-2] = BOOL_VAL(AS_INT(_a) > AS_INT(_b)); sp--; }\n");
                OUT(t, "      else if (__builtin_expect(IS_NUMBER(_a) & IS_NUMBER(_b), 1))\n");
                OUT(t, "        { sp[-2] = BOOL_VAL(AS_NUMBER(_a) > AS_NUMBER(_b)); sp--; }\n");
                OUT(t, "      else if (__builtin_expect(IS_NUMERIC(_a) & IS_NUMERIC(_b), 1))\n");
                OUT(t, "        { sp[-2] = BOOL_VAL(btl_numeric_to_double(_a) > btl_numeric_to_double(_b)); sp--; }\n");
                OUT(t, "      else return btl_error_not_numbers(vm, sp); }\n");
                type_push(&ts, TYPE_BOOL);
            }
            break;
        }
        case BTL_OP_LESS: {
            emit_comment(t, start_ip, "OP_LESS");
            TrackedValue tvb = type_peek_tv(&ts, 0);
            TrackedValue tva = type_peek_tv(&ts, 1);
            type_pop_tv(&ts); type_pop_tv(&ts);
            if (tva.isConstant && tvb.isConstant && tva.type == TYPE_INT && tvb.type == TYPE_INT) {
                bool result = tva.intValue < tvb.intValue;
                OUT(t, "    sp -= 2; PUSH(sp, %s); /* const-folded int: %" PRId64 " < %" PRId64 "*/\n", result ? "BTL_TRUE_VAL" : "BTL_FALSE_VAL", tva.intValue, tvb.intValue);
                type_push_tv(&ts, tracked_bool(result));
            } else if (tva.isConstant && tvb.isConstant && tva.type == TYPE_NUMBER && tvb.type == TYPE_NUMBER) {
                /* Both constants - constant fold at compile time*/
                bool result = tva.numValue < tvb.numValue;
                OUT(t, "    sp -= 2; PUSH(sp, %s); /* const-folded: %.17g < %.17g*/\n", result ? "BTL_TRUE_VAL" : "BTL_FALSE_VAL", tva.numValue, tvb.numValue);
                type_push_tv(&ts, tracked_bool(result));
            } else if (tva.type == TYPE_INT && tvb.type == TYPE_INT) {
                OUT(t, "    { int64_t _b = AS_INT(sp[-1]); sp[-2] = BOOL_VAL(AS_INT(sp[-2]) < _b); sp--; } /* type-specialized int*/\n");
                type_push(&ts, TYPE_BOOL);
            } else if (tva.type == TYPE_NUMBER && tvb.type == TYPE_NUMBER) {
                /* Both known to be numbers - skip type check entirely*/
                OUT(t, "    { double _b = AS_NUMBER(sp[-1]); sp[-2] = BOOL_VAL(AS_NUMBER(sp[-2]) < _b); sp--; } /* type-specialized*/\n");
                type_push(&ts, TYPE_BOOL);
            } else if (tva.type == TYPE_INT) {
                /* Left known INT, right unknown — one-sided type check*/
                OUT(t, "    { int64_t _a = AS_INT(sp[-2]); BtlValue _b = sp[-1];\n");
                OUT(t, "      sp[-2] = BOOL_VAL(__builtin_expect(IS_INT(_b), 1) ?\n");
                OUT(t, "        (_a < AS_INT(_b)) : ((double)_a < btl_numeric_to_double(_b))); sp--; }\n");
                type_push(&ts, TYPE_BOOL);
            } else if (tvb.type == TYPE_INT) {
                /* Right known INT, left unknown — one-sided type check*/
                OUT(t, "    { BtlValue _a = sp[-2]; int64_t _b = AS_INT(sp[-1]);\n");
                OUT(t, "      sp[-2] = BOOL_VAL(__builtin_expect(IS_INT(_a), 1) ?\n");
                OUT(t, "        (AS_INT(_a) < _b) : (btl_numeric_to_double(_a) < (double)_b)); sp--; }\n");
                type_push(&ts, TYPE_BOOL);
            } else {
                OUT(t, "    { BtlValue _b = sp[-1], _a = sp[-2];\n");
                OUT(t, "      if (__builtin_expect(IS_INT(_a) & IS_INT(_b), 1))\n");
                OUT(t, "        { sp[-2] = BOOL_VAL(AS_INT(_a) < AS_INT(_b)); sp--; }\n");
                OUT(t, "      else if (__builtin_expect(IS_NUMBER(_a) & IS_NUMBER(_b), 1))\n");
                OUT(t, "        { sp[-2] = BOOL_VAL(AS_NUMBER(_a) < AS_NUMBER(_b)); sp--; }\n");
                OUT(t, "      else if (__builtin_expect(IS_NUMERIC(_a) & IS_NUMERIC(_b), 1))\n");
                OUT(t, "        { sp[-2] = BOOL_VAL(btl_numeric_to_double(_a) < btl_numeric_to_double(_b)); sp--; }\n");
                OUT(t, "      else return btl_error_not_numbers(vm, sp); }\n");
                type_push(&ts, TYPE_BOOL);
            }
            break;
        }

            // ================================================================
            // CONTROL FLOW
            // ================================================================
        case BTL_OP_JUMP: {
            uint16_t offset = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            if (reachable) {
                emit_comment(t, start_ip, "OP_JUMP");
                OUT(t, "    goto L_%04d;\n", ip + offset);
            }
            reachable = false; /* Code after unconditional jump is dead*/
            break;
        }
        case BTL_OP_JUMP_IF_FALSE: {
            uint16_t offset = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            if (reachable) {
                AbstractType jf_type = type_peek(&ts, 0);
                emit_comment(t, start_ip, "OP_JUMP_IF_FALSE");
                if (jf_type == TYPE_BOOL)
                    OUT(t, "    if (!AS_BOOL(sp[-1])) goto L_%04d;\n", ip + offset);
                else if (jf_type == TYPE_INT)
                    OUT(t, "    if (AS_INT(sp[-1]) == 0) goto L_%04d;\n", ip + offset);
                else if (jf_type == TYPE_NUMBER)
                    OUT(t, "    if (AS_NUMBER(sp[-1]) == 0.0) goto L_%04d;\n", ip + offset);
                else
                    OUT(t, "    if (btl_compiled_is_falsey(sp[-1])) goto L_%04d;\n", ip + offset);
            }
            break;
        }
        case BTL_OP_POP_JUMP_IF_FALSE: {
            uint16_t offset = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            if (reachable) {
                /* Check if condition is a known constant for DCE*/
                TrackedValue tv = type_peek_tv(&ts, 0);
                if (tv.isConstant && tv.type == TYPE_BOOL) {
                    bool isFalsey = tv.numValue == 0.0;  /* false = 0.0, true = non-zero*/
                    emit_comment(t, start_ip, "OP_POP_JUMP_IF_FALSE (const-elim)");
                    OUT(t, "    sp--; /* pop const bool*/\n");
                    if (isFalsey) {
                        OUT(t, "    goto L_%04d; /* const true: always jump*/\n", ip + offset);
                        reachable = false;
                    }
                    /* else: const false, never jumps, fall through*/
                } else if (tv.type == TYPE_BOOL) {
                    emit_comment(t, start_ip, "OP_POP_JUMP_IF_FALSE");
                    OUT(t, "    if (!AS_BOOL(POP(sp))) goto L_%04d;\n", ip + offset);
                } else if (tv.type == TYPE_INT) {
                    emit_comment(t, start_ip, "OP_POP_JUMP_IF_FALSE");
                    OUT(t, "    if (AS_INT(POP(sp)) == 0) goto L_%04d;\n", ip + offset);
                } else if (tv.type == TYPE_NUMBER) {
                    emit_comment(t, start_ip, "OP_POP_JUMP_IF_FALSE");
                    OUT(t, "    if (AS_NUMBER(POP(sp)) == 0.0) goto L_%04d;\n", ip + offset);
                } else if (tv.type == TYPE_NIL) {
                    emit_comment(t, start_ip, "OP_POP_JUMP_IF_FALSE");
                    OUT(t, "    sp--; goto L_%04d; /* nil is always falsey*/\n", ip + offset);
                    reachable = false;
                } else if (tv.type == TYPE_STRING) {
                    emit_comment(t, start_ip, "OP_POP_JUMP_IF_FALSE");
                    OUT(t, "    { ObjString* _s = AS_STRING(POP(sp)); if (_s->length == 0) goto L_%04d; }\n", ip + offset);
                } else if (tv.type == TYPE_OBJECT) {
                    emit_comment(t, start_ip, "OP_POP_JUMP_IF_FALSE");
                    OUT(t, "    sp--; /* object always truthy, no jump*/\n");
                } else {
                    emit_comment(t, start_ip, "OP_POP_JUMP_IF_FALSE");
                    OUT(t, "    if (btl_compiled_is_falsey(POP(sp))) goto L_%04d;\n", ip + offset);
                }
                type_pop(&ts);
            }
            break;
        }
        case BTL_OP_JUMP_IF_TRUE: {
            uint16_t offset = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            if (reachable) {
                AbstractType jt_type = type_peek(&ts, 0);
                emit_comment(t, start_ip, "OP_JUMP_IF_TRUE");
                if (jt_type == TYPE_BOOL)
                    OUT(t, "    if (AS_BOOL(sp[-1])) goto L_%04d;\n", ip + offset);
                else if (jt_type == TYPE_INT)
                    OUT(t, "    if (AS_INT(sp[-1]) != 0) goto L_%04d;\n", ip + offset);
                else if (jt_type == TYPE_NUMBER)
                    OUT(t, "    if (AS_NUMBER(sp[-1]) != 0.0) goto L_%04d;\n", ip + offset);
                else
                    OUT(t, "    if (!btl_compiled_is_falsey(sp[-1])) goto L_%04d;\n", ip + offset);
            }
            break;
        }
        case BTL_OP_POP_JUMP_IF_TRUE: {
            uint16_t offset = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            if (reachable) {
                /* Check if condition is a known constant for DCE*/
                TrackedValue tv = type_peek_tv(&ts, 0);
                if (tv.isConstant && tv.type == TYPE_BOOL) {
                    bool isTruthy = tv.numValue != 0.0;  /* true = non-zero*/
                    emit_comment(t, start_ip, "OP_POP_JUMP_IF_TRUE (const-elim)");
                    OUT(t, "    sp--; /* pop const bool*/\n");
                    if (isTruthy) {
                        OUT(t, "    goto L_%04d; /* const true: always jump*/\n", ip + offset);
                        reachable = false;
                    }
                    /* else: const false, never jumps, fall through*/
                } else if (tv.type == TYPE_BOOL) {
                    emit_comment(t, start_ip, "OP_POP_JUMP_IF_TRUE");
                    OUT(t, "    if (AS_BOOL(POP(sp))) goto L_%04d;\n", ip + offset);
                } else if (tv.type == TYPE_INT) {
                    emit_comment(t, start_ip, "OP_POP_JUMP_IF_TRUE");
                    OUT(t, "    if (AS_INT(POP(sp)) != 0) goto L_%04d;\n", ip + offset);
                } else if (tv.type == TYPE_NUMBER) {
                    emit_comment(t, start_ip, "OP_POP_JUMP_IF_TRUE");
                    OUT(t, "    if (AS_NUMBER(POP(sp)) != 0.0) goto L_%04d;\n", ip + offset);
                } else if (tv.type == TYPE_NIL) {
                    emit_comment(t, start_ip, "OP_POP_JUMP_IF_TRUE");
                    OUT(t, "    sp--; /* nil is always falsey, no jump*/\n");
                } else if (tv.type == TYPE_STRING) {
                    emit_comment(t, start_ip, "OP_POP_JUMP_IF_TRUE");
                    OUT(t, "    { ObjString* _s = AS_STRING(POP(sp)); if (_s->length != 0) goto L_%04d; }\n", ip + offset);
                } else if (tv.type == TYPE_OBJECT) {
                    emit_comment(t, start_ip, "OP_POP_JUMP_IF_TRUE");
                    OUT(t, "    sp--; goto L_%04d; /* object always truthy*/\n", ip + offset);
                    reachable = false;
                } else {
                    emit_comment(t, start_ip, "OP_POP_JUMP_IF_TRUE");
                    OUT(t, "    if (!btl_compiled_is_falsey(POP(sp))) goto L_%04d;\n", ip + offset);
                }
                type_pop(&ts);
            }
            break;
        }
        case BTL_OP_JUMP_IF_NOT_EQUAL: {
            uint16_t offset = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            if (reachable) {
                AbstractType jne_b = type_peek(&ts, 0);
                AbstractType jne_a = type_peek(&ts, 1);
                emit_comment(t, start_ip, "OP_JUMP_IF_NOT_EQUAL");
                if (jne_a == TYPE_INT && jne_b == TYPE_INT)
                    OUT(t, "    { int64_t _b = AS_INT(POP(sp)); int64_t _a = AS_INT(POP(sp)); if (_a != _b) goto L_%04d; }\n", ip + offset);
                else if (jne_a == TYPE_NUMBER && jne_b == TYPE_NUMBER)
                    OUT(t, "    { double _b = AS_NUMBER(POP(sp)); double _a = AS_NUMBER(POP(sp)); if (_a != _b) goto L_%04d; }\n", ip + offset);
                else if (jne_a == TYPE_BOOL && jne_b == TYPE_BOOL)
                    OUT(t, "    { bool _b = AS_BOOL(POP(sp)); bool _a = AS_BOOL(POP(sp)); if (_a != _b) goto L_%04d; }\n", ip + offset);
                else if (jne_a == TYPE_NIL && jne_b == TYPE_NIL)
                    OUT(t, "    sp -= 2; /* nil!=nil always false, no jump */\n");
                else if (jne_a == TYPE_NIL)
                    OUT(t, "    { BtlValue _b = POP(sp); sp--; if (!btl_compiled_is_null_like(_b)) goto L_%04d; } /* nil!=? */\n", ip + offset);
                else if (jne_b == TYPE_NIL)
                    OUT(t, "    { sp--; BtlValue _a = POP(sp); if (!btl_compiled_is_null_like(_a)) goto L_%04d; } /* ?!=nil */\n", ip + offset);
                else if (jne_a == TYPE_STRING && jne_b == TYPE_STRING)
                    OUT(t, "    { BtlValue _b = POP(sp); BtlValue _a = POP(sp); if (_a != _b) goto L_%04d; } /* string!=string */\n", ip + offset);
                else if (jne_a == TYPE_INT)
                    OUT(t, "    { BtlValue _b = POP(sp); int64_t _a = AS_INT(POP(sp)); if (!(__builtin_expect(IS_INT(_b), 1) ? (_a == AS_INT(_b)) : btl_compiled_equal(INT_VAL(_a), _b))) goto L_%04d; }\n", ip + offset);
                else if (jne_b == TYPE_INT)
                    OUT(t, "    { int64_t _b = AS_INT(POP(sp)); BtlValue _a = POP(sp); if (!(__builtin_expect(IS_INT(_a), 1) ? (AS_INT(_a) == _b) : btl_compiled_equal(_a, INT_VAL(_b)))) goto L_%04d; }\n", ip + offset);
                else
                    OUT(t, "    { BtlValue _b = POP(sp); BtlValue _a = POP(sp); if (!btl_compiled_equal(_a, _b)) goto L_%04d; }\n", ip + offset);
                type_pop(&ts); type_pop(&ts);
            }
            break;
        }
        case BTL_OP_JUMP_IF_EQUAL: {
            uint16_t offset = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            if (reachable) {
                AbstractType je_b = type_peek(&ts, 0);
                AbstractType je_a = type_peek(&ts, 1);
                emit_comment(t, start_ip, "OP_JUMP_IF_EQUAL");
                if (je_a == TYPE_INT && je_b == TYPE_INT)
                    OUT(t, "    { int64_t _b = AS_INT(POP(sp)); int64_t _a = AS_INT(POP(sp)); if (_a == _b) goto L_%04d; }\n", ip + offset);
                else if (je_a == TYPE_NUMBER && je_b == TYPE_NUMBER)
                    OUT(t, "    { double _b = AS_NUMBER(POP(sp)); double _a = AS_NUMBER(POP(sp)); if (_a == _b) goto L_%04d; }\n", ip + offset);
                else if (je_a == TYPE_BOOL && je_b == TYPE_BOOL)
                    OUT(t, "    { bool _b = AS_BOOL(POP(sp)); bool _a = AS_BOOL(POP(sp)); if (_a == _b) goto L_%04d; }\n", ip + offset);
                else if (je_a == TYPE_NIL && je_b == TYPE_NIL)
                    OUT(t, "    sp -= 2; goto L_%04d; /* nil==nil always true */\n", ip + offset);
                else if (je_a == TYPE_NIL)
                    OUT(t, "    { BtlValue _b = POP(sp); sp--; if (btl_compiled_is_null_like(_b)) goto L_%04d; } /* nil==? */\n", ip + offset);
                else if (je_b == TYPE_NIL)
                    OUT(t, "    { sp--; BtlValue _a = POP(sp); if (btl_compiled_is_null_like(_a)) goto L_%04d; } /* ?==nil */\n", ip + offset);
                else if (je_a == TYPE_STRING && je_b == TYPE_STRING)
                    OUT(t, "    { BtlValue _b = POP(sp); BtlValue _a = POP(sp); if (_a == _b) goto L_%04d; } /* string==string */\n", ip + offset);
                else if (je_a == TYPE_INT)
                    OUT(t, "    { BtlValue _b = POP(sp); int64_t _a = AS_INT(POP(sp)); if (__builtin_expect(IS_INT(_b), 1) ? (_a == AS_INT(_b)) : btl_compiled_equal(INT_VAL(_a), _b)) goto L_%04d; }\n", ip + offset);
                else if (je_b == TYPE_INT)
                    OUT(t, "    { int64_t _b = AS_INT(POP(sp)); BtlValue _a = POP(sp); if (__builtin_expect(IS_INT(_a), 1) ? (AS_INT(_a) == _b) : btl_compiled_equal(_a, INT_VAL(_b))) goto L_%04d; }\n", ip + offset);
                else
                    OUT(t, "    { BtlValue _b = POP(sp); BtlValue _a = POP(sp); if (btl_compiled_equal(_a, _b)) goto L_%04d; }\n", ip + offset);
                type_pop(&ts); type_pop(&ts);
            }
            break;
        }
        case BTL_OP_JUMP_IF_NOT_GREATER: {
            uint16_t offset = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            emit_comment(t, start_ip, "OP_JUMP_IF_NOT_GREATER");
            {
                TrackedValue tvb_jg = type_peek_tv(&ts, 0);
                TrackedValue tva_jg = type_peek_tv(&ts, 1);
                type_pop_tv(&ts); type_pop_tv(&ts);
                if (tva_jg.type == TYPE_INT && tvb_jg.type == TYPE_INT) {
                    OUT(t, "    { int64_t _b = AS_INT(POP(sp)); int64_t _a = AS_INT(POP(sp)); if (!(_a > _b)) goto L_%04d; }\n", ip + offset);
                } else if (tva_jg.type == TYPE_NUMBER && tvb_jg.type == TYPE_NUMBER) {
                    OUT(t, "    { double _b = AS_NUMBER(POP(sp)); double _a = AS_NUMBER(POP(sp)); if (!(_a > _b)) goto L_%04d; }\n", ip + offset);
                } else if (tva_jg.type == TYPE_INT) {
                    OUT(t, "    { BtlValue _b = POP(sp); int64_t _a = AS_INT(POP(sp));\n");
                    OUT(t, "      if (__builtin_expect(IS_INT(_b), 1)) { if (!(_a > AS_INT(_b))) goto L_%04d; }\n", ip + offset);
                    OUT(t, "      else if (__builtin_expect(IS_NUMBER(_b), 1)) { if (!((double)_a > AS_NUMBER(_b))) goto L_%04d; }\n", ip + offset);
                    OUT(t, "      else return btl_error_not_numbers(vm, sp); }\n");
                } else if (tvb_jg.type == TYPE_INT) {
                    OUT(t, "    { int64_t _b = AS_INT(POP(sp)); BtlValue _a = POP(sp);\n");
                    OUT(t, "      if (__builtin_expect(IS_INT(_a), 1)) { if (!(AS_INT(_a) > _b)) goto L_%04d; }\n", ip + offset);
                    OUT(t, "      else if (__builtin_expect(IS_NUMBER(_a), 1)) { if (!(AS_NUMBER(_a) > (double)_b)) goto L_%04d; }\n", ip + offset);
                    OUT(t, "      else return btl_error_not_numbers(vm, sp); }\n");
                } else {
                    OUT(t, "    { BtlValue _vb = POP(sp), _va = POP(sp);\n");
                    OUT(t, "      if (__builtin_expect(IS_NUMERIC(_va) & IS_NUMERIC(_vb), 1)) {\n");
                    OUT(t, "        if (!(btl_numeric_to_double(_va) > btl_numeric_to_double(_vb))) goto L_%04d;\n", ip + offset);
                    OUT(t, "      } else return btl_error_not_numbers(vm, sp); }\n");
                }
            }
            break;
        }
        case BTL_OP_JUMP_IF_NOT_LESS: {
            uint16_t offset = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            emit_comment(t, start_ip, "OP_JUMP_IF_NOT_LESS");
            {
                TrackedValue tvb_jl = type_peek_tv(&ts, 0);
                TrackedValue tva_jl = type_peek_tv(&ts, 1);
                type_pop_tv(&ts); type_pop_tv(&ts);
                if (tva_jl.type == TYPE_INT && tvb_jl.type == TYPE_INT) {
                    OUT(t, "    { int64_t _b = AS_INT(POP(sp)); int64_t _a = AS_INT(POP(sp)); if (!(_a < _b)) goto L_%04d; }\n", ip + offset);
                } else if (tva_jl.type == TYPE_NUMBER && tvb_jl.type == TYPE_NUMBER) {
                    OUT(t, "    { double _b = AS_NUMBER(POP(sp)); double _a = AS_NUMBER(POP(sp)); if (!(_a < _b)) goto L_%04d; }\n", ip + offset);
                } else if (tva_jl.type == TYPE_INT) {
                    OUT(t, "    { BtlValue _b = POP(sp); int64_t _a = AS_INT(POP(sp));\n");
                    OUT(t, "      if (__builtin_expect(IS_INT(_b), 1)) { if (!(_a < AS_INT(_b))) goto L_%04d; }\n", ip + offset);
                    OUT(t, "      else if (__builtin_expect(IS_NUMBER(_b), 1)) { if (!((double)_a < AS_NUMBER(_b))) goto L_%04d; }\n", ip + offset);
                    OUT(t, "      else return btl_error_not_numbers(vm, sp); }\n");
                } else if (tvb_jl.type == TYPE_INT) {
                    OUT(t, "    { int64_t _b = AS_INT(POP(sp)); BtlValue _a = POP(sp);\n");
                    OUT(t, "      if (__builtin_expect(IS_INT(_a), 1)) { if (!(AS_INT(_a) < _b)) goto L_%04d; }\n", ip + offset);
                    OUT(t, "      else if (__builtin_expect(IS_NUMBER(_a), 1)) { if (!(AS_NUMBER(_a) < (double)_b)) goto L_%04d; }\n", ip + offset);
                    OUT(t, "      else return btl_error_not_numbers(vm, sp); }\n");
                } else {
                    OUT(t, "    { BtlValue _vb = POP(sp), _va = POP(sp);\n");
                    OUT(t, "      if (__builtin_expect(IS_NUMERIC(_va) & IS_NUMERIC(_vb), 1)) {\n");
                    OUT(t, "        if (!(btl_numeric_to_double(_va) < btl_numeric_to_double(_vb))) goto L_%04d;\n", ip + offset);
                    OUT(t, "      } else return btl_error_not_numbers(vm, sp); }\n");
                }
            }
            break;
        }
        case BTL_OP_LOOP: {
            uint16_t offset = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            if (reachable) {
                emit_comment(t, start_ip, "OP_LOOP");
                OUT(t, "    goto L_%04d;\n", ip - offset);
            }
            reachable = false; /* Code after loop is dead (until next label)*/
            break;
        }

                    // ================================================================
                    // CALLS Ã¢â‚¬â€ the big performance win
                    //
                    // Instead of btl_call_value() + run() (which re-enters the interpreter),
                    // we:
                    //   1) Sync sp to vm->stackTop
                    //   2) Set up the CallFrame ourselves
                    //   3) Call btl_fn_N(vm) directly (known target) or
                    //      callValue+run (unknown/native target)
                    //   4) Reload sp from vm->stackTop
                    // ================================================================
        case BTL_OP_CALL_0: case BTL_OP_CALL_1: case BTL_OP_CALL_2: case BTL_OP_CALL_3:
        case BTL_OP_CALL_4: case BTL_OP_CALL_5: case BTL_OP_CALL_6: case BTL_OP_CALL_7:
        case BTL_OP_CALL_8: {
            int argc = op - BTL_OP_CALL_0;
            emit_call_bracket_open(t);
            if (callee_is_local_0(code, start_ip, argc, targets)) {
                emit_comment(t, start_ip, "OP_CALL_N (self-recursive)");
                emit_self_recursive_call(t, argc);
            } else {
                emit_comment(t, start_ip, "OP_CALL_N");
                emit_optimized_call(t, argc);
            }
            emit_call_bracket_close(t);
            /* Type tracking: pop callee + args, push result */
            for (int i = 0; i < argc + 1; i++) type_pop(&ts);
            type_push(&ts, TYPE_UNKNOWN);
            break;
        }
        case BTL_OP_CALL: {
            uint8_t argc = code[ip++];
            emit_comment(t, start_ip, "OP_CALL");
            emit_call_bracket_open(t);
            emit_optimized_call(t, argc);
            emit_call_bracket_close(t);
            /* Type tracking: pop callee + args, push result */
            for (int i = 0; i < argc + 1; i++) type_pop(&ts);
            type_push(&ts, TYPE_UNKNOWN);
            break;
        }

                    /* Tail calls -- self-recursive optimization*/
        case BTL_OP_TAIL_CALL_0: case BTL_OP_TAIL_CALL_1: case BTL_OP_TAIL_CALL_2:
        case BTL_OP_TAIL_CALL_3: case BTL_OP_TAIL_CALL_4: case BTL_OP_TAIL_CALL_5:
        case BTL_OP_TAIL_CALL_6: case BTL_OP_TAIL_CALL_7: case BTL_OP_TAIL_CALL_8: {
            int argc = op - BTL_OP_TAIL_CALL_0;
            emit_comment(t, start_ip, "OP_TAIL_CALL_N (tail-opt)");
            emit_call_bracket_open(t);
            emit_tail_call(t, argc);
            emit_call_bracket_close(t);
            break;
        }
        case BTL_OP_TAIL_CALL: {
            uint8_t argc = code[ip++];
            emit_comment(t, start_ip, "OP_TAIL_CALL (tail-opt)");
            emit_call_bracket_open(t);
            emit_tail_call(t, argc);
            emit_call_bracket_close(t);
            break;
        }

                         // ================================================================
                         // INVOKE (indexed)
                         // ================================================================
        case BTL_OP_INVOKE_0: case BTL_OP_INVOKE_1: case BTL_OP_INVOKE_2: case BTL_OP_INVOKE_3:
        case BTL_OP_INVOKE_4: case BTL_OP_INVOKE_5: case BTL_OP_INVOKE_6: case BTL_OP_INVOKE_7:
        case BTL_OP_INVOKE_8: {
            int argc = op - BTL_OP_INVOKE_0;
            uint8_t methodIdx = code[ip++];
            emit_comment(t, start_ip, "OP_INVOKE_N");
            /* Inline fast path for instance method dispatch */
            OUT(t, "    { BtlValue _recv = sp[-%d-1];\n", argc);
            OUT(t, "      if (__builtin_expect(IS_INSTANCE(_recv), 1)) {\n");
            OUT(t, "        ObjInstance* _inst = AS_INSTANCE(_recv);\n");
            OUT(t, "        if (__builtin_expect(%d < _inst->klass->methodCount, 1)) {\n", methodIdx);
            OUT(t, "          BtlMethodEntry* _me = &_inst->klass->methods[%d];\n", methodIdx);
            OUT(t, "          if (__builtin_expect(_me->closure != NULL && %d == _me->arity, 1)) {\n", argc);
            OUT(t, "            vm->stackTop = sp;\n");
            OUT(t, "            if (__builtin_expect(vm->frameCount >= vm->frameCapacity, 0)) {\n");
            OUT(t, "              if (!btl_ensure_frame_capacity(vm)) return BTL_INTERPRET_RUNTIME_ERROR;\n");
            OUT(t, "            }\n");
            OUT(t, "            { BtlCallFrame* _nf = &vm->frames[vm->frameCount++];\n");
            OUT(t, "            _nf->closure = _me->closure;\n");
            OUT(t, "            _nf->ip = _me->closure->function->chunk.code;\n");
            OUT(t, "            _nf->slots = vm->stackTop - %d;\n", argc + 1);
            OUT(t, "            _nf->openUpvalues = NULL;\n");
            OUT(t, "            BtlFnPtr _h = (BtlFnPtr)_me->closure->function->compiledHandler;\n");
            OUT(t, "            if (_h) {\n");
            OUT(t, "              BtlInterpretResult _r = _h(vm);\n");
            OUT(t, "              if (_r != BTL_INTERPRET_OK) return _r;\n");
            OUT(t, "            } else {\n");
            OUT(t, "              int _sf = vm->runFloor; vm->runFloor = vm->frameCount - 1;\n");
            OUT(t, "              BtlInterpretResult _r = btl_run(vm);\n");
            OUT(t, "              vm->runFloor = _sf;\n");
            OUT(t, "              if (_r != BTL_INTERPRET_OK) return _r;\n");
            OUT(t, "            } }\n");
            emit_reload(t);
            OUT(t, "            goto L_invoke_%d_done;\n", start_ip);
            OUT(t, "          }\n");
            OUT(t, "        }\n");
            OUT(t, "      }\n");
            OUT(t, "      /* Slow path: non-instance or method mismatch */\n");
            OUT(t, "      vm->stackTop = sp;\n");
            OUT(t, "      if (!btl_compiled_invoke_indexed(vm, %d, %d)) return BTL_INTERPRET_RUNTIME_ERROR;\n", methodIdx, argc);
            emit_reload(t);
            OUT(t, "    L_invoke_%d_done:; }\n", start_ip);
            /* Type tracking: pop receiver + args, push unknown result */
            for (int i = 0; i < argc + 1; i++) type_pop(&ts);
            type_push(&ts, TYPE_UNKNOWN);
            break;
        }
        case BTL_OP_INVOKE: {
            uint8_t methodIdx = code[ip++];
            uint8_t argc = code[ip++];
            emit_comment(t, start_ip, "OP_INVOKE");
            /* Inline fast path for instance method dispatch */
            OUT(t, "    { BtlValue _recv = sp[-%d-1];\n", argc);
            OUT(t, "      if (__builtin_expect(IS_INSTANCE(_recv), 1)) {\n");
            OUT(t, "        ObjInstance* _inst = AS_INSTANCE(_recv);\n");
            OUT(t, "        if (__builtin_expect(%d < _inst->klass->methodCount, 1)) {\n", methodIdx);
            OUT(t, "          BtlMethodEntry* _me = &_inst->klass->methods[%d];\n", methodIdx);
            OUT(t, "          if (__builtin_expect(_me->closure != NULL && %d == _me->arity, 1)) {\n", argc);
            OUT(t, "            vm->stackTop = sp;\n");
            OUT(t, "            if (__builtin_expect(vm->frameCount >= vm->frameCapacity, 0)) {\n");
            OUT(t, "              if (!btl_ensure_frame_capacity(vm)) return BTL_INTERPRET_RUNTIME_ERROR;\n");
            OUT(t, "            }\n");
            OUT(t, "            { BtlCallFrame* _nf = &vm->frames[vm->frameCount++];\n");
            OUT(t, "            _nf->closure = _me->closure;\n");
            OUT(t, "            _nf->ip = _me->closure->function->chunk.code;\n");
            OUT(t, "            _nf->slots = vm->stackTop - %d;\n", argc + 1);
            OUT(t, "            _nf->openUpvalues = NULL;\n");
            OUT(t, "            BtlFnPtr _h = (BtlFnPtr)_me->closure->function->compiledHandler;\n");
            OUT(t, "            if (_h) {\n");
            OUT(t, "              BtlInterpretResult _r = _h(vm);\n");
            OUT(t, "              if (_r != BTL_INTERPRET_OK) return _r;\n");
            OUT(t, "            } else {\n");
            OUT(t, "              int _sf = vm->runFloor; vm->runFloor = vm->frameCount - 1;\n");
            OUT(t, "              BtlInterpretResult _r = btl_run(vm);\n");
            OUT(t, "              vm->runFloor = _sf;\n");
            OUT(t, "              if (_r != BTL_INTERPRET_OK) return _r;\n");
            OUT(t, "            } }\n");
            emit_reload(t);
            OUT(t, "            goto L_invoke_%d_done;\n", start_ip);
            OUT(t, "          }\n");
            OUT(t, "        }\n");
            OUT(t, "      }\n");
            OUT(t, "      /* Slow path: non-instance or method mismatch */\n");
            OUT(t, "      vm->stackTop = sp;\n");
            OUT(t, "      if (!btl_compiled_invoke_indexed(vm, %d, %d)) return BTL_INTERPRET_RUNTIME_ERROR;\n", methodIdx, argc);
            emit_reload(t);
            OUT(t, "    L_invoke_%d_done:; }\n", start_ip);
            /* Type tracking: pop receiver + args, push unknown result */
            for (int i = 0; i < argc + 1; i++) type_pop(&ts);
            type_push(&ts, TYPE_UNKNOWN);
            break;
        }
        case BTL_OP_INVOKE_LONG: {
            uint16_t methodIdx = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            uint8_t argc = code[ip++];
            emit_comment(t, start_ip, "OP_INVOKE_LONG");
            /* Inline fast path for instance method dispatch */
            OUT(t, "    { BtlValue _recv = sp[-%d-1];\n", argc);
            OUT(t, "      if (__builtin_expect(IS_INSTANCE(_recv), 1)) {\n");
            OUT(t, "        ObjInstance* _inst = AS_INSTANCE(_recv);\n");
            OUT(t, "        if (__builtin_expect(%d < _inst->klass->methodCount, 1)) {\n", methodIdx);
            OUT(t, "          BtlMethodEntry* _me = &_inst->klass->methods[%d];\n", methodIdx);
            OUT(t, "          if (__builtin_expect(_me->closure != NULL && %d == _me->arity, 1)) {\n", argc);
            OUT(t, "            vm->stackTop = sp;\n");
            OUT(t, "            if (__builtin_expect(vm->frameCount >= vm->frameCapacity, 0)) {\n");
            OUT(t, "              if (!btl_ensure_frame_capacity(vm)) return BTL_INTERPRET_RUNTIME_ERROR;\n");
            OUT(t, "            }\n");
            OUT(t, "            { BtlCallFrame* _nf = &vm->frames[vm->frameCount++];\n");
            OUT(t, "            _nf->closure = _me->closure;\n");
            OUT(t, "            _nf->ip = _me->closure->function->chunk.code;\n");
            OUT(t, "            _nf->slots = vm->stackTop - %d;\n", argc + 1);
            OUT(t, "            _nf->openUpvalues = NULL;\n");
            OUT(t, "            BtlFnPtr _h = (BtlFnPtr)_me->closure->function->compiledHandler;\n");
            OUT(t, "            if (_h) {\n");
            OUT(t, "              BtlInterpretResult _r = _h(vm);\n");
            OUT(t, "              if (_r != BTL_INTERPRET_OK) return _r;\n");
            OUT(t, "            } else {\n");
            OUT(t, "              int _sf = vm->runFloor; vm->runFloor = vm->frameCount - 1;\n");
            OUT(t, "              BtlInterpretResult _r = btl_run(vm);\n");
            OUT(t, "              vm->runFloor = _sf;\n");
            OUT(t, "              if (_r != BTL_INTERPRET_OK) return _r;\n");
            OUT(t, "            } }\n");
            emit_reload(t);
            OUT(t, "            goto L_invoke_%d_done;\n", start_ip);
            OUT(t, "          }\n");
            OUT(t, "        }\n");
            OUT(t, "      }\n");
            OUT(t, "      /* Slow path: non-instance or method mismatch */\n");
            OUT(t, "      vm->stackTop = sp;\n");
            OUT(t, "      if (!btl_compiled_invoke_indexed(vm, %d, %d)) return BTL_INTERPRET_RUNTIME_ERROR;\n", methodIdx, argc);
            emit_reload(t);
            OUT(t, "    L_invoke_%d_done:; }\n", start_ip);
            /* Type tracking: pop receiver + args, push unknown result */
            for (int i = 0; i < argc + 1; i++) type_pop(&ts);
            type_push(&ts, TYPE_UNKNOWN);
            break;
        }
        case BTL_OP_INVOKE_IC: {
            uint8_t nameIdx = code[ip++];
            uint8_t argc = code[ip++];
            uint8_t icSlot = code[ip++];
            emit_comment(t, start_ip, "OP_INVOKE_IC");

            /* Get method name for native method specialization */
            ObjString* methodName = AS_STRING(fn->chunk.constants.values[nameIdx]);

            OUT(t, "    { BtlValue _recv = sp[-%d-1];\n", argc);

            /* Inline fast paths for common native methods */
            if (argc == 0 && strcmp(methodName->chars, "length") == 0) {
                /* list.length() / string.length() / table.length() - ultra fast inline */
                OUT(t, "      if (__builtin_expect(IS_LIST(_recv), 1)) {\n");
                OUT(t, "        sp[-1] = INT_VAL(AS_LIST(_recv)->items.count);\n");
                OUT(t, "        goto L_invoke_ic_%d_done;\n", start_ip);
                OUT(t, "      }\n");
                OUT(t, "      if (IS_STRING(_recv)) {\n");
                OUT(t, "        sp[-1] = INT_VAL(AS_STRING(_recv)->length);\n");
                OUT(t, "        goto L_invoke_ic_%d_done;\n", start_ip);
                OUT(t, "      }\n");
                OUT(t, "      if (IS_TABLE(_recv)) {\n");
                OUT(t, "        sp[-1] = INT_VAL(AS_TABLE(_recv)->table.count);\n");
                OUT(t, "        goto L_invoke_ic_%d_done;\n", start_ip);
                OUT(t, "      }\n");
            } else if (argc == 1 && strcmp(methodName->chars, "push") == 0) {
                /* list.push(value) - inline with GC write barrier */
                OUT(t, "      if (__builtin_expect(IS_LIST(_recv), 1)) {\n");
                OUT(t, "        ObjList* _l = AS_LIST(_recv);\n");
                OUT(t, "        BtlValue _v = sp[-1];\n");
                OUT(t, "        vm->stackTop = sp;\n");
                OUT(t, "        btl_value_array_write(vm, &_l->items, _v);\n");
                OUT(t, "        if (IS_OBJ(_v)) btl_gc_write_barrier(vm, (BtlObj*)_l, _v);\n");
                OUT(t, "        sp = vm->stackTop;\n");
                OUT(t, "        sp[-2] = _recv; sp--;\n");
                OUT(t, "        goto L_invoke_ic_%d_done;\n", start_ip);
                OUT(t, "      }\n");
            } else if (argc == 0 && strcmp(methodName->chars, "pop") == 0) {
                /* list.pop() - inline */
                OUT(t, "      if (__builtin_expect(IS_LIST(_recv), 1)) {\n");
                OUT(t, "        ObjList* _l = AS_LIST(_recv);\n");
                OUT(t, "        if (__builtin_expect(_l->items.count > 0, 1)) {\n");
                OUT(t, "          sp[-1] = _l->items.values[--_l->items.count];\n");
                OUT(t, "          goto L_invoke_ic_%d_done;\n", start_ip);
                OUT(t, "        }\n");
                OUT(t, "      }\n");
            } else if (argc == 1 && strcmp(methodName->chars, "has") == 0) {
                /* table.has(key) - inline */
                OUT(t, "      if (__builtin_expect(IS_TABLE(_recv), 1)) {\n");
                OUT(t, "        ObjTable* _t = AS_TABLE(_recv);\n");
                OUT(t, "        BtlValue _k = sp[-1], _dummy;\n");
                OUT(t, "        bool _found = btl_table_get(&_t->table, _k, &_dummy);\n");
                OUT(t, "        sp[-2] = BOOL_VAL(_found); sp--;\n");
                OUT(t, "        goto L_invoke_ic_%d_done;\n", start_ip);
                OUT(t, "      }\n");
            } else if (argc == 0 && strcmp(methodName->chars, "toString") == 0) {
                /* int.toString() - inline */
                OUT(t, "      if (__builtin_expect(IS_INT(_recv), 1)) {\n");
                OUT(t, "        char _buf[32];\n");
                OUT(t, "        int _len = snprintf(_buf, 32, \"%%\" PRId64, AS_INT(_recv));\n");
                OUT(t, "        vm->stackTop = sp;\n");
                OUT(t, "        ObjString* _s = btl_string_copy(vm, _buf, _len);\n");
                OUT(t, "        sp = vm->stackTop;\n");
                OUT(t, "        sp[-1] = OBJ_VAL(_s);\n");
                OUT(t, "        goto L_invoke_ic_%d_done;\n", start_ip);
                OUT(t, "      }\n");
                /* number.toString() - inline */
                OUT(t, "      if (__builtin_expect(IS_NUMBER(_recv), 1)) {\n");
                OUT(t, "        char _buf[32];\n");
                OUT(t, "        int _len = snprintf(_buf, 32, \"%%g\", AS_NUMBER(_recv));\n");
                OUT(t, "        vm->stackTop = sp;\n");
                OUT(t, "        ObjString* _s = btl_string_copy(vm, _buf, _len);\n");
                OUT(t, "        sp = vm->stackTop;\n");
                OUT(t, "        sp[-1] = OBJ_VAL(_s);\n");
                OUT(t, "        goto L_invoke_ic_%d_done;\n", start_ip);
                OUT(t, "      }\n");
            } else if (argc == 0 && strcmp(methodName->chars, "abs") == 0) {
                /* int.abs() - inline */
                OUT(t, "      if (__builtin_expect(IS_INT(_recv), 1)) {\n");
                OUT(t, "        int64_t _n = AS_INT(_recv);\n");
                OUT(t, "        sp[-1] = INT_VAL(_n < 0 ? -_n : _n);\n");
                OUT(t, "        goto L_invoke_ic_%d_done;\n", start_ip);
                OUT(t, "      }\n");
                /* number.abs() - inline */
                OUT(t, "      if (__builtin_expect(IS_NUMBER(_recv), 1)) {\n");
                OUT(t, "        sp[-1] = NUMBER_VAL(fabs(AS_NUMBER(_recv)));\n");
                OUT(t, "        goto L_invoke_ic_%d_done;\n", start_ip);
                OUT(t, "      }\n");
            } else if (argc == 0 && strcmp(methodName->chars, "sign") == 0) {
                /* int.sign() - inline */
                OUT(t, "      if (__builtin_expect(IS_INT(_recv), 1)) {\n");
                OUT(t, "        int64_t _n = AS_INT(_recv);\n");
                OUT(t, "        sp[-1] = INT_VAL((_n > 0) - (_n < 0));\n");
                OUT(t, "        goto L_invoke_ic_%d_done;\n", start_ip);
                OUT(t, "      }\n");
            } else if (argc == 0 && strcmp(methodName->chars, "isEven") == 0) {
                /* int.isEven() - inline */
                OUT(t, "      if (__builtin_expect(IS_INT(_recv), 1)) {\n");
                OUT(t, "        sp[-1] = BOOL_VAL(!(AS_INT(_recv) & 1));\n");
                OUT(t, "        goto L_invoke_ic_%d_done;\n", start_ip);
                OUT(t, "      }\n");
            } else if (argc == 0 && strcmp(methodName->chars, "isOdd") == 0) {
                /* int.isOdd() - inline */
                OUT(t, "      if (__builtin_expect(IS_INT(_recv), 1)) {\n");
                OUT(t, "        sp[-1] = BOOL_VAL(AS_INT(_recv) & 1);\n");
                OUT(t, "        goto L_invoke_ic_%d_done;\n", start_ip);
                OUT(t, "      }\n");
            } else if (argc == 0 && strcmp(methodName->chars, "isZero") == 0) {
                /* int.isZero() - inline */
                OUT(t, "      if (__builtin_expect(IS_INT(_recv), 1)) {\n");
                OUT(t, "        sp[-1] = BOOL_VAL(AS_INT(_recv) == 0);\n");
                OUT(t, "        goto L_invoke_ic_%d_done;\n", start_ip);
                OUT(t, "      }\n");
            } else if (argc == 0 && strcmp(methodName->chars, "isPositive") == 0) {
                /* int.isPositive() - inline */
                OUT(t, "      if (__builtin_expect(IS_INT(_recv), 1)) {\n");
                OUT(t, "        sp[-1] = BOOL_VAL(AS_INT(_recv) > 0);\n");
                OUT(t, "        goto L_invoke_ic_%d_done;\n", start_ip);
                OUT(t, "      }\n");
            } else if (argc == 0 && strcmp(methodName->chars, "isNegative") == 0) {
                /* int.isNegative() - inline */
                OUT(t, "      if (__builtin_expect(IS_INT(_recv), 1)) {\n");
                OUT(t, "        sp[-1] = BOOL_VAL(AS_INT(_recv) < 0);\n");
                OUT(t, "        goto L_invoke_ic_%d_done;\n", start_ip);
                OUT(t, "      }\n");
            } else if (argc == 0 && strcmp(methodName->chars, "bitNot") == 0) {
                /* int.bitNot() - inline */
                OUT(t, "      if (__builtin_expect(IS_INT(_recv), 1)) {\n");
                OUT(t, "        sp[-1] = INT_VAL(~AS_INT(_recv));\n");
                OUT(t, "        goto L_invoke_ic_%d_done;\n", start_ip);
                OUT(t, "      }\n");
            } else if (argc == 0 && strcmp(methodName->chars, "toFloat") == 0) {
                /* int.toFloat() - inline */
                OUT(t, "      if (__builtin_expect(IS_INT(_recv), 1)) {\n");
                OUT(t, "        sp[-1] = NUMBER_VAL((double)AS_INT(_recv));\n");
                OUT(t, "        goto L_invoke_ic_%d_done;\n", start_ip);
                OUT(t, "      }\n");
            } else if (argc == 0 && strcmp(methodName->chars, "floor") == 0) {
                /* number.floor() - inline */
                OUT(t, "      if (__builtin_expect(IS_NUMBER(_recv), 1)) {\n");
                OUT(t, "        sp[-1] = INT_VAL((int64_t)floor(AS_NUMBER(_recv)));\n");
                OUT(t, "        goto L_invoke_ic_%d_done;\n", start_ip);
                OUT(t, "      }\n");
            } else if (argc == 0 && strcmp(methodName->chars, "ceil") == 0) {
                /* number.ceil() - inline */
                OUT(t, "      if (__builtin_expect(IS_NUMBER(_recv), 1)) {\n");
                OUT(t, "        sp[-1] = INT_VAL((int64_t)ceil(AS_NUMBER(_recv)));\n");
                OUT(t, "        goto L_invoke_ic_%d_done;\n", start_ip);
                OUT(t, "      }\n");
            /* --- Tier 2: One-arg bitwise int methods --- */
            } else if (argc == 1 && strcmp(methodName->chars, "bitAnd") == 0) {
                /* int.bitAnd(n) - inline */
                OUT(t, "      if (__builtin_expect(IS_INT(_recv) & IS_INT(sp[-1]), 1)) {\n");
                OUT(t, "        sp[-2] = INT_VAL(AS_INT(_recv) & AS_INT(sp[-1])); sp--;\n");
                OUT(t, "        goto L_invoke_ic_%d_done;\n", start_ip);
                OUT(t, "      }\n");
            } else if (argc == 1 && strcmp(methodName->chars, "bitOr") == 0) {
                /* int.bitOr(n) - inline */
                OUT(t, "      if (__builtin_expect(IS_INT(_recv) & IS_INT(sp[-1]), 1)) {\n");
                OUT(t, "        sp[-2] = INT_VAL(AS_INT(_recv) | AS_INT(sp[-1])); sp--;\n");
                OUT(t, "        goto L_invoke_ic_%d_done;\n", start_ip);
                OUT(t, "      }\n");
            } else if (argc == 1 && strcmp(methodName->chars, "bitXor") == 0) {
                /* int.bitXor(n) - inline */
                OUT(t, "      if (__builtin_expect(IS_INT(_recv) & IS_INT(sp[-1]), 1)) {\n");
                OUT(t, "        sp[-2] = INT_VAL(AS_INT(_recv) ^ AS_INT(sp[-1])); sp--;\n");
                OUT(t, "        goto L_invoke_ic_%d_done;\n", start_ip);
                OUT(t, "      }\n");
            } else if (argc == 1 && strcmp(methodName->chars, "leftShift") == 0) {
                /* int.leftShift(n) - inline with range check */
                OUT(t, "      if (__builtin_expect(IS_INT(_recv) & IS_INT(sp[-1]), 1)) {\n");
                OUT(t, "        int64_t _sh = AS_INT(sp[-1]);\n");
                OUT(t, "        if (__builtin_expect((uint64_t)_sh < 48, 1)) {\n");
                OUT(t, "          sp[-2] = INT_VAL(AS_INT(_recv) << _sh); sp--;\n");
                OUT(t, "          goto L_invoke_ic_%d_done;\n", start_ip);
                OUT(t, "        }\n");
                OUT(t, "      }\n");
            } else if (argc == 1 && strcmp(methodName->chars, "rightShift") == 0) {
                /* int.rightShift(n) - inline with range check */
                OUT(t, "      if (__builtin_expect(IS_INT(_recv) & IS_INT(sp[-1]), 1)) {\n");
                OUT(t, "        int64_t _sh = AS_INT(sp[-1]);\n");
                OUT(t, "        if (__builtin_expect((uint64_t)_sh < 48, 1)) {\n");
                OUT(t, "          sp[-2] = INT_VAL(AS_INT(_recv) >> _sh); sp--;\n");
                OUT(t, "          goto L_invoke_ic_%d_done;\n", start_ip);
                OUT(t, "        }\n");
                OUT(t, "      }\n");
            /* --- Tier 3: One-arg math int methods --- */
            } else if (argc == 1 && strcmp(methodName->chars, "min") == 0) {
                /* int.min(n) - inline */
                OUT(t, "      if (__builtin_expect(IS_INT(_recv) & IS_INT(sp[-1]), 1)) {\n");
                OUT(t, "        int64_t _a = AS_INT(_recv), _b = AS_INT(sp[-1]);\n");
                OUT(t, "        sp[-2] = INT_VAL(_a < _b ? _a : _b); sp--;\n");
                OUT(t, "        goto L_invoke_ic_%d_done;\n", start_ip);
                OUT(t, "      }\n");
            } else if (argc == 1 && strcmp(methodName->chars, "max") == 0) {
                /* int.max(n) - inline */
                OUT(t, "      if (__builtin_expect(IS_INT(_recv) & IS_INT(sp[-1]), 1)) {\n");
                OUT(t, "        int64_t _a = AS_INT(_recv), _b = AS_INT(sp[-1]);\n");
                OUT(t, "        sp[-2] = INT_VAL(_a > _b ? _a : _b); sp--;\n");
                OUT(t, "        goto L_invoke_ic_%d_done;\n", start_ip);
                OUT(t, "      }\n");
            } else if (argc == 1 && strcmp(methodName->chars, "mod") == 0) {
                /* int.mod(n) - inline with div-by-zero guard */
                OUT(t, "      if (__builtin_expect(IS_INT(_recv) & IS_INT(sp[-1]), 1)) {\n");
                OUT(t, "        int64_t _b = AS_INT(sp[-1]);\n");
                OUT(t, "        if (__builtin_expect(_b != 0, 1)) {\n");
                OUT(t, "          int64_t _r = AS_INT(_recv) %% _b;\n");
                OUT(t, "          if (_r < 0) _r += (_b < 0 ? -_b : _b);\n");
                OUT(t, "          sp[-2] = INT_VAL(_r); sp--;\n");
                OUT(t, "          goto L_invoke_ic_%d_done;\n", start_ip);
                OUT(t, "        }\n");
                OUT(t, "      }\n");
            } else if (argc == 1 && strcmp(methodName->chars, "gcd") == 0) {
                /* int.gcd(n) - inline Euclidean algorithm */
                OUT(t, "      if (__builtin_expect(IS_INT(_recv) & IS_INT(sp[-1]), 1)) {\n");
                OUT(t, "        int64_t _a = AS_INT(_recv), _b = AS_INT(sp[-1]);\n");
                OUT(t, "        if (_a < 0) _a = -_a; if (_b < 0) _b = -_b;\n");
                OUT(t, "        while (_b) { int64_t _t = _b; _b = _a %% _b; _a = _t; }\n");
                OUT(t, "        sp[-2] = INT_VAL(_a); sp--;\n");
                OUT(t, "        goto L_invoke_ic_%d_done;\n", start_ip);
                OUT(t, "      }\n");
            } else if (argc == 1 && strcmp(methodName->chars, "pow") == 0) {
                /* int.pow(n) - inline for non-negative int exponents */
                OUT(t, "      if (__builtin_expect(IS_INT(_recv) & IS_INT(sp[-1]), 1)) {\n");
                OUT(t, "        int64_t _exp = AS_INT(sp[-1]);\n");
                OUT(t, "        if (__builtin_expect(_exp >= 0, 1)) {\n");
                OUT(t, "          int64_t _base = AS_INT(_recv), _r = 1;\n");
                OUT(t, "          for (int64_t _i = 0; _i < _exp; _i++) _r *= _base;\n");
                OUT(t, "          sp[-2] = INT_VAL(_r); sp--;\n");
                OUT(t, "          goto L_invoke_ic_%d_done;\n", start_ip);
                OUT(t, "        }\n");
                OUT(t, "      }\n");
            /* --- Tier 3: Two-arg int methods --- */
            } else if (argc == 2 && strcmp(methodName->chars, "clamp") == 0) {
                /* int.clamp(min, max) - inline */
                OUT(t, "      if (__builtin_expect(IS_INT(_recv) & IS_INT(sp[-2]) & IS_INT(sp[-1]), 1)) {\n");
                OUT(t, "        int64_t _n = AS_INT(_recv), _lo = AS_INT(sp[-2]), _hi = AS_INT(sp[-1]);\n");
                OUT(t, "        if (_n < _lo) _n = _lo; else if (_n > _hi) _n = _hi;\n");
                OUT(t, "        sp[-3] = INT_VAL(_n); sp -= 2;\n");
                OUT(t, "        goto L_invoke_ic_%d_done;\n", start_ip);
                OUT(t, "      }\n");
            } else if (argc == 2 && strcmp(methodName->chars, "between") == 0) {
                /* int.between(min, max) - inline */
                OUT(t, "      if (__builtin_expect(IS_INT(_recv) & IS_INT(sp[-2]) & IS_INT(sp[-1]), 1)) {\n");
                OUT(t, "        int64_t _n = AS_INT(_recv), _lo = AS_INT(sp[-2]), _hi = AS_INT(sp[-1]);\n");
                OUT(t, "        sp[-3] = BOOL_VAL(_n >= _lo && _n <= _hi); sp -= 2;\n");
                OUT(t, "        goto L_invoke_ic_%d_done;\n", start_ip);
                OUT(t, "      }\n");
            }

            /* Instance method IC fast path */
            OUT(t, "      if (__builtin_expect(IS_INSTANCE(_recv), 1)) {\n");
            OUT(t, "        ObjInstance* _inst = AS_INSTANCE(_recv);\n");
            OUT(t, "        BtlMethodIC* _ic = &frame->closure->methodICs[%d];\n", icSlot);
            OUT(t, "        if (__builtin_expect(_ic->cachedClass == _inst->klass && _ic->methodIndex >= 0, 1)) {\n");
            OUT(t, "          BtlMethodEntry* _me = &_inst->klass->methods[_ic->methodIndex];\n");
            OUT(t, "          if (__builtin_expect(%d == _me->arity, 1)) {\n", argc);
            OUT(t, "            vm->stackTop = sp;\n");
            OUT(t, "            if (__builtin_expect(vm->frameCount >= vm->frameCapacity, 0)) {\n");
            OUT(t, "              if (!btl_ensure_frame_capacity(vm)) return BTL_INTERPRET_RUNTIME_ERROR;\n");
            OUT(t, "            }\n");
            OUT(t, "            { BtlCallFrame* _nf = &vm->frames[vm->frameCount++];\n");
            OUT(t, "            _nf->closure = _me->closure;\n");
            OUT(t, "            _nf->ip = _me->closure->function->chunk.code;\n");
            OUT(t, "            _nf->slots = vm->stackTop - %d;\n", argc + 1);
            OUT(t, "            _nf->openUpvalues = NULL;\n");
            OUT(t, "            BtlFnPtr _h = (BtlFnPtr)_me->closure->function->compiledHandler;\n");
            OUT(t, "            if (_h) {\n");
            OUT(t, "              BtlInterpretResult _r = _h(vm);\n");
            OUT(t, "              if (_r != BTL_INTERPRET_OK) return _r;\n");
            OUT(t, "            } else {\n");
            OUT(t, "              int _sf = vm->runFloor; vm->runFloor = vm->frameCount - 1;\n");
            OUT(t, "              BtlInterpretResult _r = btl_run(vm);\n");
            OUT(t, "              vm->runFloor = _sf;\n");
            OUT(t, "              if (_r != BTL_INTERPRET_OK) return _r;\n");
            OUT(t, "            } }\n");
            emit_reload(t);
            OUT(t, "            goto L_invoke_ic_%d_done;\n", start_ip);
            OUT(t, "          }\n");
            OUT(t, "        }\n");
            OUT(t, "      }\n");
            /* Native module method fast paths (e.g., math.abs, math.floor) */
            if (argc == 1) {
                if (strcmp(methodName->chars, "abs") == 0) {
                    OUT(t, "      if (__builtin_expect(IS_NATIVE_MODULE(_recv), 0)) {\n");
                    OUT(t, "        BtlValue _arg = sp[-1];\n");
                    OUT(t, "        if (IS_INT(_arg)) {\n");
                    OUT(t, "          int64_t _n = AS_INT(_arg); sp[-2] = INT_VAL(_n < 0 ? -_n : _n); sp--;\n");
                    OUT(t, "          goto L_invoke_ic_%d_done;\n", start_ip);
                    OUT(t, "        }\n");
                    OUT(t, "        if (IS_NUMBER(_arg)) {\n");
                    OUT(t, "          sp[-2] = NUMBER_VAL(fabs(AS_NUMBER(_arg))); sp--;\n");
                    OUT(t, "          goto L_invoke_ic_%d_done;\n", start_ip);
                    OUT(t, "        }\n");
                    OUT(t, "      }\n");
                } else if (strcmp(methodName->chars, "floor") == 0) {
                    OUT(t, "      if (__builtin_expect(IS_NATIVE_MODULE(_recv), 0)) {\n");
                    OUT(t, "        BtlValue _arg = sp[-1];\n");
                    OUT(t, "        if (IS_INT(_arg)) {\n");
                    OUT(t, "          sp[-2] = _arg; sp--;\n");
                    OUT(t, "          goto L_invoke_ic_%d_done;\n", start_ip);
                    OUT(t, "        }\n");
                    OUT(t, "        if (IS_NUMBER(_arg)) {\n");
                    OUT(t, "          sp[-2] = INT_VAL((int64_t)floor(AS_NUMBER(_arg))); sp--;\n");
                    OUT(t, "          goto L_invoke_ic_%d_done;\n", start_ip);
                    OUT(t, "        }\n");
                    OUT(t, "      }\n");
                } else if (strcmp(methodName->chars, "ceil") == 0) {
                    OUT(t, "      if (__builtin_expect(IS_NATIVE_MODULE(_recv), 0)) {\n");
                    OUT(t, "        BtlValue _arg = sp[-1];\n");
                    OUT(t, "        if (IS_INT(_arg)) {\n");
                    OUT(t, "          sp[-2] = _arg; sp--;\n");
                    OUT(t, "          goto L_invoke_ic_%d_done;\n", start_ip);
                    OUT(t, "        }\n");
                    OUT(t, "        if (IS_NUMBER(_arg)) {\n");
                    OUT(t, "          sp[-2] = INT_VAL((int64_t)ceil(AS_NUMBER(_arg))); sp--;\n");
                    OUT(t, "          goto L_invoke_ic_%d_done;\n", start_ip);
                    OUT(t, "        }\n");
                    OUT(t, "      }\n");
                } else if (strcmp(methodName->chars, "sqrt") == 0) {
                    OUT(t, "      if (__builtin_expect(IS_NATIVE_MODULE(_recv), 0)) {\n");
                    OUT(t, "        BtlValue _arg = sp[-1];\n");
                    OUT(t, "        if (IS_INT(_arg)) {\n");
                    OUT(t, "          sp[-2] = NUMBER_VAL(sqrt((double)AS_INT(_arg))); sp--;\n");
                    OUT(t, "          goto L_invoke_ic_%d_done;\n", start_ip);
                    OUT(t, "        }\n");
                    OUT(t, "        if (IS_NUMBER(_arg)) {\n");
                    OUT(t, "          sp[-2] = NUMBER_VAL(sqrt(AS_NUMBER(_arg))); sp--;\n");
                    OUT(t, "          goto L_invoke_ic_%d_done;\n", start_ip);
                    OUT(t, "        }\n");
                    OUT(t, "      }\n");
                } else if (strcmp(methodName->chars, "round") == 0) {
                    OUT(t, "      if (__builtin_expect(IS_NATIVE_MODULE(_recv), 0)) {\n");
                    OUT(t, "        BtlValue _arg = sp[-1];\n");
                    OUT(t, "        if (IS_INT(_arg)) {\n");
                    OUT(t, "          sp[-2] = _arg; sp--;\n");
                    OUT(t, "          goto L_invoke_ic_%d_done;\n", start_ip);
                    OUT(t, "        }\n");
                    OUT(t, "        if (IS_NUMBER(_arg)) {\n");
                    OUT(t, "          sp[-2] = INT_VAL((int64_t)round(AS_NUMBER(_arg))); sp--;\n");
                    OUT(t, "          goto L_invoke_ic_%d_done;\n", start_ip);
                    OUT(t, "        }\n");
                    OUT(t, "      }\n");
                }
            } else if (argc == 2) {
                if (strcmp(methodName->chars, "min") == 0) {
                    OUT(t, "      if (__builtin_expect(IS_NATIVE_MODULE(_recv), 0)) {\n");
                    OUT(t, "        BtlValue _a = sp[-2], _b = sp[-1];\n");
                    OUT(t, "        if (IS_INT(_a) & IS_INT(_b)) {\n");
                    OUT(t, "          int64_t _ia = AS_INT(_a), _ib = AS_INT(_b);\n");
                    OUT(t, "          sp[-3] = INT_VAL(_ia < _ib ? _ia : _ib); sp -= 2;\n");
                    OUT(t, "          goto L_invoke_ic_%d_done;\n", start_ip);
                    OUT(t, "        }\n");
                    OUT(t, "        if (IS_NUMBER(_a) & IS_NUMBER(_b)) {\n");
                    OUT(t, "          double _da = AS_NUMBER(_a), _db = AS_NUMBER(_b);\n");
                    OUT(t, "          sp[-3] = NUMBER_VAL(_da < _db ? _da : _db); sp -= 2;\n");
                    OUT(t, "          goto L_invoke_ic_%d_done;\n", start_ip);
                    OUT(t, "        }\n");
                    OUT(t, "      }\n");
                } else if (strcmp(methodName->chars, "max") == 0) {
                    OUT(t, "      if (__builtin_expect(IS_NATIVE_MODULE(_recv), 0)) {\n");
                    OUT(t, "        BtlValue _a = sp[-2], _b = sp[-1];\n");
                    OUT(t, "        if (IS_INT(_a) & IS_INT(_b)) {\n");
                    OUT(t, "          int64_t _ia = AS_INT(_a), _ib = AS_INT(_b);\n");
                    OUT(t, "          sp[-3] = INT_VAL(_ia > _ib ? _ia : _ib); sp -= 2;\n");
                    OUT(t, "          goto L_invoke_ic_%d_done;\n", start_ip);
                    OUT(t, "        }\n");
                    OUT(t, "        if (IS_NUMBER(_a) & IS_NUMBER(_b)) {\n");
                    OUT(t, "          double _da = AS_NUMBER(_a), _db = AS_NUMBER(_b);\n");
                    OUT(t, "          sp[-3] = NUMBER_VAL(_da > _db ? _da : _db); sp -= 2;\n");
                    OUT(t, "          goto L_invoke_ic_%d_done;\n", start_ip);
                    OUT(t, "        }\n");
                    OUT(t, "      }\n");
                }
            }
            OUT(t, "      /* IC miss or non-instance: slow path*/\n");
            OUT(t, "      vm->stackTop = sp;\n");
            OUT(t, "      if (!btl_compiled_invoke_ic(vm, frame, %d, %d, %d)) return BTL_INTERPRET_RUNTIME_ERROR;\n", nameIdx, argc, icSlot);
            emit_reload(t);
            OUT(t, "    L_invoke_ic_%d_done:;\n", start_ip);
            OUT(t, "    }\n");
            /* Type tracking: pop receiver + args, push result type */
            for (int i = 0; i < argc + 1; i++) type_pop(&ts);
            if (argc == 0 && strcmp(methodName->chars, "toString") == 0) {
                type_push(&ts, TYPE_STRING);
            } else if (argc == 0 && (strcmp(methodName->chars, "length") == 0 ||
                       strcmp(methodName->chars, "abs") == 0 ||
                       strcmp(methodName->chars, "sign") == 0 ||
                       strcmp(methodName->chars, "bitNot") == 0 ||
                       strcmp(methodName->chars, "floor") == 0 ||
                       strcmp(methodName->chars, "ceil") == 0 ||
                       strcmp(methodName->chars, "round") == 0)) {
                type_push(&ts, TYPE_INT);
            } else if (argc == 1 && (strcmp(methodName->chars, "min") == 0 ||
                       strcmp(methodName->chars, "max") == 0 ||
                       strcmp(methodName->chars, "mod") == 0 ||
                       strcmp(methodName->chars, "gcd") == 0 ||
                       strcmp(methodName->chars, "pow") == 0 ||
                       strcmp(methodName->chars, "bitAnd") == 0 ||
                       strcmp(methodName->chars, "bitOr") == 0 ||
                       strcmp(methodName->chars, "bitXor") == 0 ||
                       strcmp(methodName->chars, "leftShift") == 0 ||
                       strcmp(methodName->chars, "rightShift") == 0)) {
                type_push(&ts, TYPE_INT);
            } else if (argc == 2 && strcmp(methodName->chars, "clamp") == 0) {
                type_push(&ts, TYPE_INT);
            } else if (argc == 0 && (strcmp(methodName->chars, "isEven") == 0 ||
                       strcmp(methodName->chars, "isOdd") == 0 ||
                       strcmp(methodName->chars, "isZero") == 0 ||
                       strcmp(methodName->chars, "isPositive") == 0 ||
                       strcmp(methodName->chars, "isNegative") == 0 ||
                       strcmp(methodName->chars, "has") == 0)) {
                type_push(&ts, TYPE_BOOL);
            } else if (argc == 1 && strcmp(methodName->chars, "between") == 0) {
                type_push(&ts, TYPE_BOOL);
            } else if (argc == 0 && strcmp(methodName->chars, "toFloat") == 0) {
                type_push(&ts, TYPE_NUMBER);
            } else {
                type_push(&ts, TYPE_UNKNOWN);
            }
            break;
        }

                         /* Tail invoke indexed*/
        case BTL_OP_TAIL_INVOKE_0: case BTL_OP_TAIL_INVOKE_1: case BTL_OP_TAIL_INVOKE_2:
        case BTL_OP_TAIL_INVOKE_3: case BTL_OP_TAIL_INVOKE_4: case BTL_OP_TAIL_INVOKE_5:
        case BTL_OP_TAIL_INVOKE_6: case BTL_OP_TAIL_INVOKE_7: case BTL_OP_TAIL_INVOKE_8: {
            int argc = op - BTL_OP_TAIL_INVOKE_0;
            uint8_t methodIdx = code[ip++];
            emit_comment(t, start_ip, "OP_TAIL_INVOKE_N");
            emit_call_bracket_open(t);
            OUT(t, "    if (!btl_compiled_invoke_indexed(vm, %d, %d)) return BTL_INTERPRET_RUNTIME_ERROR;\n", methodIdx, argc);
            emit_call_bracket_close(t);
            break;
        }
        case BTL_OP_TAIL_INVOKE: {
            uint8_t methodIdx = code[ip++];
            uint8_t argc = code[ip++];
            emit_comment(t, start_ip, "OP_TAIL_INVOKE");
            emit_call_bracket_open(t);
            OUT(t, "    if (!btl_compiled_invoke_indexed(vm, %d, %d)) return BTL_INTERPRET_RUNTIME_ERROR;\n", methodIdx, argc);
            emit_call_bracket_close(t);
            break;
        }
        case BTL_OP_TAIL_INVOKE_LONG: {
            uint16_t methodIdx = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            uint8_t argc = code[ip++];
            emit_comment(t, start_ip, "OP_TAIL_INVOKE_LONG");
            emit_call_bracket_open(t);
            OUT(t, "    if (!btl_compiled_invoke_indexed(vm, %d, %d)) return BTL_INTERPRET_RUNTIME_ERROR;\n", methodIdx, argc);
            emit_call_bracket_close(t);
            break;
        }
        case BTL_OP_TAIL_INVOKE_IC: {
            uint8_t nameIdx = code[ip++];
            uint8_t argc = code[ip++];
            uint8_t icSlot = code[ip++];
            emit_comment(t, start_ip, "OP_TAIL_INVOKE_IC");
            emit_call_bracket_open(t);
            OUT(t, "    if (!btl_compiled_invoke_ic(vm, frame, %d, %d, %d)) return BTL_INTERPRET_RUNTIME_ERROR;\n", nameIdx, argc, icSlot);
            emit_call_bracket_close(t);
            break;
        }

                              // ================================================================
                              // SUPER INVOKE
                              // ================================================================
        case BTL_OP_SUPER_INVOKE_0: case BTL_OP_SUPER_INVOKE_1: case BTL_OP_SUPER_INVOKE_2:
        case BTL_OP_SUPER_INVOKE_3: case BTL_OP_SUPER_INVOKE_4: case BTL_OP_SUPER_INVOKE_5:
        case BTL_OP_SUPER_INVOKE_6: case BTL_OP_SUPER_INVOKE_7: case BTL_OP_SUPER_INVOKE_8: {
            int argc = op - BTL_OP_SUPER_INVOKE_0;
            uint8_t methodIdx = code[ip++];
            emit_comment(t, start_ip, "OP_SUPER_INVOKE_N");
            emit_inline_super_invoke(t, methodIdx, argc);
            /* Type tracking: pop super + args, push unknown result */
            for (int i = 0; i < argc + 1; i++) type_pop(&ts);
            type_push(&ts, TYPE_UNKNOWN);
            break;
        }
        case BTL_OP_SUPER_INVOKE: {
            uint8_t methodIdx = code[ip++];
            uint8_t argc = code[ip++];
            emit_comment(t, start_ip, "OP_SUPER_INVOKE");
            emit_inline_super_invoke(t, methodIdx, argc);
            /* Type tracking: pop super + args, push unknown result */
            for (int i = 0; i < argc + 1; i++) type_pop(&ts);
            type_push(&ts, TYPE_UNKNOWN);
            break;
        }
        case BTL_OP_SUPER_INVOKE_LONG: {
            uint16_t methodIdx = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            uint8_t argc = code[ip++];
            emit_comment(t, start_ip, "OP_SUPER_INVOKE_LONG");
            emit_inline_super_invoke(t, methodIdx, argc);
            /* Type tracking: pop super + args, push unknown result */
            for (int i = 0; i < argc + 1; i++) type_pop(&ts);
            type_push(&ts, TYPE_UNKNOWN);
            break;
        }
                                 /* Tail super invoke*/
        case BTL_OP_TAIL_SUPER_INVOKE_0: case BTL_OP_TAIL_SUPER_INVOKE_1: case BTL_OP_TAIL_SUPER_INVOKE_2:
        case BTL_OP_TAIL_SUPER_INVOKE_3: case BTL_OP_TAIL_SUPER_INVOKE_4: case BTL_OP_TAIL_SUPER_INVOKE_5:
        case BTL_OP_TAIL_SUPER_INVOKE_6: case BTL_OP_TAIL_SUPER_INVOKE_7: case BTL_OP_TAIL_SUPER_INVOKE_8: {
            int argc = op - BTL_OP_TAIL_SUPER_INVOKE_0;
            uint8_t methodIdx = code[ip++];
            emit_comment(t, start_ip, "OP_TAIL_SUPER_INVOKE_N");
            emit_inline_super_invoke(t, methodIdx, argc);
            break;
        }
        case BTL_OP_TAIL_SUPER_INVOKE: {
            uint8_t methodIdx = code[ip++];
            uint8_t argc = code[ip++];
            emit_comment(t, start_ip, "OP_TAIL_SUPER_INVOKE");
            emit_inline_super_invoke(t, methodIdx, argc);
            break;
        }
        case BTL_OP_TAIL_SUPER_INVOKE_LONG: {
            uint16_t methodIdx = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            uint8_t argc = code[ip++];
            emit_comment(t, start_ip, "OP_TAIL_SUPER_INVOKE_LONG");
            emit_inline_super_invoke(t, methodIdx, argc);
            break;
        }

                                      // ================================================================
                                      // CLOSURES
                                      // ================================================================
        case BTL_OP_CLOSURE: case BTL_OP_CLOSURE_LONG: {
            uint16_t fn_idx;
            if (op == BTL_OP_CLOSURE) {
                fn_idx = code[ip++];
            } else {
                fn_idx = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            }
            /* Read upvalue descriptors (advance ip past them)*/
            BtlValue fn_val_c = fn->chunk.constants.values[fn_idx];
            ObjFunction* child_c = AS_FUNCTION(fn_val_c);
            int upvalue_start_ip = ip;
            ip += child_c->upvalueCount * 3;

            /* Fuse: CLOSURE (no upvalues) + DEFINE_GLOBAL -> direct assign */
            bool fused_define = false;
            if (child_c->upvalueCount == 0 && ip < code_len) {
                uint8_t next_op = code[ip];
                if (next_op == BTL_OP_DEFINE_GLOBAL) {
                    uint8_t gidx = code[ip + 1];
                    emit_comment(t, start_ip, "FUSED: closure+define_global");
                    emit_sync(t);
                    OUT(t, "    { ObjFunction* _f = AS_FUNCTION(fn->chunk.constants.values[%d]);\n", fn_idx);
                    OUT(t, "      ObjClosure* _c = btl_closure_new(vm, _f);\n");
                    OUT(t, "      sp = vm->stackTop;\n");
                    OUT(t, "      mod->globalValues.values[%d] = OBJ_VAL(_c); }\n", gidx);
                    ip += 2;
                    fused_define = true;
                } else if (next_op == BTL_OP_DEFINE_GLOBAL_LONG) {
                    uint16_t gidx = (uint16_t)((code[ip + 1] << 8) | code[ip + 2]);
                    emit_comment(t, start_ip, "FUSED: closure+define_global_long");
                    emit_sync(t);
                    OUT(t, "    { ObjFunction* _f = AS_FUNCTION(fn->chunk.constants.values[%d]);\n", fn_idx);
                    OUT(t, "      ObjClosure* _c = btl_closure_new(vm, _f);\n");
                    OUT(t, "      sp = vm->stackTop;\n");
                    OUT(t, "      mod->globalValues.values[%d] = OBJ_VAL(_c); }\n", gidx);
                    ip += 3;
                    fused_define = true;
                }
            }
            if (fused_define) break;

            emit_comment(t, start_ip, "OP_CLOSURE");
            /* Sync sp Ã¢â‚¬â€ newClosure can trigger GC*/
            emit_sync(t);
            OUT(t, "    {\n");
            OUT(t, "        ObjFunction* _f = AS_FUNCTION(fn->chunk.constants.values[%d]);\n", fn_idx);
            OUT(t, "        ObjClosure* _c = btl_closure_new(vm, _f);\n");
            OUT(t, "        btl_push(vm, OBJ_VAL(_c));\n");

            /* Emit upvalue descriptors from saved position */
            { int uip = upvalue_start_ip;
            for (int i = 0; i < child_c->upvalueCount; i++) {
                uint8_t isLocal = code[uip++];
                uint8_t index = code[uip++];
                uint8_t isMutable = code[uip++];

                OUT(t, "        { BtlRuntimeUpvalue* _d = &_c->upvalues[%d];\n", i);
                OUT(t, "          _d->isMutable = %s;\n", isMutable ? "true" : "false");
                if (isLocal) {
                    OUT(t, "          _d->isOpen = true; _d->loc.stack = slots + %d;\n", index);
                    OUT(t, "          _d->next = frame->openUpvalues; frame->openUpvalues = _d;\n");
                } else {
                    OUT(t, "          BtlRuntimeUpvalue* _p = &frame->closure->upvalues[%d];\n", index);
                    OUT(t, "          _d->isOpen = _p->isOpen;\n");
                    OUT(t, "          if (_p->isOpen) {\n");
                    OUT(t, "              _d->loc.stack = _p->loc.stack; _d->next = _p->next; _p->next = _d;\n");
                    OUT(t, "          } else {\n");
                    OUT(t, "              if (_p->isMutable) _d->loc.box = _p->loc.box;\n");
                    OUT(t, "              else _d->loc.immValue = _p->loc.immValue;\n");
                    OUT(t, "              _d->next = NULL;\n");
                    OUT(t, "          }\n");
                }
                OUT(t, "        }\n");
            }
            } /* end uip scope */
            OUT(t, "    }\n");
            /* Reload sp after GC-triggering newClosure*/
            OUT(t, "    sp = vm->stackTop;\n");
            type_push(&ts, TYPE_UNKNOWN);
            break;
        }

        case BTL_OP_CLOSE_UPVALUE:
            emit_comment(t, start_ip, "OP_CLOSE_UPVALUE");
            emit_sync(t);
            OUT(t, "    btl_compiled_close_upvalues(vm, frame); btl_pop(vm);\n");
            emit_light_reload(t);  /* Close upvalues never pushes frames */
            type_pop(&ts);  /* btl_pop removes one value */
            break;

            // ================================================================
            // RETURN
            //
            // Sync sp, tear down frame, push result, return.
            // ================================================================
        case BTL_OP_RETURN:
            if (reachable) {
                emit_comment(t, start_ip, "OP_RETURN");
                OUT(t, "    {\n");
                OUT(t, "        BtlValue _result = sp[-1];\n");
                emit_sync(t);
                /* Only call close_upvalues if this function needs it*/
                if (function_needs_close_upvalues(t->current_fn)) {
                    OUT(t, "        btl_compiled_close_upvalues(vm, frame);\n");
                }
                OUT(t, "        vm->frameCount--;\n");
                OUT(t, "        if (vm->frameCount == 0) { vm->lastReturnValue = _result; vm->stackTop--; return BTL_INTERPRET_OK; }\n");
                OUT(t, "        vm->stackTop = frame->slots;\n");
                OUT(t, "        *vm->stackTop++ = _result; /* inline push*/\n");
                OUT(t, "        return BTL_INTERPRET_OK;\n");
                OUT(t, "    }\n");
            }
            reachable = false; /* Code after return is dead*/
            break;

            // ================================================================
            // CLASSES
            // ================================================================
        case BTL_OP_CLASS: {
            uint8_t nameIdx = code[ip++];
            emit_comment(t, start_ip, "OP_CLASS");
            emit_call_bracket_open(t);
            OUT(t, "    btl_compiled_class(vm, fn, %d);\n", nameIdx);
            emit_light_reload(t);
            type_push(&ts, TYPE_OBJECT);  /* class pushed onto stack */
            break;
        }
        case BTL_OP_CLASS_LONG: {
            uint16_t nameIdx = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            emit_comment(t, start_ip, "OP_CLASS_LONG");
            emit_call_bracket_open(t);
            OUT(t, "    btl_compiled_class_long(vm, fn, %d);\n", nameIdx);
            emit_light_reload(t);
            type_push(&ts, TYPE_OBJECT);  /* class pushed onto stack */
            break;
        }
        case BTL_OP_INHERIT:
            emit_comment(t, start_ip, "OP_INHERIT");
            emit_call_bracket_open(t);
            OUT(t, "    if (!btl_compiled_inherit(vm)) return BTL_INTERPRET_RUNTIME_ERROR;\n");
            emit_light_reload(t);
            type_pop(&ts); type_pop(&ts);  /* pops superclass and subclass */
            break;
        case BTL_OP_METHOD: {
            uint8_t methodIdx = code[ip++];
            uint8_t arity = code[ip++];
            emit_comment(t, start_ip, "OP_METHOD");
            emit_call_bracket_open(t);
            OUT(t, "    btl_compiled_method(vm, %d, %d);\n", methodIdx, arity);
            emit_light_reload(t);
            type_pop(&ts);  /* pops method closure, class stays */
            break;
        }
        case BTL_OP_METHOD_LONG: {
            uint16_t methodIdx = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            uint8_t arity = code[ip++];
            emit_comment(t, start_ip, "OP_METHOD_LONG");
            emit_call_bracket_open(t);
            OUT(t, "    btl_compiled_method(vm, %d, %d);\n", methodIdx, arity);
            emit_light_reload(t);
            type_pop(&ts);  /* pops method closure, class stays */
            break;
        }

                           // ================================================================
                           // COLLECTIONS
                           // ================================================================
        case BTL_OP_BUILD_LIST: {
            uint8_t count = code[ip++];
            emit_comment(t, start_ip, "OP_BUILD_LIST");
            emit_call_bracket_open(t);
            OUT(t, "    btl_compiled_build_list(vm, %d);\n", count);
            emit_light_reload(t);
            for (int i = 0; i < count; i++) type_pop(&ts);
            type_push(&ts, TYPE_UNKNOWN);
            break;
        }
        case BTL_OP_BUILD_TABLE: {
            uint8_t count = code[ip++];
            emit_comment(t, start_ip, "OP_BUILD_TABLE");
            emit_call_bracket_open(t);
            OUT(t, "    btl_compiled_build_table(vm, %d);\n", count);
            emit_light_reload(t);
            for (int i = 0; i < count * 2; i++) type_pop(&ts);
            type_push(&ts, TYPE_UNKNOWN);
            break;
        }
        case BTL_OP_INDEX_GET:
            emit_comment(t, start_ip, "OP_INDEX_GET");
            /* Inline fast paths for list[index] and table[key] access */
            OUT(t, "    { BtlValue _key = sp[-1], _obj = sp[-2];\n");
            /* INT key fast path (most common — loop variables are INT) */
            OUT(t, "      if (__builtin_expect(IS_LIST(_obj) && IS_INT(_key), 1)) {\n");
            OUT(t, "        ObjList* _l = AS_LIST(_obj);\n");
            OUT(t, "        int _idx = (int)AS_INT(_key);\n");
            OUT(t, "        if (__builtin_expect(_idx >= 0 && _idx < _l->items.count, 1)) {\n");
            OUT(t, "          sp -= 2; PUSH(sp, _l->items.values[_idx]);\n");
            OUT(t, "          goto L_idx_get_%d_done;\n", start_ip);
            OUT(t, "        }\n");
            OUT(t, "      }\n");
            /* Table fast path — before NUMBER list path (tables more common) */
            OUT(t, "      if (IS_TABLE(_obj)) {\n");
            OUT(t, "        ObjTable* _t = AS_TABLE(_obj);\n");
            /* Inline string-key lookup: uses pre-computed hash + pointer equality */
            OUT(t, "        if (__builtin_expect(IS_STRING(_key) && _t->table.count > 0, 1)) {\n");
            OUT(t, "          ObjString* _sk = AS_STRING(_key);\n");
            OUT(t, "          uint32_t _hi = _sk->hash & (_t->table.capacity - 1);\n");
            OUT(t, "          for (;;) {\n");
            OUT(t, "            BtlEntry* _e = &_t->table.entries[_hi];\n");
            OUT(t, "            if (IS_EMPTY(_e->key)) {\n");
            OUT(t, "              if (IS_NULL(_e->value)) { sp[-2] = BTL_NULL_VAL; break; }\n");
            OUT(t, "            } else if (_e->key == _key) {\n");
            OUT(t, "              sp[-2] = _e->value; break;\n");
            OUT(t, "            }\n");
            OUT(t, "            _hi = (_hi + 1) & (_t->table.capacity - 1);\n");
            OUT(t, "          }\n");
            OUT(t, "          sp--;\n");
            OUT(t, "        } else {\n");
            OUT(t, "          BtlValue _val;\n");
            OUT(t, "          sp[-2] = btl_table_get(&_t->table, _key, &_val) ? _val : BTL_NULL_VAL;\n");
            OUT(t, "          sp--;\n");
            OUT(t, "        }\n");
            OUT(t, "        goto L_idx_get_%d_done;\n", start_ip);
            OUT(t, "      }\n");
            /* NUMBER key path (rare — float indices into lists) */
            OUT(t, "      if (__builtin_expect(IS_LIST(_obj) && IS_NUMBER(_key), 1)) {\n");
            OUT(t, "        ObjList* _l = AS_LIST(_obj);\n");
            OUT(t, "        int _idx = (int)AS_NUMBER(_key);\n");
            OUT(t, "        if (__builtin_expect(_idx >= 0 && _idx < _l->items.count, 1)) {\n");
            OUT(t, "          sp -= 2; PUSH(sp, _l->items.values[_idx]);\n");
            OUT(t, "          goto L_idx_get_%d_done;\n", start_ip);
            OUT(t, "        }\n");
            OUT(t, "      }\n");
            OUT(t, "      /* Slow path: string indexing, or bounds check */\n");
            emit_sync(t);
            OUT(t, "      if (!btl_compiled_index_get(vm)) return BTL_INTERPRET_RUNTIME_ERROR;\n");
            emit_light_reload(t);  /* Index ops never push frames */
            OUT(t, "    L_idx_get_%d_done:; }\n", start_ip);
            type_pop(&ts); type_pop(&ts);  /* Pop object and key */
            type_push(&ts, TYPE_UNKNOWN);  /* Push result */
            break;
        case BTL_OP_INDEX_SET:
            emit_comment(t, start_ip, "OP_INDEX_SET");
            /* Inline fast paths for list[index] = value and table[key] = value */
            OUT(t, "    { BtlValue _val = sp[-1], _key = sp[-2], _obj = sp[-3];\n");
            /* INT key fast path (most common — loop variables are INT) */
            OUT(t, "      if (__builtin_expect(IS_LIST(_obj) && IS_INT(_key), 1)) {\n");
            OUT(t, "        ObjList* _l = AS_LIST(_obj);\n");
            OUT(t, "        int _idx = (int)AS_INT(_key);\n");
            OUT(t, "        if (__builtin_expect(_idx >= 0 && _idx < _l->items.count, 1)) {\n");
            OUT(t, "          _l->items.values[_idx] = _val;\n");
            OUT(t, "          if (IS_OBJ(_val)) btl_gc_write_barrier(vm, (BtlObj*)_l, _val);\n");
            OUT(t, "          sp -= 3; PUSH(sp, _val);\n");
            OUT(t, "          goto L_idx_set_%d_done;\n", start_ip);
            OUT(t, "        }\n");
            OUT(t, "      }\n");
            /* Table fast path — before NUMBER list path (tables more common) */
            OUT(t, "      if (IS_TABLE(_obj)) {\n");
            OUT(t, "        ObjTable* _t = AS_TABLE(_obj);\n");
            OUT(t, "        vm->stackTop = sp;\n");
            OUT(t, "        btl_table_set(vm, &_t->table, _key, _val);\n");
            OUT(t, "        sp = vm->stackTop;\n");
            OUT(t, "        if (IS_OBJ(_val)) btl_gc_write_barrier(vm, (BtlObj*)_t, _val);\n");
            OUT(t, "        if (IS_OBJ(_key)) btl_gc_write_barrier(vm, (BtlObj*)_t, _key);\n");
            OUT(t, "        sp[-3] = _val; sp -= 2;\n");
            OUT(t, "        goto L_idx_set_%d_done;\n", start_ip);
            OUT(t, "      }\n");
            /* NUMBER key path (rare — float indices into lists) */
            OUT(t, "      if (__builtin_expect(IS_LIST(_obj) && IS_NUMBER(_key), 1)) {\n");
            OUT(t, "        ObjList* _l = AS_LIST(_obj);\n");
            OUT(t, "        int _idx = (int)AS_NUMBER(_key);\n");
            OUT(t, "        if (__builtin_expect(_idx >= 0 && _idx < _l->items.count, 1)) {\n");
            OUT(t, "          _l->items.values[_idx] = _val;\n");
            OUT(t, "          if (IS_OBJ(_val)) btl_gc_write_barrier(vm, (BtlObj*)_l, _val);\n");
            OUT(t, "          sp -= 3; PUSH(sp, _val);\n");
            OUT(t, "          goto L_idx_set_%d_done;\n", start_ip);
            OUT(t, "        }\n");
            OUT(t, "      }\n");
            OUT(t, "      /* Slow path: append, or bounds check */\n");
            emit_sync(t);
            OUT(t, "      if (!btl_compiled_index_set(vm)) return BTL_INTERPRET_RUNTIME_ERROR;\n");
            emit_light_reload(t);  /* Index ops never push frames */
            OUT(t, "    L_idx_set_%d_done:; }\n", start_ip);
            type_pop(&ts); type_pop(&ts); type_pop(&ts);  /* Pop object, key, value */
            type_push(&ts, TYPE_UNKNOWN);  /* Push result */
            break;

            // ================================================================
            // MODULES
            // ================================================================
        case BTL_OP_IMPORT: {
            uint8_t nameIdx = code[ip++];
            emit_comment(t, start_ip, "OP_IMPORT");
            emit_call_bracket_open(t);
            OUT(t, "    if (!btl_compiled_import(vm, frame, %d)) return BTL_INTERPRET_RUNTIME_ERROR;\n", nameIdx);
            emit_light_call_bracket_close(t);  /* Import never pushes frames */
            type_push(&ts, TYPE_UNKNOWN);
            break;
        }
        case BTL_OP_IMPORT_LONG: {
            uint16_t nameIdx = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            emit_comment(t, start_ip, "OP_IMPORT_LONG");
            emit_call_bracket_open(t);
            OUT(t, "    if (!btl_compiled_import_long(vm, frame, %d)) return BTL_INTERPRET_RUNTIME_ERROR;\n", nameIdx);
            emit_light_call_bracket_close(t);  /* Import never pushes frames */
            type_push(&ts, TYPE_UNKNOWN);
            break;
        }

                           // ================================================================
                           // ACTORS
                           // ================================================================
        case BTL_OP_DO_NEW: {
            uint8_t argc = code[ip++];
            emit_comment(t, start_ip, "OP_DO_NEW");
            emit_call_bracket_open(t);
            OUT(t, "    if (!btl_compiled_do_new(vm, %d)) return BTL_INTERPRET_RUNTIME_ERROR;\n", argc);
            emit_call_bracket_close(t);
            break;
        }
        case BTL_OP_DO_INVOKE: {
            uint8_t nameConst = code[ip++];
            uint8_t argc = code[ip++];
            emit_comment(t, start_ip, "OP_DO_INVOKE");
            emit_call_bracket_open(t);
            OUT(t, "    if (!btl_compiled_do_invoke(vm, frame, %d, %d)) return BTL_INTERPRET_RUNTIME_ERROR;\n", nameConst, argc);
            emit_call_bracket_close(t);
            break;
        }

                           // ================================================================
                           // ITERATORS (for...in)
                           // ================================================================
        case BTL_OP_ITER_INIT: {
            emit_comment(t, start_ip, "OP_ITER_INIT");
            emit_sync(t);
            OUT(t, "    if (!btl_compiled_iter_init(vm)) return BTL_INTERPRET_RUNTIME_ERROR;\n");
            emit_reload(t);
            type_push(&ts, TYPE_UNKNOWN);  /* pushed index 0 */
            break;
        }
        case BTL_OP_ITER_NEXT: {
            uint8_t slot = code[ip++];
            uint16_t offset = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            int done_target = ip + offset;
            emit_comment(t, start_ip, "OP_ITER_NEXT");
            /* Inline list fast path: avoids emit_sync + function call per iteration */
            OUT(t, "    { BtlValue _col = sp[-2];\n");
            OUT(t, "      if (__builtin_expect(IS_LIST(_col), 1)) {\n");
            OUT(t, "        ObjList* _l = AS_LIST(_col);\n");
            OUT(t, "        int _idx = (int)AS_INT(sp[-1]);\n");
            OUT(t, "        if (__builtin_expect(_idx >= _l->items.count, 0)) goto L_%04d;\n", done_target);
            OUT(t, "        slots[%d] = _l->items.values[_idx];\n", slot);
            OUT(t, "        sp[-1] = INT_VAL(_idx + 1);\n");
            OUT(t, "      } else {\n");
            OUT(t, "        vm->stackTop = sp;\n");
            OUT(t, "        if (!btl_compiled_iter_next(vm, %d)) { sp = vm->stackTop; goto L_%04d; }\n", slot, done_target);
            OUT(t, "        sp = vm->stackTop;\n");
            OUT(t, "      } }\n");
            break;
        }

        default:
            emit_sync(t);
            OUT(t, "    /* UNHANDLED OPCODE %d*/\n", op);
            OUT(t, "    btl_runtime_error(vm, \"Unhandled opcode in transpiled code: %%d\", %d); return BTL_INTERPRET_RUNTIME_ERROR;\n", op);
            break;
        }
    }

    OUT(t, "    return BTL_INTERPRET_OK;\n");
    OUT(t, "}\n\n");
    free(targets);
    if (loop_hoists) free(loop_hoists);
    if (loop_types) free(loop_types);
}

// ================================================================
// Program entry point
// ================================================================

static void emit_main(BtlTranspiler* t) {
    OUT(t, "/* Entry point for transpiled program*/\n");
    OUT(t, "BtlInterpretResult btl_compiled_run(VM* vm, ObjModule* module, const char* source) {\n");
    OUT(t, "    ObjFunction* f = btl_compile(vm, module, source);\n");
    OUT(t, "    if (f == NULL) return BTL_INTERPRET_COMPILE_ERROR;\n");
    OUT(t, "    btl_register_functions(f);\n");
    OUT(t, "    btl_push(vm, OBJ_VAL(f));\n");
    OUT(t, "    ObjClosure* c = btl_closure_new(vm, f);\n");
    OUT(t, "    btl_pop(vm); btl_push(vm, OBJ_VAL(c));\n");
    OUT(t, "    btl_call_value(vm, OBJ_VAL(c), 0);\n");
    OUT(t, "    return btl_fn_0(vm);\n");
    OUT(t, "}\n");
}

// ================================================================
// Public API
// ================================================================

BtlTranspiler* btl_transpiler_new(BtlTranspilerConfig config, VM* vm) {
    BtlTranspiler* t = calloc(1, sizeof(BtlTranspiler));
    t->config = config;
    t->vm = vm;
    t->out = fopen(config.output_path, "w");
    if (!t->out) {
        free(t); return NULL;
    }
    return t;
}

void btl_transpiler_free(BtlTranspiler* t) {
    if (t->out) fclose(t->out);
    free(t);
}

bool btl_transpiler_emit_program(BtlTranspiler* t, ObjFunction* main_fn, ObjModule* module) {
    t->module = module;

    /* Build O(1) reverse lookup for global variable names */
    build_global_name_lookup(t, module);

    /* Collect all functions*/
    btl_function_list_init(&t->fns);
    btl_collect_functions(main_fn, &t->fns);

    /* Emit header with forward declarations*/
    emit_header(t);

    /* Emit each function*/
    for (int i = 0; i < t->fns.count; i++) {
        emit_function(t, t->fns.functions[i], i);
    }

    /* Emit entry point*/
    emit_main(t);

    btl_function_list_free(&t->fns);
    return true;
}