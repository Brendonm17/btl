/*
 * BTL to C Transpiler v2 Ã¢â‚¬â€ Performance Edition
 *
 * KEY PERFORMANCE TECHNIQUES:
 *
 * 1) INLINE STACK OPS
 *    Old: push(vm, x);  // function call, vm->stackTop dereference
 *    New: *sp++ = x;    // sp is a C local Ã¢â€ â€™ register
 *
 * 2) CACHED VM STATE
 *    We keep frame, slots, sp as C locals. The C compiler can put
 *    these in registers. We only sync back to vm->stackTop at call
 *    boundaries (where the callee needs to see the real stack).
 *
 * 3) DIRECT CALL THREADING
 *    When OP_CLOSURE creates a function whose constant-pool index maps
 *    to a known transpiled fn id, and OP_CALL later calls that closure,
 *    we emit a direct C function call instead of callValue() + run().
 *    This skips: generic dispatch, opcode decode loop, indirect branch.
 *
 * 4) FUSED OPCODE PATTERNS
 *    Common patterns get single C statements:
 *      GET_LOCAL + GET_LOCAL + LESS + POP_JUMP_IF_FALSE
 *        Ã¢â€ â€™ if (AS_NUMBER(slots[a]) >= AS_NUMBER(slots[b])) goto L;
 *      GET_LOCAL + GET_LOCAL + ADD + SET_LOCAL_POP
 *        Ã¢â€ â€™ slots[c] = NUMBER_VAL(AS_NUMBER(slots[a]) + AS_NUMBER(slots[b]));
 *
 * 5) TAIL CALL OPTIMIZATION
 *    Tail calls to known transpiled functions become a parameter-shuffle
 *    + goto to the function top, or a direct call that replaces the frame.
 */

#include "transpiler.h"
#include "collect.h"
#include "../src/chunk.h"
#include "../src/object.h"
#include "../src/value.h"
#include "../src/vm.h"

#include <stdlib.h>
#include <string.h>
#include <math.h>

 /* ================================================================
  * Output helpers
  * ================================================================ */

#define OUT(t, ...) fprintf((t)->out, __VA_ARGS__)
#define NL(t) fprintf((t)->out, "\n")

static void emit_comment(Transpiler* t, int offset, const char* name) {
    if (t->config.emit_comments) {
        OUT(t, "    /* %04d: %s */\n", offset, name);
    }
}

static void emit_label(Transpiler* t, int offset) {
    OUT(t, "  L_%04d:;\n", offset);
}

/* ================================================================
 * Function list (collect.c)
 * ================================================================ */

void function_list_init(FunctionList* list);
void function_list_free(FunctionList* list);
void collect_functions(ObjFunction* main_fn, FunctionList* out);
int function_id(FunctionList* list, ObjFunction* fn);

/* ================================================================
 * Jump target collection Ã¢â‚¬â€ first pass over bytecode
 * ================================================================ */

static void collect_jump_targets(ObjFunction* fn, bool* targets, int code_len) {
    uint8_t* code = fn->chunk.code;
    memset(targets, 0, sizeof(bool) * (code_len + 1));

    int ip = 0;
    while (ip < code_len) {
        uint8_t op = code[ip];
        ip++;

        switch (op) {
            /* 16-bit forward jumps */
        case OP_JUMP:
        case OP_JUMP_IF_FALSE:
        case OP_POP_JUMP_IF_FALSE:
        case OP_JUMP_IF_TRUE:
        case OP_POP_JUMP_IF_TRUE:
        case OP_JUMP_IF_NOT_EQUAL:
        case OP_JUMP_IF_EQUAL:
        case OP_JUMP_IF_NOT_GREATER:
        case OP_JUMP_IF_NOT_LESS: {
            uint16_t offset = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            int target = ip + offset;
            if (target <= code_len) targets[target] = true;
            break;
        }
                                /* 16-bit backward jump */
        case OP_LOOP: {
            uint16_t offset = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            int target = ip - offset;
            if (target >= 0) targets[target] = true;
            break;
        }

                    /* Opcodes with operands Ã¢â‚¬â€ skip their bytes */
        case OP_CONSTANT: case OP_GET_LOCAL: case OP_SET_LOCAL:
        case OP_GET_GLOBAL: case OP_DEFINE_GLOBAL: case OP_SET_GLOBAL:
        case OP_GET_UPVALUE: case OP_GET_UPVALUE_OPEN:
        case OP_GET_UPVALUE_CLOSED: case OP_GET_UPVALUE_IMMUTABLE:
        case OP_SET_UPVALUE: case OP_SET_UPVALUE_OPEN:
        case OP_SET_UPVALUE_CLOSED:
        case OP_INC_LOCAL_POP: case OP_INC_LOCAL:
        case OP_CALL: case OP_TAIL_CALL:
        case OP_POP_N: case OP_FIELD:
        case OP_GET_FIELD_THIS: case OP_SET_FIELD_THIS:
        case OP_CLASS: case OP_BUILD_LIST: case OP_BUILD_TABLE:
        case OP_IMPORT: case OP_DO_NEW:
            ip += 1; break;

        case OP_CONSTANT_LONG:
        case OP_GET_GLOBAL_LONG: case OP_DEFINE_GLOBAL_LONG:
        case OP_SET_GLOBAL_LONG: case OP_GET_SUPER_LONG:
        case OP_CLASS_LONG: case OP_IMPORT_LONG:
        case OP_GET_PROPERTY_IC: case OP_SET_PROPERTY_IC:
        case OP_METHOD: case OP_DO_INVOKE:
            ip += 2; break;

        case OP_GET_SUPER:
            ip += 1; break;

        case OP_INVOKE: case OP_TAIL_INVOKE:
        case OP_SUPER_INVOKE: case OP_TAIL_SUPER_INVOKE:
            ip += 2; break;

        case OP_INVOKE_LONG: case OP_TAIL_INVOKE_LONG:
        case OP_SUPER_INVOKE_LONG: case OP_TAIL_SUPER_INVOKE_LONG:
        case OP_INVOKE_IC: case OP_TAIL_INVOKE_IC:
        case OP_METHOD_LONG:
            ip += 3; break;

            /* Invoke_N and tail invoke_N: 1 operand byte */
        case OP_INVOKE_0: case OP_INVOKE_1: case OP_INVOKE_2:
        case OP_INVOKE_3: case OP_INVOKE_4: case OP_INVOKE_5:
        case OP_INVOKE_6: case OP_INVOKE_7: case OP_INVOKE_8:
        case OP_TAIL_INVOKE_0: case OP_TAIL_INVOKE_1: case OP_TAIL_INVOKE_2:
        case OP_TAIL_INVOKE_3: case OP_TAIL_INVOKE_4: case OP_TAIL_INVOKE_5:
        case OP_TAIL_INVOKE_6: case OP_TAIL_INVOKE_7: case OP_TAIL_INVOKE_8:
        case OP_SUPER_INVOKE_0: case OP_SUPER_INVOKE_1: case OP_SUPER_INVOKE_2:
        case OP_SUPER_INVOKE_3: case OP_SUPER_INVOKE_4: case OP_SUPER_INVOKE_5:
        case OP_SUPER_INVOKE_6: case OP_SUPER_INVOKE_7: case OP_SUPER_INVOKE_8:
        case OP_TAIL_SUPER_INVOKE_0: case OP_TAIL_SUPER_INVOKE_1: case OP_TAIL_SUPER_INVOKE_2:
        case OP_TAIL_SUPER_INVOKE_3: case OP_TAIL_SUPER_INVOKE_4: case OP_TAIL_SUPER_INVOKE_5:
        case OP_TAIL_SUPER_INVOKE_6: case OP_TAIL_SUPER_INVOKE_7: case OP_TAIL_SUPER_INVOKE_8:
            ip += 1; break;

            /* Closure: variable-length */
        case OP_CLOSURE: {
            uint8_t fn_idx = code[ip++];
            Value fn_val = fn->chunk.constants.values[fn_idx];
            if (IS_OBJ(fn_val) && OBJ_TYPE(fn_val) == OBJ_FUNCTION) {
                ObjFunction* child = AS_FUNCTION(fn_val);
                ip += child->upvalueCount * 3;
            }
            break;
        }
        case OP_CLOSURE_LONG: {
            uint16_t fn_idx = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            Value fn_val = fn->chunk.constants.values[fn_idx];
            if (IS_OBJ(fn_val) && OBJ_TYPE(fn_val) == OBJ_FUNCTION) {
                ObjFunction* child = AS_FUNCTION(fn_val);
                ip += child->upvalueCount * 3;
            }
            break;
        }

                            /* Zero-operand opcodes: everything else */
        default:
            break;
        }
    }
}

/* ================================================================
 * Peephole pattern matching
 *
 * We look ahead in the bytecode to detect common multi-opcode
 * sequences and fuse them into single C statements that avoid
 * intermediate stack traffic.
 * ================================================================ */

 /* Check if byte at offset is a GET_LOCAL_N opcode, return slot or -1 */
static int is_get_local_at(uint8_t* code, int ip, int code_len) {
    if (ip >= code_len) return -1;
    uint8_t op = code[ip];
    if (op >= OP_GET_LOCAL_0 && op <= OP_GET_LOCAL_7)
        return op - OP_GET_LOCAL_0;
    if (op == OP_GET_LOCAL && ip + 1 < code_len)
        return code[ip + 1]; /* slot in next byte, but return -2 to signal "has operand" */
    return -1;
}

/* Check if byte at offset is a SET_LOCAL_N_POP, return slot or -1 */
static int is_set_local_pop_at(uint8_t* code, int ip, int code_len) {
    if (ip >= code_len) return -1;
    uint8_t op = code[ip];
    if (op >= OP_SET_LOCAL_0_POP && op <= OP_SET_LOCAL_7_POP)
        return op - OP_SET_LOCAL_0_POP;
    return -1;
}

/* ================================================================
 * SYNC/UNSYNC macros
 *
 * We keep sp as a C local for speed. Before calling into the VM
 * (callValue, runtimeError, GC-triggering ops) we must sync.
 * After returning, we reload.
 * ================================================================ */

static void emit_sync(Transpiler* t) {
    OUT(t, "    vm->stackTop = sp;\n");
}

static void emit_reload(Transpiler* t) {
    OUT(t, "    sp = vm->stackTop;\n");
    OUT(t, "    frame = &vm->frames[vm->frameCount - 1];\n");
    OUT(t, "    slots = frame->slots;\n");
}

/* Full sync + reload bracketing for calls that modify the frame */
static void emit_call_bracket_open(Transpiler* t) {
    emit_sync(t);
}

static void emit_call_bracket_close(Transpiler* t) {
    emit_reload(t);
}

/* Emit an optimized call: closure fast path (btl_call_direct) with
 * fallback to btl_call_and_run for non-closure callees. */
static void emit_optimized_call(Transpiler* t, int argc) {
    OUT(t, "    { Value _callee = vm->stackTop[-%d];\n", argc + 1);
    OUT(t, "      InterpretResult _r;\n");
    OUT(t, "      if (IS_OBJ(_callee) && OBJ_TYPE(_callee) == OBJ_CLOSURE)\n");
    OUT(t, "          _r = btl_call_direct(vm, AS_CLOSURE(_callee), %d);\n", argc);
    OUT(t, "      else\n");
    OUT(t, "          _r = btl_call_and_run(vm, _callee, %d);\n", argc);
    OUT(t, "      if (_r != INTERPRET_OK) return _r; }\n");
}

/* ================================================================
 * File header emission
 * ================================================================ */

static void emit_header(Transpiler* t) {
    int fn_count = t->fns.count;

    OUT(t, "/*\n");
    OUT(t, " * Generated by BTL transpiler v2 (performance) - DO NOT EDIT\n");
    OUT(t, " */\n\n");
    OUT(t, "#include \"compiled.h\"\n\n");
    OUT(t, "/* Inline stack operations - avoid function call overhead */\n");
    OUT(t, "#define PUSH(sp, v)  (*(sp)++ = (v))\n");
    OUT(t, "#define POP(sp)      (*--(sp))\n");
    OUT(t, "#define PEEK(sp, n)  ((sp)[-(n)-1])\n");
    OUT(t, "#define PEEK_SET(sp, n, v) ((sp)[-(n)-1] = (v))\n");
    NL(t);

    /* Forward declare all transpiled functions */
    for (int i = 0; i < fn_count; i++) {
        OUT(t, "static InterpretResult btl_fn_%d(VM* vm);\n", i);
    }
    NL(t);

    /* ---- Direct dispatch table ----
     * Maps ObjFunction* (from compile()) to transpiled C function pointers.
     * Populated at startup by btl_register_functions(). Looked up at every
     * call site to bypass the interpreter dispatch loop. */
    OUT(t, "/* ---- Direct dispatch table ---- */\n");
    OUT(t, "typedef InterpretResult (*BtlFnPtr)(VM*);\n");
    OUT(t, "#define BTL_FN_COUNT %d\n", fn_count);
    OUT(t, "static struct { ObjFunction* fn; BtlFnPtr handler; } btl_dispatch[BTL_FN_COUNT];\n\n");

    /* Lookup: scan the (small) table for a matching ObjFunction* */
    OUT(t, "static inline BtlFnPtr btl_find_handler(ObjFunction* target) {\n");
    OUT(t, "    for (int i = 0; i < BTL_FN_COUNT; i++) {\n");
    OUT(t, "        if (btl_dispatch[i].fn == target) return btl_dispatch[i].handler;\n");
    OUT(t, "    }\n");
    OUT(t, "    return NULL;\n");
    OUT(t, "}\n\n");

    /* Registration: walk the constant pool tree (same order as collect_functions)
     * and pair each ObjFunction* with its btl_fn_N handler.
     * We emit a static array of handlers indexed by fn_id, then walk the
     * function tree to fill btl_dispatch[]. */
    OUT(t, "static void btl_register_recursive(ObjFunction* fn, int* idx, BtlFnPtr* handlers) {\n");
    OUT(t, "    if (fn == NULL) return;\n");
    OUT(t, "    /* Check if already registered */\n");
    OUT(t, "    for (int i = 0; i < *idx; i++)\n");
    OUT(t, "        if (btl_dispatch[i].fn == fn) return;\n");
    OUT(t, "    if (*idx >= BTL_FN_COUNT) return;\n");
    OUT(t, "    btl_dispatch[*idx].fn = fn;\n");
    OUT(t, "    btl_dispatch[*idx].handler = handlers[*idx];\n");
    OUT(t, "    (*idx)++;\n");
    OUT(t, "    /* Recurse into constant pool for nested functions */\n");
    OUT(t, "    for (int i = 0; i < fn->chunk.constants.count; i++) {\n");
    OUT(t, "        Value v = fn->chunk.constants.values[i];\n");
    OUT(t, "        if (IS_OBJ(v) && OBJ_TYPE(v) == OBJ_FUNCTION)\n");
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
     * For closure calls: look up the function in the dispatch table.
     * If found, push the frame via callValue then run the transpiled C
     * function directly instead of entering the interpreter loop. */
    OUT(t, "/*\n");
    OUT(t, " * Frame-guarded call with direct dispatch.\n");
    OUT(t, " * If callee is a closure with a transpiled handler, we call it directly.\n");
    OUT(t, " * Otherwise fall back to callValue() + run() (interpreter).\n");
    OUT(t, " */\n");
    OUT(t, "static inline InterpretResult btl_call_and_run(VM* vm, Value callee, int argc) {\n");
    OUT(t, "    int fc = vm->frameCount;\n");
    OUT(t, "    if (!callValue(vm, callee, argc)) return INTERPRET_RUNTIME_ERROR;\n");
    OUT(t, "    if (vm->frameCount > fc) {\n");
    OUT(t, "        /* A new frame was pushed -- need to execute it */\n");
    OUT(t, "        ObjFunction* target = vm->frames[fc].closure->function;\n");
    OUT(t, "        BtlFnPtr handler = btl_find_handler(target);\n");
    OUT(t, "        if (handler) {\n");
    OUT(t, "            return handler(vm);\n");
    OUT(t, "        }\n");
    OUT(t, "        /* Unknown function -- fall back to interpreter */\n");
    OUT(t, "        int savedFloor = vm->runFloor;\n");
    OUT(t, "        vm->runFloor = fc;\n");
    OUT(t, "        InterpretResult r = run(vm);\n");
    OUT(t, "        vm->runFloor = savedFloor;\n");
    OUT(t, "        if (r != INTERPRET_OK) return r;\n");
    OUT(t, "    }\n");
    OUT(t, "    return INTERPRET_OK;\n");
    OUT(t, "}\n\n");

    /* Fast path for closure calls: inline frame setup + dispatch table lookup.
     * Skips callValue's type dispatch entirely for the common closure case. */
    OUT(t, "/*\n");
    OUT(t, " * Direct closure call: inline frame setup + transpiled dispatch.\n");
    OUT(t, " * Bypasses callValue() entirely for known closure targets.\n");
    OUT(t, " */\n");
    OUT(t, "static inline InterpretResult btl_call_direct(VM* vm, ObjClosure* closure, int argc) {\n");
    OUT(t, "    if (__builtin_expect(argc != closure->function->arity, 0)) {\n");
    OUT(t, "        runtimeError(vm, \"Expected %%d arguments but got %%d.\",\n");
    OUT(t, "                     closure->function->arity, argc);\n");
    OUT(t, "        return INTERPRET_RUNTIME_ERROR;\n");
    OUT(t, "    }\n");
    OUT(t, "    if (__builtin_expect(vm->frameCount == FRAMES_MAX, 0)) {\n");
    OUT(t, "        runtimeError(vm, \"Stack overflow.\");\n");
    OUT(t, "        return INTERPRET_RUNTIME_ERROR;\n");
    OUT(t, "    }\n");
    OUT(t, "    /* Inline frame push (same as vm.c call()) */\n");
    OUT(t, "    CallFrame* newFrame = &vm->frames[vm->frameCount++];\n");
    OUT(t, "    newFrame->closure = closure;\n");
    OUT(t, "    newFrame->ip = closure->function->chunk.code;\n");
    OUT(t, "    newFrame->slots = vm->stackTop - argc - 1;\n");
    OUT(t, "    newFrame->openUpvalues = NULL;\n");
    OUT(t, "    /* Dispatch to transpiled handler or interpreter */\n");
    OUT(t, "    BtlFnPtr handler = btl_find_handler(closure->function);\n");
    OUT(t, "    if (handler) return handler(vm);\n");
    OUT(t, "    int savedFloor = vm->runFloor;\n");
    OUT(t, "    vm->runFloor = vm->frameCount - 1;\n");
    OUT(t, "    InterpretResult r = run(vm);\n");
    OUT(t, "    vm->runFloor = savedFloor;\n");
    OUT(t, "    return r;\n");
    OUT(t, "}\n\n");
}

/* ================================================================
 * Try to fuse a common opcode pattern. Returns number of bytes
 * consumed (0 if no pattern matched, meaning fall through to
 * single-opcode emit).
 *
 * PATTERN 1: GET_LOCAL(a) GET_LOCAL(b) <arith> SET_LOCAL_N_POP(c)
 *   Ã¢â€ â€™ slots[c] = NUMBER_VAL(AS_NUMBER(slots[a]) OP AS_NUMBER(slots[b]))
 *   Eliminates 4 push/pop operations.
 *
 * PATTERN 2: GET_LOCAL(a) GET_LOCAL(b) LESS/GREATER POP_JUMP_IF_FALSE
 *   Ã¢â€ â€™ if (!(AS_NUMBER(slots[a]) < AS_NUMBER(slots[b]))) goto L;
 *   Eliminates 3 push/pop + a falsey check.
 *
 * PATTERN 3: GET_LOCAL(a) <const> <arith> SET_LOCAL_N_POP(a)
 *   Ã¢â€ â€™ slots[a] = NUMBER_VAL(AS_NUMBER(slots[a]) OP const)
 *   Common in loops like `i = i + 1`.
 * ================================================================ */

static int try_fuse(Transpiler* t, uint8_t* code, int ip, int code_len,
    bool* targets) {
    /* Don't fuse across jump targets Ã¢â‚¬â€ any instruction that's a jump
       target must be emittable standalone */

       /* Need at least 4 opcodes to fuse */
    if (ip + 3 >= code_len) return 0;

    uint8_t op0 = code[ip];
    int slot_a, slot_b;

    /* ---- PATTERN 1 & 2: GET_LOCAL(a) GET_LOCAL(b) <op> <consume> ---- */
    slot_a = is_get_local_at(code, ip, code_len);
    if (slot_a < 0) return 0;

    int size0 = (op0 == OP_GET_LOCAL) ? 2 : 1;
    if (op0 == OP_GET_LOCAL) slot_a = code[ip + 1];

    int ip1 = ip + size0;
    if (ip1 >= code_len || targets[ip1]) return 0;

    slot_b = is_get_local_at(code, ip1, code_len);
    if (slot_b < 0) return 0;

    uint8_t op1 = code[ip1];
    int size1 = (op1 == OP_GET_LOCAL) ? 2 : 1;
    if (op1 == OP_GET_LOCAL) slot_b = code[ip1 + 1];

    int ip2 = ip1 + size1;
    if (ip2 >= code_len || targets[ip2]) return 0;

    uint8_t op2 = code[ip2];

    /* Check if op2 is an arithmetic/comparison op */
    const char* c_op = NULL;
    bool is_comparison = false;
    switch (op2) {
    case OP_ADD:      c_op = "+"; break;
    case OP_SUBTRACT: c_op = "-"; break;
    case OP_MULTIPLY: c_op = "*"; break;
    case OP_DIVIDE:   c_op = "/"; break;
    case OP_LESS:     c_op = "<";  is_comparison = true; break;
    case OP_GREATER:  c_op = ">";  is_comparison = true; break;
    case OP_EQUAL:    c_op = NULL; is_comparison = true; break;
    default: return 0;
    }

    int ip3 = ip2 + 1;
    if (ip3 >= code_len || targets[ip3]) return 0;

    uint8_t op3 = code[ip3];

    /* PATTERN 1: arith + SET_LOCAL_POP */
    if (!is_comparison) {
        int slot_c = is_set_local_pop_at(code, ip3, code_len);
        if (slot_c >= 0) {
            emit_comment(t, ip, "FUSED: arith assign");
            OUT(t, "    slots[%d] = NUMBER_VAL(AS_NUMBER(slots[%d]) %s AS_NUMBER(slots[%d]));\n",
                slot_c, slot_a, c_op, slot_b);
            return (ip3 + 1) - ip; /* total bytes consumed */
        }
    }

    /* PATTERN 2: comparison + POP_JUMP_IF_FALSE */
    if (is_comparison && op3 == OP_POP_JUMP_IF_FALSE && ip3 + 3 <= code_len) {
        uint16_t offset = (uint16_t) ((code[ip3 + 1] << 8) | code[ip3 + 2]);
        int target_ip = ip3 + 3 + offset;
        emit_comment(t, ip, "FUSED: compare+branch");
        if (op2 == OP_EQUAL) {
            OUT(t, "    if (!valuesEqual(slots[%d], slots[%d])) goto L_%04d;\n",
                slot_a, slot_b, target_ip);
        } else {
            OUT(t, "    if (!(AS_NUMBER(slots[%d]) %s AS_NUMBER(slots[%d]))) goto L_%04d;\n",
                slot_a, c_op, slot_b, target_ip);
        }
        return (ip3 + 3) - ip;
    }

    /* PATTERN 2b: comparison + POP_JUMP_IF_TRUE */
    if (is_comparison && op3 == OP_POP_JUMP_IF_TRUE && ip3 + 3 <= code_len) {
        uint16_t offset = (uint16_t) ((code[ip3 + 1] << 8) | code[ip3 + 2]);
        int target_ip = ip3 + 3 + offset;
        emit_comment(t, ip, "FUSED: compare+branch_true");
        if (op2 == OP_EQUAL) {
            OUT(t, "    if (valuesEqual(slots[%d], slots[%d])) goto L_%04d;\n",
                slot_a, slot_b, target_ip);
        } else {
            OUT(t, "    if (AS_NUMBER(slots[%d]) %s AS_NUMBER(slots[%d])) goto L_%04d;\n",
                slot_a, c_op, slot_b, target_ip);
        }
        return (ip3 + 3) - ip;
    }

    return 0;
}

/* ================================================================
 * PATTERN 3: GET_LOCAL(a) <const_push> <arith> SET_LOCAL_N_POP(c)
 *   → slots[c] = NUMBER_VAL(AS_NUMBER(slots[a]) OP <const>)
 *   Common in loops like `i = i + 1`, `x = x * 2`.
 *
 * PATTERN 4: GET_LOCAL(a) <const_push> <compare> POP_JUMP_IF_FALSE
 *   → if (!(AS_NUMBER(slots[a]) < <const>)) goto L;
 *   Common in `while (i < 10)` style loops.
 * ================================================================ */

static int try_fuse_local_const(Transpiler* t, ObjFunction* fn, uint8_t* code,
    int ip, int code_len, bool* targets) {
    if (ip + 3 >= code_len) return 0;

    uint8_t op0 = code[ip];
    int slot_a = is_get_local_at(code, ip, code_len);
    if (slot_a < 0) return 0;

    int size0 = (op0 == OP_GET_LOCAL) ? 2 : 1;
    if (op0 == OP_GET_LOCAL) slot_a = code[ip + 1];

    int ip1 = ip + size0;
    if (ip1 >= code_len || targets[ip1]) return 0;

    /* Try to match a constant push at ip1 */
    uint8_t cop = code[ip1];
    char const_expr[64];
    int const_size;

    if (cop == OP_0) {
        snprintf(const_expr, sizeof(const_expr), "0.0");
        const_size = 1;
    } else if (cop == OP_1) {
        snprintf(const_expr, sizeof(const_expr), "1.0");
        const_size = 1;
    } else if (cop == OP_2) {
        snprintf(const_expr, sizeof(const_expr), "2.0");
        const_size = 1;
    } else if (cop == OP_CONSTANT && ip1 + 1 < code_len) {
        uint8_t cidx = code[ip1 + 1];
        Value cval = fn->chunk.constants.values[cidx];
        if (!IS_NUMBER(cval)) return 0;
        snprintf(const_expr, sizeof(const_expr), "fn->chunk.constants.values[%d]", cidx);
        const_size = 2;
    } else {
        return 0;
    }

    int ip2 = ip1 + const_size;
    if (ip2 >= code_len || targets[ip2]) return 0;

    uint8_t arith_op = code[ip2];
    const char* c_op = NULL;
    bool is_comparison = false;
    switch (arith_op) {
    case OP_ADD:      c_op = "+"; break;
    case OP_SUBTRACT: c_op = "-"; break;
    case OP_MULTIPLY: c_op = "*"; break;
    case OP_DIVIDE:   c_op = "/"; break;
    case OP_LESS:     c_op = "<";  is_comparison = true; break;
    case OP_GREATER:  c_op = ">";  is_comparison = true; break;
    default: return 0;
    }

    int ip3 = ip2 + 1;
    if (ip3 >= code_len || targets[ip3]) return 0;

    /* PATTERN 3: arith + SET_LOCAL_POP */
    if (!is_comparison) {
        int slot_c = is_set_local_pop_at(code, ip3, code_len);
        if (slot_c >= 0) {
            /* For numeric literals, emit the value directly */
            if (cop == OP_0 || cop == OP_1 || cop == OP_2) {
                emit_comment(t, ip, "FUSED: local op= const");
                OUT(t, "    slots[%d] = NUMBER_VAL(AS_NUMBER(slots[%d]) %s %s);\n",
                    slot_c, slot_a, c_op, const_expr);
            } else {
                emit_comment(t, ip, "FUSED: local op= const");
                OUT(t, "    slots[%d] = NUMBER_VAL(AS_NUMBER(slots[%d]) %s AS_NUMBER(%s));\n",
                    slot_c, slot_a, c_op, const_expr);
            }
            return (ip3 + 1) - ip;
        }
    }

    /* PATTERN 4: comparison + POP_JUMP_IF_FALSE */
    uint8_t op3 = code[ip3];
    if (is_comparison && op3 == OP_POP_JUMP_IF_FALSE && ip3 + 3 <= code_len) {
        uint16_t offset = (uint16_t) ((code[ip3 + 1] << 8) | code[ip3 + 2]);
        int target_ip = ip3 + 3 + offset;
        if (cop == OP_0 || cop == OP_1 || cop == OP_2) {
            emit_comment(t, ip, "FUSED: local cmp const + branch");
            OUT(t, "    if (!(AS_NUMBER(slots[%d]) %s %s)) goto L_%04d;\n",
                slot_a, c_op, const_expr, target_ip);
        } else {
            emit_comment(t, ip, "FUSED: local cmp const + branch");
            OUT(t, "    if (!(AS_NUMBER(slots[%d]) %s AS_NUMBER(%s))) goto L_%04d;\n",
                slot_a, c_op, const_expr, target_ip);
        }
        return (ip3 + 3) - ip;
    }

    /* PATTERN 4b: comparison + POP_JUMP_IF_TRUE */
    if (is_comparison && op3 == OP_POP_JUMP_IF_TRUE && ip3 + 3 <= code_len) {
        uint16_t offset = (uint16_t) ((code[ip3 + 1] << 8) | code[ip3 + 2]);
        int target_ip = ip3 + 3 + offset;
        if (cop == OP_0 || cop == OP_1 || cop == OP_2) {
            emit_comment(t, ip, "FUSED: local cmp const + branch_true");
            OUT(t, "    if (AS_NUMBER(slots[%d]) %s %s) goto L_%04d;\n",
                slot_a, c_op, const_expr, target_ip);
        } else {
            emit_comment(t, ip, "FUSED: local cmp const + branch_true");
            OUT(t, "    if (AS_NUMBER(slots[%d]) %s AS_NUMBER(%s)) goto L_%04d;\n",
                slot_a, c_op, const_expr, target_ip);
        }
        return (ip3 + 3) - ip;
    }

    return 0;
}

/* ================================================================
 * Emit a single function body
 * ================================================================ */

static void emit_function(Transpiler* t, ObjFunction* fn, int fn_id) {
    Chunk* chunk = &fn->chunk;
    uint8_t* code = chunk->code;
    int code_len = chunk->count;

    t->current_fn = fn;
    t->current_fn_id = fn_id;

    /* First pass: collect jump targets */
    bool* targets = calloc(code_len + 1, sizeof(bool));
    collect_jump_targets(fn, targets, code_len);

    /* Function signature */
    const char* name = fn->name ? fn->name->chars : "<script>";
    OUT(t, "/* BTL function: %s (arity=%d, upvalues=%d) */\n", name, fn->arity, fn->upvalueCount);
    OUT(t, "static InterpretResult btl_fn_%d(VM* vm) {\n", fn_id);

    /* ---- Cached state in C locals ----
     * These live in registers. We sync back to vm->stackTop only
     * at call boundaries.
     *
     * 'sp' is our local stack pointer. It shadows vm->stackTop.
     * 'slots' is frame->slots (base of current frame's stack window).
     * 'frame' is the current CallFrame pointer.
     */
    OUT(t, "    register Value* sp = vm->stackTop;\n");
    OUT(t, "    CallFrame* frame = &vm->frames[vm->frameCount - 1];\n");
    OUT(t, "    Value* slots = frame->slots;\n");
    OUT(t, "    ObjFunction* fn = frame->closure->function;\n");
    OUT(t, "    ObjModule* mod = fn->module;\n");
    OUT(t, "    (void)fn; (void)mod; (void)slots;\n");
    NL(t);

    /* Second pass: emit C code */
    int ip = 0;
    while (ip < code_len) {
        /* Emit label if this is a jump target */
        if (targets[ip]) {
            emit_label(t, ip);
        }

        int start_ip = ip;

        /* ---- Try fused patterns first ---- */
        if (!targets[ip]) {  /* can't fuse if this ip is a jump target */
            int fused = try_fuse(t, code, ip, code_len, targets);
            if (fused > 0) {
                ip += fused;
                continue;
            }
            /* Try local+const patterns (i = i + 1, while i < 10, etc.) */
            fused = try_fuse_local_const(t, fn, code, ip, code_len, targets);
            if (fused > 0) {
                ip += fused;
                continue;
            }
        }

        uint8_t op = code[ip++];

        switch (op) {

            /* ================================================================
             * CONSTANTS & LITERALS
             *
             * Direct stack writes via sp Ã¢â‚¬â€ no function call.
             * ================================================================ */
        case OP_CONSTANT: {
            uint8_t idx = code[ip++];
            emit_comment(t, start_ip, "OP_CONSTANT");
            OUT(t, "    PUSH(sp, fn->chunk.constants.values[%d]);\n", idx);
            break;
        }
        case OP_CONSTANT_LONG: {
            uint16_t idx = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            emit_comment(t, start_ip, "OP_CONSTANT_LONG");
            OUT(t, "    PUSH(sp, fn->chunk.constants.values[%d]);\n", idx);
            break;
        }
        case OP_NULL:
            emit_comment(t, start_ip, "OP_NULL");
            OUT(t, "    PUSH(sp, NULL_VAL);\n");
            break;
        case OP_TRUE:
            emit_comment(t, start_ip, "OP_TRUE");
            OUT(t, "    PUSH(sp, TRUE_VAL);\n");
            break;
        case OP_FALSE:
            emit_comment(t, start_ip, "OP_FALSE");
            OUT(t, "    PUSH(sp, FALSE_VAL);\n");
            break;
        case OP_0:
            emit_comment(t, start_ip, "OP_0");
            OUT(t, "    PUSH(sp, NUMBER_VAL(0.0));\n");
            break;
        case OP_1:
            emit_comment(t, start_ip, "OP_1");
            OUT(t, "    PUSH(sp, NUMBER_VAL(1.0));\n");
            break;
        case OP_2:
            emit_comment(t, start_ip, "OP_2");
            OUT(t, "    PUSH(sp, NUMBER_VAL(2.0));\n");
            break;

            /* ================================================================
             * STACK MANIPULATION
             * ================================================================ */
        case OP_POP:
            emit_comment(t, start_ip, "OP_POP");
            OUT(t, "    sp--;\n");
            break;
        case OP_POP_N: {
            uint8_t n = code[ip++];
            emit_comment(t, start_ip, "OP_POP_N");
            OUT(t, "    sp -= %d;\n", n);
            break;
        }
        case OP_DUP:
            emit_comment(t, start_ip, "OP_DUP");
            OUT(t, "    sp[0] = sp[-1]; sp++;\n");
            break;
        case OP_SWAP:
            emit_comment(t, start_ip, "OP_SWAP");
            OUT(t, "    { Value _t = sp[-1]; sp[-1] = sp[-2]; sp[-2] = _t; }\n");
            break;

            /* ================================================================
             * LOCAL VARIABLES
             *
             * Direct slots[] access Ã¢â‚¬â€ no push(vm,...) overhead.
             * ================================================================ */
        case OP_GET_LOCAL: {
            uint8_t slot = code[ip++];
            emit_comment(t, start_ip, "OP_GET_LOCAL");
            OUT(t, "    PUSH(sp, slots[%d]);\n", slot);
            break;
        }
        case OP_GET_LOCAL_0: case OP_GET_LOCAL_1: case OP_GET_LOCAL_2:
        case OP_GET_LOCAL_3: case OP_GET_LOCAL_4: case OP_GET_LOCAL_5:
        case OP_GET_LOCAL_6: case OP_GET_LOCAL_7: {
            int slot = op - OP_GET_LOCAL_0;
            emit_comment(t, start_ip, "OP_GET_LOCAL_N");
            OUT(t, "    PUSH(sp, slots[%d]);\n", slot);
            break;
        }
        case OP_SET_LOCAL: {
            uint8_t slot = code[ip++];
            emit_comment(t, start_ip, "OP_SET_LOCAL");
            OUT(t, "    slots[%d] = sp[-1];\n", slot);
            break;
        }
        case OP_SET_LOCAL_0: case OP_SET_LOCAL_1: case OP_SET_LOCAL_2:
        case OP_SET_LOCAL_3: case OP_SET_LOCAL_4: case OP_SET_LOCAL_5:
        case OP_SET_LOCAL_6: case OP_SET_LOCAL_7: {
            int slot = op - OP_SET_LOCAL_0;
            emit_comment(t, start_ip, "OP_SET_LOCAL_N");
            OUT(t, "    slots[%d] = sp[-1];\n", slot);
            break;
        }
        case OP_SET_LOCAL_0_POP: case OP_SET_LOCAL_1_POP: case OP_SET_LOCAL_2_POP:
        case OP_SET_LOCAL_3_POP: case OP_SET_LOCAL_4_POP: case OP_SET_LOCAL_5_POP:
        case OP_SET_LOCAL_6_POP: case OP_SET_LOCAL_7_POP: {
            int slot = op - OP_SET_LOCAL_0_POP;
            emit_comment(t, start_ip, "OP_SET_LOCAL_N_POP");
            OUT(t, "    slots[%d] = POP(sp);\n", slot);
            break;
        }

                               /* ================================================================
                                * INCREMENT / DECREMENT
                                * ================================================================ */
        case OP_INC_LOCAL_POP: {
            uint8_t slot = code[ip++];
            emit_comment(t, start_ip, "OP_INC_LOCAL_POP");
            OUT(t, "    slots[%d] = NUMBER_VAL(AS_NUMBER(slots[%d]) + 1.0);\n", slot, slot);
            break;
        }
        case OP_INC_LOCAL: {
            uint8_t slot = code[ip++];
            emit_comment(t, start_ip, "OP_INC_LOCAL");
            OUT(t, "    { double _v = AS_NUMBER(slots[%d]) + 1.0; slots[%d] = NUMBER_VAL(_v); PUSH(sp, NUMBER_VAL(_v)); }\n", slot, slot);
            break;
        }
        case OP_INCREMENT:
            emit_comment(t, start_ip, "OP_INCREMENT");
            OUT(t, "    sp[-1] = NUMBER_VAL(AS_NUMBER(sp[-1]) + 1.0);\n");
            break;
        case OP_DECREMENT:
            emit_comment(t, start_ip, "OP_DECREMENT");
            OUT(t, "    sp[-1] = NUMBER_VAL(AS_NUMBER(sp[-1]) - 1.0);\n");
            break;

            /* ================================================================
             * GLOBAL VARIABLES
             *
             * Must sync sp before runtimeError (it accesses the stack).
             * ================================================================ */
        case OP_GET_GLOBAL: {
            uint8_t idx = code[ip++];
            emit_comment(t, start_ip, "OP_GET_GLOBAL");
            OUT(t, "    { Value _g = mod->globalValues.values[%d];\n", idx);
            OUT(t, "      if (__builtin_expect(IS_EMPTY(_g), 0)) { ");
            emit_sync(t);
            OUT(t, " runtimeError(vm, \"Undefined variable.\"); return INTERPRET_RUNTIME_ERROR; }\n");
            OUT(t, "      PUSH(sp, _g); }\n");
            break;
        }
        case OP_GET_GLOBAL_LONG: {
            uint16_t idx = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            emit_comment(t, start_ip, "OP_GET_GLOBAL_LONG");
            OUT(t, "    { Value _g = mod->globalValues.values[%d];\n", idx);
            OUT(t, "      if (__builtin_expect(IS_EMPTY(_g), 0)) { ");
            emit_sync(t);
            OUT(t, " runtimeError(vm, \"Undefined variable.\"); return INTERPRET_RUNTIME_ERROR; }\n");
            OUT(t, "      PUSH(sp, _g); }\n");
            break;
        }
        case OP_DEFINE_GLOBAL: {
            uint8_t idx = code[ip++];
            emit_comment(t, start_ip, "OP_DEFINE_GLOBAL");
            OUT(t, "    mod->globalValues.values[%d] = POP(sp);\n", idx);
            break;
        }
        case OP_DEFINE_GLOBAL_LONG: {
            uint16_t idx = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            emit_comment(t, start_ip, "OP_DEFINE_GLOBAL_LONG");
            OUT(t, "    mod->globalValues.values[%d] = POP(sp);\n", idx);
            break;
        }
        case OP_SET_GLOBAL: {
            uint8_t idx = code[ip++];
            emit_comment(t, start_ip, "OP_SET_GLOBAL");
            OUT(t, "    if (__builtin_expect(IS_EMPTY(mod->globalValues.values[%d]), 0)) { ", idx);
            emit_sync(t);
            OUT(t, " runtimeError(vm, \"Undefined variable.\"); return INTERPRET_RUNTIME_ERROR; }\n");
            OUT(t, "    mod->globalValues.values[%d] = sp[-1];\n", idx);
            break;
        }
        case OP_SET_GLOBAL_LONG: {
            uint16_t idx = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            emit_comment(t, start_ip, "OP_SET_GLOBAL_LONG");
            OUT(t, "    if (__builtin_expect(IS_EMPTY(mod->globalValues.values[%d]), 0)) { ", idx);
            emit_sync(t);
            OUT(t, " runtimeError(vm, \"Undefined variable.\"); return INTERPRET_RUNTIME_ERROR; }\n");
            OUT(t, "    mod->globalValues.values[%d] = sp[-1];\n", idx);
            break;
        }

                               /* ================================================================
                                * UPVALUES
                                * ================================================================ */
        case OP_GET_UPVALUE: {
            uint8_t slot = code[ip++];
            emit_comment(t, start_ip, "OP_GET_UPVALUE");
            OUT(t, "    { RuntimeUpvalue* _uv = &frame->closure->upvalues[%d];\n", slot);
            OUT(t, "      PUSH(sp, _uv->isOpen ? *_uv->loc.stack : (_uv->isMutable ? _uv->loc.box->closed : _uv->loc.immValue)); }\n");
            break;
        }
        case OP_GET_UPVALUE_OPEN: {
            uint8_t slot = code[ip++];
            emit_comment(t, start_ip, "OP_GET_UPVALUE_OPEN");
            OUT(t, "    PUSH(sp, *frame->closure->upvalues[%d].loc.stack);\n", slot);
            break;
        }
        case OP_GET_UPVALUE_CLOSED: {
            uint8_t slot = code[ip++];
            emit_comment(t, start_ip, "OP_GET_UPVALUE_CLOSED");
            OUT(t, "    PUSH(sp, frame->closure->upvalues[%d].loc.box->closed);\n", slot);
            break;
        }
        case OP_GET_UPVALUE_IMMUTABLE: {
            uint8_t slot = code[ip++];
            emit_comment(t, start_ip, "OP_GET_UPVALUE_IMMUTABLE");
            OUT(t, "    PUSH(sp, frame->closure->upvalues[%d].loc.immValue);\n", slot);
            break;
        }
        case OP_SET_UPVALUE: {
            uint8_t slot = code[ip++];
            emit_comment(t, start_ip, "OP_SET_UPVALUE");
            OUT(t, "    { RuntimeUpvalue* _uv = &frame->closure->upvalues[%d];\n", slot);
            OUT(t, "      if (_uv->isOpen) *_uv->loc.stack = sp[-1]; else _uv->loc.box->closed = sp[-1]; }\n");
            break;
        }
        case OP_SET_UPVALUE_OPEN: {
            uint8_t slot = code[ip++];
            emit_comment(t, start_ip, "OP_SET_UPVALUE_OPEN");
            OUT(t, "    *frame->closure->upvalues[%d].loc.stack = sp[-1];\n", slot);
            break;
        }
        case OP_SET_UPVALUE_CLOSED: {
            uint8_t slot = code[ip++];
            emit_comment(t, start_ip, "OP_SET_UPVALUE_CLOSED");
            OUT(t, "    frame->closure->upvalues[%d].loc.box->closed = sp[-1];\n", slot);
            break;
        }

                                  /* Specialized upvalue slot 0-3 GET */
        case OP_GET_UPVALUE_0: case OP_GET_UPVALUE_OPEN_0:
        case OP_GET_UPVALUE_CLOSED_0: case OP_GET_UPVALUE_IMMUTABLE_0:
        case OP_GET_UPVALUE_1: case OP_GET_UPVALUE_OPEN_1:
        case OP_GET_UPVALUE_CLOSED_1: case OP_GET_UPVALUE_IMMUTABLE_1:
        case OP_GET_UPVALUE_2: case OP_GET_UPVALUE_OPEN_2:
        case OP_GET_UPVALUE_CLOSED_2: case OP_GET_UPVALUE_IMMUTABLE_2:
        case OP_GET_UPVALUE_3: case OP_GET_UPVALUE_OPEN_3:
        case OP_GET_UPVALUE_CLOSED_3: case OP_GET_UPVALUE_IMMUTABLE_3: {
            /* Each slot has 7 opcodes: GET, GET_OPEN, GET_CLOSED, GET_IMMUTABLE,
             * SET, SET_OPEN, SET_CLOSED. Decode slot and variant. */
            int base = op - OP_GET_UPVALUE_0;
            int slot = base / 7;
            int variant = base % 7;  /* 0=generic, 1=OPEN, 2=CLOSED, 3=IMMUTABLE */
            emit_comment(t, start_ip, "OP_GET_UPVALUE_N");
            switch (variant) {
            case 1: /* OPEN */
                OUT(t, "    PUSH(sp, *frame->closure->upvalues[%d].loc.stack);\n", slot);
                break;
            case 2: /* CLOSED */
                OUT(t, "    PUSH(sp, frame->closure->upvalues[%d].loc.box->closed);\n", slot);
                break;
            case 3: /* IMMUTABLE */
                OUT(t, "    PUSH(sp, frame->closure->upvalues[%d].loc.immValue);\n", slot);
                break;
            default: /* generic */
                OUT(t, "    { RuntimeUpvalue* _uv = &frame->closure->upvalues[%d];\n", slot);
                OUT(t, "      PUSH(sp, _uv->isOpen ? *_uv->loc.stack : (_uv->isMutable ? _uv->loc.box->closed : _uv->loc.immValue)); }\n");
                break;
            }
            break;
        }

                                    /* Specialized upvalue slot 0-3 SET */
        case OP_SET_UPVALUE_0: case OP_SET_UPVALUE_OPEN_0: case OP_SET_UPVALUE_CLOSED_0:
        case OP_SET_UPVALUE_1: case OP_SET_UPVALUE_OPEN_1: case OP_SET_UPVALUE_CLOSED_1:
        case OP_SET_UPVALUE_2: case OP_SET_UPVALUE_OPEN_2: case OP_SET_UPVALUE_CLOSED_2:
        case OP_SET_UPVALUE_3: case OP_SET_UPVALUE_OPEN_3: case OP_SET_UPVALUE_CLOSED_3: {
            /* SET variants are at offsets 4, 5, 6 within each slot group of 7 */
            int base = op - OP_GET_UPVALUE_0;
            int slot = base / 7;
            int variant = base % 7;  /* 4=generic, 5=OPEN, 6=CLOSED */
            emit_comment(t, start_ip, "OP_SET_UPVALUE_N");
            switch (variant) {
            case 5: /* OPEN */
                OUT(t, "    *frame->closure->upvalues[%d].loc.stack = sp[-1];\n", slot);
                break;
            case 6: /* CLOSED */
                OUT(t, "    frame->closure->upvalues[%d].loc.box->closed = sp[-1];\n", slot);
                break;
            default: /* generic */
                OUT(t, "    { RuntimeUpvalue* _uv = &frame->closure->upvalues[%d];\n", slot);
                OUT(t, "      if (_uv->isOpen) *_uv->loc.stack = sp[-1]; else _uv->loc.box->closed = sp[-1]; }\n");
                break;
            }
            break;
        }

                             /* ================================================================
                              * FIELDS & PROPERTIES
                              * ================================================================ */
        case OP_FIELD: {
            uint8_t nameIdx = code[ip++];
            emit_comment(t, start_ip, "OP_FIELD");
            emit_sync(t);
            OUT(t, "    btl_compiled_field(vm, frame, %d);\n", nameIdx);
            emit_reload(t);
            break;
        }
        case OP_GET_FIELD_THIS: {
            uint8_t idx = code[ip++];
            emit_comment(t, start_ip, "OP_GET_FIELD_THIS");
            OUT(t, "    PUSH(sp, AS_INSTANCE(slots[0])->fields[%d]);\n", idx);
            break;
        }
        case OP_SET_FIELD_THIS: {
            uint8_t idx = code[ip++];
            emit_comment(t, start_ip, "OP_SET_FIELD_THIS");
            OUT(t, "    { ObjInstance* _inst = AS_INSTANCE(slots[0]); _inst->fields[%d] = sp[-1]; }\n", idx);
            break;
        }
        case OP_GET_PROPERTY_IC: {
            uint8_t nameIdx = code[ip++];
            uint8_t icSlot = code[ip++];
            emit_comment(t, start_ip, "OP_GET_PROPERTY_IC");
            emit_call_bracket_open(t);
            OUT(t, "    if (!btl_compiled_get_property(vm, frame, %d, %d)) return INTERPRET_RUNTIME_ERROR;\n", nameIdx, icSlot);
            emit_call_bracket_close(t);
            break;
        }
        case OP_SET_PROPERTY_IC: {
            uint8_t nameIdx = code[ip++];
            uint8_t icSlot = code[ip++];
            emit_comment(t, start_ip, "OP_SET_PROPERTY_IC");
            emit_call_bracket_open(t);
            OUT(t, "    if (!btl_compiled_set_property(vm, frame, %d, %d)) return INTERPRET_RUNTIME_ERROR;\n", nameIdx, icSlot);
            emit_call_bracket_close(t);
            break;
        }
        case OP_GET_SUPER: {
            uint8_t nameIdx = code[ip++];
            emit_comment(t, start_ip, "OP_GET_SUPER");
            emit_call_bracket_open(t);
            OUT(t, "    if (!btl_compiled_get_super(vm, frame, %d)) return INTERPRET_RUNTIME_ERROR;\n", nameIdx);
            emit_call_bracket_close(t);
            break;
        }
        case OP_GET_SUPER_LONG: {
            uint16_t nameIdx = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            emit_comment(t, start_ip, "OP_GET_SUPER_LONG");
            emit_call_bracket_open(t);
            OUT(t, "    if (!btl_compiled_get_super_long(vm, frame, %d)) return INTERPRET_RUNTIME_ERROR;\n", nameIdx);
            emit_call_bracket_close(t);
            break;
        }

                              /* ================================================================
                               * ARITHMETIC Ã¢â‚¬â€ in-place on sp, no function calls
                               * ================================================================ */
        case OP_ADD:
            emit_comment(t, start_ip, "OP_ADD");
            /* Fast path: both numbers (common case). Slow path: string concat. */
            OUT(t, "    { Value _b = sp[-1], _a = sp[-2];\n");
            OUT(t, "      if (__builtin_expect(IS_NUMBER(_a) & IS_NUMBER(_b), 1))\n");
            OUT(t, "        { sp[-2] = NUMBER_VAL(AS_NUMBER(_a) + AS_NUMBER(_b)); sp--; }\n");
            OUT(t, "      else {\n");
            emit_sync(t);
            OUT(t, "        if (!btl_compiled_add(vm)) return INTERPRET_RUNTIME_ERROR;\n");
            emit_reload(t);
            OUT(t, "    } }\n");
            break;
        case OP_SUBTRACT:
            emit_comment(t, start_ip, "OP_SUBTRACT");
            OUT(t, "    { double _b = AS_NUMBER(sp[-1]); sp[-2] = NUMBER_VAL(AS_NUMBER(sp[-2]) - _b); sp--; }\n");
            break;
        case OP_MULTIPLY:
            emit_comment(t, start_ip, "OP_MULTIPLY");
            OUT(t, "    { double _b = AS_NUMBER(sp[-1]); sp[-2] = NUMBER_VAL(AS_NUMBER(sp[-2]) * _b); sp--; }\n");
            break;
        case OP_DIVIDE:
            emit_comment(t, start_ip, "OP_DIVIDE");
            OUT(t, "    { double _b = AS_NUMBER(sp[-1]); sp[-2] = NUMBER_VAL(AS_NUMBER(sp[-2]) / _b); sp--; }\n");
            break;
        case OP_MODULO:
            emit_comment(t, start_ip, "OP_MODULO");
            OUT(t, "    { double _b = AS_NUMBER(sp[-1]); sp[-2] = NUMBER_VAL(fmod(AS_NUMBER(sp[-2]), _b)); sp--; }\n");
            break;
        case OP_NEGATE:
            emit_comment(t, start_ip, "OP_NEGATE");
            OUT(t, "    sp[-1] = NUMBER_VAL(-AS_NUMBER(sp[-1]));\n");
            break;
        case OP_NOT:
            emit_comment(t, start_ip, "OP_NOT");
            OUT(t, "    sp[-1] = BOOL_VAL(btl_compiled_is_falsey(sp[-1]));\n");
            break;

            /* ================================================================
             * COMPARISON
             * ================================================================ */
        case OP_EQUAL:
            emit_comment(t, start_ip, "OP_EQUAL");
            OUT(t, "    { Value _b = POP(sp); sp[-1] = BOOL_VAL(valuesEqual(sp[-1], _b)); }\n");
            break;
        case OP_GREATER:
            emit_comment(t, start_ip, "OP_GREATER");
            OUT(t, "    { double _b = AS_NUMBER(POP(sp)); sp[-1] = BOOL_VAL(AS_NUMBER(sp[-1]) > _b); }\n");
            break;
        case OP_LESS:
            emit_comment(t, start_ip, "OP_LESS");
            OUT(t, "    { double _b = AS_NUMBER(POP(sp)); sp[-1] = BOOL_VAL(AS_NUMBER(sp[-1]) < _b); }\n");
            break;

            /* ================================================================
             * CONTROL FLOW
             * ================================================================ */
        case OP_JUMP: {
            uint16_t offset = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            emit_comment(t, start_ip, "OP_JUMP");
            OUT(t, "    goto L_%04d;\n", ip + offset);
            break;
        }
        case OP_JUMP_IF_FALSE: {
            uint16_t offset = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            emit_comment(t, start_ip, "OP_JUMP_IF_FALSE");
            OUT(t, "    if (btl_compiled_is_falsey(sp[-1])) goto L_%04d;\n", ip + offset);
            break;
        }
        case OP_POP_JUMP_IF_FALSE: {
            uint16_t offset = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            emit_comment(t, start_ip, "OP_POP_JUMP_IF_FALSE");
            OUT(t, "    if (btl_compiled_is_falsey(POP(sp))) goto L_%04d;\n", ip + offset);
            break;
        }
        case OP_JUMP_IF_TRUE: {
            uint16_t offset = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            emit_comment(t, start_ip, "OP_JUMP_IF_TRUE");
            OUT(t, "    if (!btl_compiled_is_falsey(sp[-1])) goto L_%04d;\n", ip + offset);
            break;
        }
        case OP_POP_JUMP_IF_TRUE: {
            uint16_t offset = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            emit_comment(t, start_ip, "OP_POP_JUMP_IF_TRUE");
            OUT(t, "    if (!btl_compiled_is_falsey(POP(sp))) goto L_%04d;\n", ip + offset);
            break;
        }
        case OP_JUMP_IF_NOT_EQUAL: {
            uint16_t offset = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            emit_comment(t, start_ip, "OP_JUMP_IF_NOT_EQUAL");
            OUT(t, "    { Value _b = POP(sp); Value _a = POP(sp); if (!valuesEqual(_a, _b)) goto L_%04d; }\n", ip + offset);
            break;
        }
        case OP_JUMP_IF_EQUAL: {
            uint16_t offset = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            emit_comment(t, start_ip, "OP_JUMP_IF_EQUAL");
            OUT(t, "    { Value _b = POP(sp); Value _a = POP(sp); if (valuesEqual(_a, _b)) goto L_%04d; }\n", ip + offset);
            break;
        }
        case OP_JUMP_IF_NOT_GREATER: {
            uint16_t offset = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            emit_comment(t, start_ip, "OP_JUMP_IF_NOT_GREATER");
            OUT(t, "    { double _b = AS_NUMBER(POP(sp)); double _a = AS_NUMBER(POP(sp)); if (!(_a > _b)) goto L_%04d; }\n", ip + offset);
            break;
        }
        case OP_JUMP_IF_NOT_LESS: {
            uint16_t offset = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            emit_comment(t, start_ip, "OP_JUMP_IF_NOT_LESS");
            OUT(t, "    { double _b = AS_NUMBER(POP(sp)); double _a = AS_NUMBER(POP(sp)); if (!(_a < _b)) goto L_%04d; }\n", ip + offset);
            break;
        }
        case OP_LOOP: {
            uint16_t offset = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            emit_comment(t, start_ip, "OP_LOOP");
            OUT(t, "    goto L_%04d;\n", ip - offset);
            break;
        }

                    /* ================================================================
                     * CALLS Ã¢â‚¬â€ the big performance win
                     *
                     * Instead of callValue() + run() (which re-enters the interpreter),
                     * we:
                     *   1) Sync sp to vm->stackTop
                     *   2) Set up the CallFrame ourselves
                     *   3) Call btl_fn_N(vm) directly (known target) or
                     *      callValue+run (unknown/native target)
                     *   4) Reload sp from vm->stackTop
                     * ================================================================ */
        case OP_CALL_0: case OP_CALL_1: case OP_CALL_2: case OP_CALL_3:
        case OP_CALL_4: case OP_CALL_5: case OP_CALL_6: case OP_CALL_7:
        case OP_CALL_8: {
            int argc = op - OP_CALL_0;
            emit_comment(t, start_ip, "OP_CALL_N");
            emit_call_bracket_open(t);
            emit_optimized_call(t, argc);
            emit_call_bracket_close(t);
            break;
        }
        case OP_CALL: {
            uint8_t argc = code[ip++];
            emit_comment(t, start_ip, "OP_CALL");
            emit_call_bracket_open(t);
            emit_optimized_call(t, argc);
            emit_call_bracket_close(t);
            break;
        }

                    /* Tail calls */
        case OP_TAIL_CALL_0: case OP_TAIL_CALL_1: case OP_TAIL_CALL_2:
        case OP_TAIL_CALL_3: case OP_TAIL_CALL_4: case OP_TAIL_CALL_5:
        case OP_TAIL_CALL_6: case OP_TAIL_CALL_7: case OP_TAIL_CALL_8: {
            int argc = op - OP_TAIL_CALL_0;
            emit_comment(t, start_ip, "OP_TAIL_CALL_N");
            emit_call_bracket_open(t);
            emit_optimized_call(t, argc);
            emit_call_bracket_close(t);
            break;
        }
        case OP_TAIL_CALL: {
            uint8_t argc = code[ip++];
            emit_comment(t, start_ip, "OP_TAIL_CALL");
            emit_call_bracket_open(t);
            emit_optimized_call(t, argc);
            emit_call_bracket_close(t);
            break;
        }

                         /* ================================================================
                          * INVOKE (indexed)
                          * ================================================================ */
        case OP_INVOKE_0: case OP_INVOKE_1: case OP_INVOKE_2: case OP_INVOKE_3:
        case OP_INVOKE_4: case OP_INVOKE_5: case OP_INVOKE_6: case OP_INVOKE_7:
        case OP_INVOKE_8: {
            int argc = op - OP_INVOKE_0;
            uint8_t methodIdx = code[ip++];
            emit_comment(t, start_ip, "OP_INVOKE_N");
            emit_call_bracket_open(t);
            OUT(t, "    if (!btl_compiled_invoke_indexed(vm, %d, %d)) return INTERPRET_RUNTIME_ERROR;\n", methodIdx, argc);
            emit_call_bracket_close(t);
            break;
        }
        case OP_INVOKE: {
            uint8_t methodIdx = code[ip++];
            uint8_t argc = code[ip++];
            emit_comment(t, start_ip, "OP_INVOKE");
            emit_call_bracket_open(t);
            OUT(t, "    if (!btl_compiled_invoke_indexed(vm, %d, %d)) return INTERPRET_RUNTIME_ERROR;\n", methodIdx, argc);
            emit_call_bracket_close(t);
            break;
        }
        case OP_INVOKE_LONG: {
            uint16_t methodIdx = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            uint8_t argc = code[ip++];
            emit_comment(t, start_ip, "OP_INVOKE_LONG");
            emit_call_bracket_open(t);
            OUT(t, "    if (!btl_compiled_invoke_indexed(vm, %d, %d)) return INTERPRET_RUNTIME_ERROR;\n", methodIdx, argc);
            emit_call_bracket_close(t);
            break;
        }
        case OP_INVOKE_IC: {
            uint8_t nameIdx = code[ip++];
            uint8_t argc = code[ip++];
            uint8_t icSlot = code[ip++];
            emit_comment(t, start_ip, "OP_INVOKE_IC");
            emit_call_bracket_open(t);
            OUT(t, "    if (!btl_compiled_invoke_ic(vm, frame, %d, %d, %d)) return INTERPRET_RUNTIME_ERROR;\n", nameIdx, argc, icSlot);
            emit_call_bracket_close(t);
            break;
        }

                         /* Tail invoke indexed */
        case OP_TAIL_INVOKE_0: case OP_TAIL_INVOKE_1: case OP_TAIL_INVOKE_2:
        case OP_TAIL_INVOKE_3: case OP_TAIL_INVOKE_4: case OP_TAIL_INVOKE_5:
        case OP_TAIL_INVOKE_6: case OP_TAIL_INVOKE_7: case OP_TAIL_INVOKE_8: {
            int argc = op - OP_TAIL_INVOKE_0;
            uint8_t methodIdx = code[ip++];
            emit_comment(t, start_ip, "OP_TAIL_INVOKE_N");
            emit_call_bracket_open(t);
            OUT(t, "    if (!btl_compiled_invoke_indexed(vm, %d, %d)) return INTERPRET_RUNTIME_ERROR;\n", methodIdx, argc);
            emit_call_bracket_close(t);
            break;
        }
        case OP_TAIL_INVOKE: {
            uint8_t methodIdx = code[ip++];
            uint8_t argc = code[ip++];
            emit_comment(t, start_ip, "OP_TAIL_INVOKE");
            emit_call_bracket_open(t);
            OUT(t, "    if (!btl_compiled_invoke_indexed(vm, %d, %d)) return INTERPRET_RUNTIME_ERROR;\n", methodIdx, argc);
            emit_call_bracket_close(t);
            break;
        }
        case OP_TAIL_INVOKE_LONG: {
            uint16_t methodIdx = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            uint8_t argc = code[ip++];
            emit_comment(t, start_ip, "OP_TAIL_INVOKE_LONG");
            emit_call_bracket_open(t);
            OUT(t, "    if (!btl_compiled_invoke_indexed(vm, %d, %d)) return INTERPRET_RUNTIME_ERROR;\n", methodIdx, argc);
            emit_call_bracket_close(t);
            break;
        }
        case OP_TAIL_INVOKE_IC: {
            uint8_t nameIdx = code[ip++];
            uint8_t argc = code[ip++];
            uint8_t icSlot = code[ip++];
            emit_comment(t, start_ip, "OP_TAIL_INVOKE_IC");
            emit_call_bracket_open(t);
            OUT(t, "    if (!btl_compiled_invoke_ic(vm, frame, %d, %d, %d)) return INTERPRET_RUNTIME_ERROR;\n", nameIdx, argc, icSlot);
            emit_call_bracket_close(t);
            break;
        }

                              /* ================================================================
                               * SUPER INVOKE
                               * ================================================================ */
        case OP_SUPER_INVOKE_0: case OP_SUPER_INVOKE_1: case OP_SUPER_INVOKE_2:
        case OP_SUPER_INVOKE_3: case OP_SUPER_INVOKE_4: case OP_SUPER_INVOKE_5:
        case OP_SUPER_INVOKE_6: case OP_SUPER_INVOKE_7: case OP_SUPER_INVOKE_8: {
            int argc = op - OP_SUPER_INVOKE_0;
            uint8_t methodIdx = code[ip++];
            emit_comment(t, start_ip, "OP_SUPER_INVOKE_N");
            emit_call_bracket_open(t);
            OUT(t, "    if (!btl_compiled_super_invoke(vm, %d, %d)) return INTERPRET_RUNTIME_ERROR;\n", methodIdx, argc);
            emit_call_bracket_close(t);
            break;
        }
        case OP_SUPER_INVOKE: {
            uint8_t methodIdx = code[ip++];
            uint8_t argc = code[ip++];
            emit_comment(t, start_ip, "OP_SUPER_INVOKE");
            emit_call_bracket_open(t);
            OUT(t, "    if (!btl_compiled_super_invoke(vm, %d, %d)) return INTERPRET_RUNTIME_ERROR;\n", methodIdx, argc);
            emit_call_bracket_close(t);
            break;
        }
        case OP_SUPER_INVOKE_LONG: {
            uint16_t methodIdx = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            uint8_t argc = code[ip++];
            emit_comment(t, start_ip, "OP_SUPER_INVOKE_LONG");
            emit_call_bracket_open(t);
            OUT(t, "    if (!btl_compiled_super_invoke(vm, %d, %d)) return INTERPRET_RUNTIME_ERROR;\n", methodIdx, argc);
            emit_call_bracket_close(t);
            break;
        }
                                 /* Tail super invoke */
        case OP_TAIL_SUPER_INVOKE_0: case OP_TAIL_SUPER_INVOKE_1: case OP_TAIL_SUPER_INVOKE_2:
        case OP_TAIL_SUPER_INVOKE_3: case OP_TAIL_SUPER_INVOKE_4: case OP_TAIL_SUPER_INVOKE_5:
        case OP_TAIL_SUPER_INVOKE_6: case OP_TAIL_SUPER_INVOKE_7: case OP_TAIL_SUPER_INVOKE_8: {
            int argc = op - OP_TAIL_SUPER_INVOKE_0;
            uint8_t methodIdx = code[ip++];
            emit_comment(t, start_ip, "OP_TAIL_SUPER_INVOKE_N");
            emit_call_bracket_open(t);
            OUT(t, "    if (!btl_compiled_super_invoke(vm, %d, %d)) return INTERPRET_RUNTIME_ERROR;\n", methodIdx, argc);
            emit_call_bracket_close(t);
            break;
        }
        case OP_TAIL_SUPER_INVOKE: {
            uint8_t methodIdx = code[ip++];
            uint8_t argc = code[ip++];
            emit_comment(t, start_ip, "OP_TAIL_SUPER_INVOKE");
            emit_call_bracket_open(t);
            OUT(t, "    if (!btl_compiled_super_invoke(vm, %d, %d)) return INTERPRET_RUNTIME_ERROR;\n", methodIdx, argc);
            emit_call_bracket_close(t);
            break;
        }
        case OP_TAIL_SUPER_INVOKE_LONG: {
            uint16_t methodIdx = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            uint8_t argc = code[ip++];
            emit_comment(t, start_ip, "OP_TAIL_SUPER_INVOKE_LONG");
            emit_call_bracket_open(t);
            OUT(t, "    if (!btl_compiled_super_invoke(vm, %d, %d)) return INTERPRET_RUNTIME_ERROR;\n", methodIdx, argc);
            emit_call_bracket_close(t);
            break;
        }

                                      /* ================================================================
                                       * CLOSURES
                                       * ================================================================ */
        case OP_CLOSURE: case OP_CLOSURE_LONG: {
            uint16_t fn_idx;
            if (op == OP_CLOSURE) {
                fn_idx = code[ip++];
            } else {
                fn_idx = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            }
            emit_comment(t, start_ip, "OP_CLOSURE");
            /* Sync sp Ã¢â‚¬â€ newClosure can trigger GC */
            emit_sync(t);
            OUT(t, "    {\n");
            OUT(t, "        ObjFunction* _f = AS_FUNCTION(fn->chunk.constants.values[%d]);\n", fn_idx);
            OUT(t, "        ObjClosure* _c = newClosure(vm, _f);\n");
            OUT(t, "        push(vm, OBJ_VAL(_c));\n");

            /* Read upvalue descriptors */
            Value fn_val = fn->chunk.constants.values[fn_idx];
            ObjFunction* child = AS_FUNCTION(fn_val);
            for (int i = 0; i < child->upvalueCount; i++) {
                uint8_t isLocal = code[ip++];
                uint8_t index = code[ip++];
                uint8_t isMutable = code[ip++];

                OUT(t, "        { RuntimeUpvalue* _d = &_c->upvalues[%d];\n", i);
                OUT(t, "          _d->isMutable = %s;\n", isMutable ? "true" : "false");
                if (isLocal) {
                    OUT(t, "          _d->isOpen = true; _d->loc.stack = slots + %d;\n", index);
                    OUT(t, "          _d->next = frame->openUpvalues; frame->openUpvalues = _d;\n");
                } else {
                    OUT(t, "          RuntimeUpvalue* _p = &frame->closure->upvalues[%d];\n", index);
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
            OUT(t, "    }\n");
            /* Reload sp after GC-triggering newClosure */
            OUT(t, "    sp = vm->stackTop;\n");
            break;
        }

        case OP_CLOSE_UPVALUE:
            emit_comment(t, start_ip, "OP_CLOSE_UPVALUE");
            emit_sync(t);
            OUT(t, "    btl_compiled_close_upvalues(vm, frame); pop(vm);\n");
            emit_reload(t);
            break;

            /* ================================================================
             * RETURN
             *
             * Sync sp, tear down frame, push result, return.
             * ================================================================ */
        case OP_RETURN:
            emit_comment(t, start_ip, "OP_RETURN");
            OUT(t, "    {\n");
            OUT(t, "        Value _result = sp[-1];\n");
            emit_sync(t);
            OUT(t, "        btl_compiled_close_upvalues(vm, frame);\n");
            OUT(t, "        vm->frameCount--;\n");
            OUT(t, "        if (vm->frameCount == 0) { vm->lastReturnValue = _result; pop(vm); return INTERPRET_OK; }\n");
            OUT(t, "        vm->stackTop = frame->slots;\n");
            OUT(t, "        push(vm, _result);\n");
            OUT(t, "        return INTERPRET_OK;\n");
            OUT(t, "    }\n");
            break;

            /* ================================================================
             * CLASSES
             * ================================================================ */
        case OP_CLASS: {
            uint8_t nameIdx = code[ip++];
            emit_comment(t, start_ip, "OP_CLASS");
            emit_call_bracket_open(t);
            OUT(t, "    btl_compiled_class(vm, fn, %d);\n", nameIdx);
            emit_call_bracket_close(t);
            break;
        }
        case OP_CLASS_LONG: {
            uint16_t nameIdx = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            emit_comment(t, start_ip, "OP_CLASS_LONG");
            emit_call_bracket_open(t);
            OUT(t, "    btl_compiled_class_long(vm, fn, %d);\n", nameIdx);
            emit_call_bracket_close(t);
            break;
        }
        case OP_INHERIT:
            emit_comment(t, start_ip, "OP_INHERIT");
            emit_call_bracket_open(t);
            OUT(t, "    if (!btl_compiled_inherit(vm)) return INTERPRET_RUNTIME_ERROR;\n");
            emit_call_bracket_close(t);
            break;
        case OP_METHOD: {
            uint8_t methodIdx = code[ip++];
            uint8_t arity = code[ip++];
            emit_comment(t, start_ip, "OP_METHOD");
            emit_call_bracket_open(t);
            OUT(t, "    btl_compiled_method(vm, %d, %d);\n", methodIdx, arity);
            emit_call_bracket_close(t);
            break;
        }
        case OP_METHOD_LONG: {
            uint16_t methodIdx = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            uint8_t arity = code[ip++];
            emit_comment(t, start_ip, "OP_METHOD_LONG");
            emit_call_bracket_open(t);
            OUT(t, "    btl_compiled_method(vm, %d, %d);\n", methodIdx, arity);
            emit_call_bracket_close(t);
            break;
        }

                           /* ================================================================
                            * COLLECTIONS
                            * ================================================================ */
        case OP_BUILD_LIST: {
            uint8_t count = code[ip++];
            emit_comment(t, start_ip, "OP_BUILD_LIST");
            emit_call_bracket_open(t);
            OUT(t, "    btl_compiled_build_list(vm, %d);\n", count);
            emit_call_bracket_close(t);
            break;
        }
        case OP_BUILD_TABLE: {
            uint8_t count = code[ip++];
            emit_comment(t, start_ip, "OP_BUILD_TABLE");
            emit_call_bracket_open(t);
            OUT(t, "    btl_compiled_build_table(vm, %d);\n", count);
            emit_call_bracket_close(t);
            break;
        }
        case OP_INDEX_GET:
            emit_comment(t, start_ip, "OP_INDEX_GET");
            emit_call_bracket_open(t);
            OUT(t, "    if (!btl_compiled_index_get(vm)) return INTERPRET_RUNTIME_ERROR;\n");
            emit_call_bracket_close(t);
            break;
        case OP_INDEX_SET:
            emit_comment(t, start_ip, "OP_INDEX_SET");
            emit_call_bracket_open(t);
            OUT(t, "    if (!btl_compiled_index_set(vm)) return INTERPRET_RUNTIME_ERROR;\n");
            emit_call_bracket_close(t);
            break;

            /* ================================================================
             * MODULES
             * ================================================================ */
        case OP_IMPORT: {
            uint8_t nameIdx = code[ip++];
            emit_comment(t, start_ip, "OP_IMPORT");
            emit_call_bracket_open(t);
            OUT(t, "    if (!btl_compiled_import(vm, frame, %d)) return INTERPRET_RUNTIME_ERROR;\n", nameIdx);
            emit_call_bracket_close(t);
            break;
        }
        case OP_IMPORT_LONG: {
            uint16_t nameIdx = (uint16_t) ((code[ip] << 8) | code[ip + 1]); ip += 2;
            emit_comment(t, start_ip, "OP_IMPORT_LONG");
            emit_call_bracket_open(t);
            OUT(t, "    if (!btl_compiled_import_long(vm, frame, %d)) return INTERPRET_RUNTIME_ERROR;\n", nameIdx);
            emit_call_bracket_close(t);
            break;
        }

                           /* ================================================================
                            * ACTORS
                            * ================================================================ */
        case OP_DO_NEW: {
            uint8_t argc = code[ip++];
            emit_comment(t, start_ip, "OP_DO_NEW");
            emit_call_bracket_open(t);
            OUT(t, "    if (!btl_compiled_do_new(vm, %d)) return INTERPRET_RUNTIME_ERROR;\n", argc);
            emit_call_bracket_close(t);
            break;
        }
        case OP_DO_INVOKE: {
            uint8_t nameConst = code[ip++];
            uint8_t argc = code[ip++];
            emit_comment(t, start_ip, "OP_DO_INVOKE");
            emit_call_bracket_open(t);
            OUT(t, "    if (!btl_compiled_do_invoke(vm, frame, %d, %d)) return INTERPRET_RUNTIME_ERROR;\n", nameConst, argc);
            emit_call_bracket_close(t);
            break;
        }

        default:
            emit_sync(t);
            OUT(t, "    /* UNHANDLED OPCODE %d */\n", op);
            OUT(t, "    runtimeError(vm, \"Unhandled opcode in transpiled code: %%d\", %d); return INTERPRET_RUNTIME_ERROR;\n", op);
            break;
        }
    }

    OUT(t, "    return INTERPRET_OK;\n");
    OUT(t, "}\n\n");
    free(targets);
}

/* ================================================================
 * Program entry point
 * ================================================================ */

static void emit_main(Transpiler* t) {
    OUT(t, "/* Entry point for transpiled program */\n");
    OUT(t, "InterpretResult btl_compiled_run(VM* vm, ObjModule* module, const char* source) {\n");
    OUT(t, "    ObjFunction* f = compile(vm, module, source);\n");
    OUT(t, "    if (f == NULL) return INTERPRET_COMPILE_ERROR;\n");
    OUT(t, "    btl_register_functions(f);\n");
    OUT(t, "    push(vm, OBJ_VAL(f));\n");
    OUT(t, "    ObjClosure* c = newClosure(vm, f);\n");
    OUT(t, "    pop(vm); push(vm, OBJ_VAL(c));\n");
    OUT(t, "    callValue(vm, OBJ_VAL(c), 0);\n");
    OUT(t, "    return btl_fn_0(vm);\n");
    OUT(t, "}\n");
}

/* ================================================================
 * Public API
 * ================================================================ */

Transpiler* transpiler_new(TranspilerConfig config) {
    Transpiler* t = calloc(1, sizeof(Transpiler));
    t->config = config;
    t->out = fopen(config.output_path, "w");
    if (!t->out) {
        free(t); return NULL;
    }
    return t;
}

void transpiler_free(Transpiler* t) {
    if (t->out) fclose(t->out);
    free(t);
}

bool transpiler_emit_program(Transpiler* t, ObjFunction* main_fn, ObjModule* module) {
    t->module = module;

    /* Collect all functions */
    function_list_init(&t->fns);
    collect_functions(main_fn, &t->fns);

    /* Emit header with forward declarations */
    emit_header(t);

    /* Emit each function */
    for (int i = 0; i < t->fns.count; i++) {
        emit_function(t, t->fns.functions[i], i);
    }

    /* Emit entry point */
    emit_main(t);

    function_list_free(&t->fns);
    return true;
}