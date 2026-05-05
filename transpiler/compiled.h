// Helpers called by transpiler-generated C. Out-of-line implementations of
// complex opcodes (invoke, property access, ...) that are too big to inline.
// Generated code also uses VM functions directly: btl_push, btl_pop,
// btl_call_value, btl_values_equal, btl_runtime_error, btl_closure_new, etc.
// Included by the generated .c file.

#ifndef btl_compiled_h
#define btl_compiled_h

#include "../src/common.h"
#include "../src/value.h"
#include "../src/object.h"
#include "../src/chunk.h"
#include "../src/vm.h"
#include "../src/memory.h"
#include "../src/table.h"
#include "../src/compiler.h"
#include "../src/runtime.h"

#include <math.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>

// MSVC compatibility
#ifdef _MSC_VER
#define __builtin_expect(expr, val) (expr)
#define BTL_COLD_NOINLINE __declspec(noinline)
#else
#define BTL_COLD_NOINLINE __attribute__((cold, noinline))
#endif

// ----------------------------------------------------------------------------
// External VM functions used by generated code
//
// NOTE: vm.c's internal call() collides with compiler.c's call() parser
// function, so we use btl_call_value(vm, OBJ_VAL(closure), argc) instead.
// The wrapper below provides a convenient btl_call_closure() for invoke
// helpers that already have the ObjClosure* in hand.
// ----------------------------------------------------------------------------

extern BtlInterpretResult btl_run(VM* vm);
extern bool btl_call_value(VM* vm, BtlValue callee, int argCount);

// Wrapper: call a known closure via btl_call_value
static inline bool btl_call_closure(VM* vm, ObjClosure* closure, int argCount) {
    return btl_call_value(vm, OBJ_VAL(closure), argCount);
}

// ----------------------------------------------------------------------------
// Inline helpers - hot path, inlined into generated code
// ----------------------------------------------------------------------------

// Returns true if value is falsey (null, false, 0, 0.0, "", [], {})
static inline bool btl_compiled_is_falsey(BtlValue value) {
    if (IS_NULL(value)) return true;
    if (IS_BOOL(value)) return !AS_BOOL(value);
    if (IS_INT(value)) return AS_INT(value) == 0;
    if (IS_NUMBER(value)) return AS_NUMBER(value) == 0.0;
    if (IS_STRING(value)) return AS_STRING(value)->length == 0;
    if (IS_LIST(value)) return AS_LIST(value)->items.count == 0;
    if (IS_TABLE(value)) return AS_TABLE(value)->table.count == 0;
    return false;
}

// ----------------------------------------------------------------------------
// Cold error helpers - extracted error paths to reduce hot code size
//
// These functions are marked cold to tell the compiler they are rarely
// executed, improving I-cache efficiency on the hot paths.
// ----------------------------------------------------------------------------

BTL_COLD_NOINLINE
static BtlInterpretResult btl_error_undefined(VM* vm, BtlValue* sp, const char* name) {
    vm->stackTop = sp;
    btl_runtime_error(vm, "Undefined variable '%s'.", name);
    return BTL_INTERPRET_RUNTIME_ERROR;
}

BTL_COLD_NOINLINE
static BtlInterpretResult btl_error_not_numbers(VM* vm, BtlValue* sp) {
    vm->stackTop = sp;
    btl_runtime_error(vm, "Operands must be numbers.");
    return BTL_INTERPRET_RUNTIME_ERROR;
}

BTL_COLD_NOINLINE
static BtlInterpretResult btl_error_not_number(VM* vm, BtlValue* sp) {
    vm->stackTop = sp;
    btl_runtime_error(vm, "Operand must be a number.");
    return BTL_INTERPRET_RUNTIME_ERROR;
}

BTL_COLD_NOINLINE
static BtlInterpretResult btl_error_div_zero(VM* vm, BtlValue* sp) {
    vm->stackTop = sp;
    btl_runtime_error(vm, "Division by zero.");
    return BTL_INTERPRET_RUNTIME_ERROR;
}

// Extended equality: handles future == null (pending check) and actor == null (dead check)
static inline bool btl_compiled_equal(BtlValue a, BtlValue b) {
    if (IS_FUTURE(a) && IS_NULL(b)) return btl_future_get_state(AS_FUTURE(a)) == BTL_FUTURE_PENDING;
    if (IS_NULL(a) && IS_FUTURE(b)) return btl_future_get_state(AS_FUTURE(b)) == BTL_FUTURE_PENDING;
    if (IS_ACTOR(a) && IS_NULL(b)) return !AS_ACTOR(a)->alive;
    if (IS_NULL(a) && IS_ACTOR(b)) return !AS_ACTOR(b)->alive;
    return btl_values_equal(a, b);
}

// Null-check with future/actor semantics: null matches pending futures and dead actors
static inline bool btl_compiled_is_null_like(BtlValue v) {
    if (IS_NULL(v)) return true;
    if (IS_FUTURE(v)) return btl_future_get_state(AS_FUTURE(v)) == BTL_FUTURE_PENDING;
    if (IS_ACTOR(v)) return !AS_ACTOR(v)->alive;
    return false;
}

// ----------------------------------------------------------------------------
// Complex opcode helpers (implemented in compiled.c)
//
// These are the "slow paths" - called via function pointer when the
// generated code can't handle something inline. They all operate on
// vm->stackTop, so the generated code must sync sp before calling.
// ----------------------------------------------------------------------------

// OP_ADD: number addition + string concatenation
bool btl_compiled_add(VM* vm);

// Upvalue closing on frame teardown
void btl_compiled_close_upvalues(VM* vm, BtlCallFrame* frame);

// Property access (inline-cached)
bool btl_compiled_get_property(VM* vm, BtlCallFrame* frame, int nameIdx, int icSlot);
bool btl_compiled_set_property(VM* vm, BtlCallFrame* frame, int nameIdx, int icSlot);

// Super property access
bool btl_compiled_get_super(VM* vm, BtlCallFrame* frame, int nameIdx);
bool btl_compiled_get_super_long(VM* vm, BtlCallFrame* frame, int nameIdx);

// OP_FIELD: register field name on class
void btl_compiled_field(VM* vm, BtlCallFrame* frame, int nameIdx);

// Method invocation
bool btl_compiled_invoke_indexed(VM* vm, int methodIndex, int argCount);
bool btl_compiled_invoke_ic(VM* vm, BtlCallFrame* frame, int nameIdx, int argCount, int icSlot);
bool btl_compiled_super_invoke(VM* vm, int methodIndex, int argCount);

// Call closure and run - exposed for inlined IC fast paths
bool btl_compiled_call_closure_and_run(VM* vm, ObjClosure* closure, int argCount);

// Fast class instantiation - avoids per-call string creation for init lookup
bool btl_compiled_call_class(VM* vm, ObjClass* klass, int argCount);

// Class operations
void btl_compiled_class(VM* vm, ObjFunction* fn, int nameIdx);
void btl_compiled_class_long(VM* vm, ObjFunction* fn, int nameIdx);
bool btl_compiled_inherit(VM* vm);
void btl_compiled_method(VM* vm, int methodIndex, int arity);

// Collections
// btl_compiled_build_list and btl_compiled_build_table are small and hot;
// inline them for better performance on list/table literals.
static inline void btl_compiled_build_list(VM* vm, int count) {
    ObjList* l = btl_list_new(vm);
    btl_push(vm, OBJ_VAL(l));  // GC root
    for (int i = 0; i < count; i++) {
        btl_value_array_write(vm, &l->items, vm->stackTop[-count - 1 + i]);
    }
    vm->stackTop -= (count + 1);
    btl_push(vm, OBJ_VAL(l));
}

static inline void btl_compiled_build_table(VM* vm, int count) {
    ObjTable* table = btl_table_obj_new(vm);
    btl_push(vm, OBJ_VAL(table));  // GC root
    BtlValue* pairs = vm->stackTop - (count * 2) - 1;
    for (int i = 0; i < count; i++) {
        BtlValue key = pairs[i * 2];
        BtlValue value = pairs[i * 2 + 1];
        btl_table_set(vm, &table->table, key, value);
    }
    vm->stackTop -= (count * 2 + 1);
    btl_push(vm, OBJ_VAL(table));
}

bool btl_compiled_index_get(VM* vm);
bool btl_compiled_index_set(VM* vm);

// Modules
bool btl_compiled_import(VM* vm, BtlCallFrame* frame, int nameIdx);
bool btl_compiled_import_long(VM* vm, BtlCallFrame* frame, int nameIdx);

// Actors
bool btl_compiled_do_new(VM* vm, int argCount);
bool btl_compiled_do_invoke(VM* vm, BtlCallFrame* frame, int nameConst, int argCount);

// Iterators (for...in)
bool btl_compiled_iter_init(VM* vm);
int btl_compiled_iter_next(VM* vm, int slot);

#endif // btl_compiled_h