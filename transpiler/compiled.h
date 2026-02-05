/*
 * BTL Compiled Support Library — v2
 *
 * Helper functions called by transpiler-generated C code.
 * These implement the complex opcodes (invoke, property access, etc.)
 * that are too large to inline into the generated code.
 *
 * Generated code also directly uses VM functions:
 *   push(), pop(), callValue(), valuesEqual(), runtimeError()
 *   newClosure(), newInstance(), newList(), etc.
 *
 * This header is #included by the generated .c file.
 */

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

#include <math.h>
#include <string.h>

 /* ============================================================================
  * External VM functions used by generated code
  *
  * NOTE: vm.c's internal call() collides with compiler.c's call() parser
  * function, so we use callValue(vm, OBJ_VAL(closure), argc) instead.
  * The wrapper below provides a convenient btl_call_closure() for invoke
  * helpers that already have the ObjClosure* in hand.
  * ============================================================================ */

extern InterpretResult run(VM* vm);
extern bool callValue(VM* vm, Value callee, int argCount);

/* Wrapper: call a known closure via callValue (avoids name collision with compiler.c) */
static inline bool btl_call_closure(VM* vm, ObjClosure* closure, int argCount) {
    return callValue(vm, OBJ_VAL(closure), argCount);
}

/* ============================================================================
 * Inline helpers — hot path, inlined into generated code
 * ============================================================================ */

static inline bool btl_compiled_is_falsey(Value value) {
    if (IS_NULL(value)) return true;
    if (IS_BOOL(value)) return !AS_BOOL(value);
    return false;
}

/* ============================================================================
 * Complex opcode helpers (implemented in btl_compiled.c)
 *
 * These are the "slow paths" — called via function pointer when the
 * generated code can't handle something inline. They all operate on
 * vm->stackTop, so the generated code must sync sp before calling.
 * ============================================================================ */

 /* OP_ADD: number addition + string concatenation */
bool btl_compiled_add(VM* vm);

/* Upvalue closing on frame teardown */
void btl_compiled_close_upvalues(VM* vm, CallFrame* frame);

/* Property access (inline-cached) */
bool btl_compiled_get_property(VM* vm, CallFrame* frame, int nameIdx, int icSlot);
bool btl_compiled_set_property(VM* vm, CallFrame* frame, int nameIdx, int icSlot);

/* Super property access */
bool btl_compiled_get_super(VM* vm, CallFrame* frame, int nameIdx);
bool btl_compiled_get_super_long(VM* vm, CallFrame* frame, int nameIdx);

/* OP_FIELD: register field name on class */
void btl_compiled_field(VM* vm, CallFrame* frame, int nameIdx);

/* Method invocation */
bool btl_compiled_invoke_indexed(VM* vm, int methodIndex, int argCount);
bool btl_compiled_invoke_ic(VM* vm, CallFrame* frame, int nameIdx, int argCount, int icSlot);
bool btl_compiled_super_invoke(VM* vm, int methodIndex, int argCount);

/* Class operations */
void btl_compiled_class(VM* vm, ObjFunction* fn, int nameIdx);
void btl_compiled_class_long(VM* vm, ObjFunction* fn, int nameIdx);
bool btl_compiled_inherit(VM* vm);
void btl_compiled_method(VM* vm, int methodIndex, int arity);

/* Collections */
void btl_compiled_build_list(VM* vm, int count);
void btl_compiled_build_table(VM* vm, int count);
bool btl_compiled_index_get(VM* vm);
bool btl_compiled_index_set(VM* vm);

/* Modules */
bool btl_compiled_import(VM* vm, CallFrame* frame, int nameIdx);
bool btl_compiled_import_long(VM* vm, CallFrame* frame, int nameIdx);

/* Actors */
bool btl_compiled_do_new(VM* vm, int argCount);
bool btl_compiled_do_invoke(VM* vm, CallFrame* frame, int nameConst, int argCount);

#endif /* BTL_COMPILED_H */