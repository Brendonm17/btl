/*
 * BTL to C Transpiler v2 — Performance-Optimized
 *
 * Improvements over v1:
 *   1. Inline stack ops: *sp++ / *--sp instead of push()/pop() function calls
 *   2. Direct call threading: btl_fn_N(vm) instead of callValue() + run()
 *   3. Fused opcode patterns: GET_LOCAL+ADD+SET_LOCAL → single C statement
 *   4. Local caching: frame/slots/stackTop in C locals, synced at call boundaries
 *   5. Tail call optimization: goto instead of call for known tail calls
 *
 * Same API as v1. Drop-in replacement.
 */

#ifndef btl_transpiler_h
#define btl_transpiler_h
#include <stdio.h>
#include <stdbool.h>
#include "collect.h"

typedef struct VM VM;
typedef struct ObjModule ObjModule;

typedef struct {
    bool emit_comments;
    bool emit_line_info;
    bool bounds_checks;
    const char* output_path;
} TranspilerConfig;

typedef struct {
    FILE* out;
    TranspilerConfig config;

    FunctionList fns;

    /* Current function state */
    ObjFunction* current_fn;
    int current_fn_id;
    ObjModule* module;
} Transpiler;

Transpiler* transpiler_new(TranspilerConfig config);
void transpiler_free(Transpiler* t);
bool transpiler_emit_program(Transpiler* t, ObjFunction* main_fn, ObjModule* module);

#endif