// ============================================================================
// transpiler.h - BTL to C Transpiler
//
// Performance-optimized transpiler that converts BTL bytecode to C code.
//
// Improvements over v1:
//   1. Inline stack ops: *sp++ / *--sp instead of push()/pop() function calls
//   2. Direct call threading: btl_fn_N(vm) instead of callValue() + run()
//   3. Fused opcode patterns: GET_LOCAL+ADD+SET_LOCAL -> single C statement
//   4. Local caching: frame/slots/stackTop in C locals, synced at call boundaries
//   5. Tail call optimization: goto instead of call for known tail calls
//
// Same API as v1. Drop-in replacement.
// ============================================================================

#ifndef btl_transpiler_h
#define btl_transpiler_h

#include <stdio.h>
#include <stdbool.h>
#include "collect.h"

typedef struct VM VM;
typedef struct ObjModule ObjModule;

// ----------------------------------------------------------------------------
// Transpiler Configuration
// ----------------------------------------------------------------------------

typedef struct {
    bool emit_comments;     // Include bytecode offset comments
    bool emit_line_info;    // Include source line info
    bool bounds_checks;     // Include bounds/type checks
    const char* output_path;
} BtlTranspilerConfig;

// ----------------------------------------------------------------------------
// Transpiler State
// ----------------------------------------------------------------------------

typedef struct {
    FILE* out;
    BtlTranspilerConfig config;

    BtlFunctionList fns;

    // Current function state
    ObjFunction* current_fn;
    int current_fn_id;
    ObjModule* module;
} BtlTranspiler;

// ----------------------------------------------------------------------------
// Public API
// ----------------------------------------------------------------------------

BtlTranspiler* btl_transpiler_new(BtlTranspilerConfig config);
void btl_transpiler_free(BtlTranspiler* t);
bool btl_transpiler_emit_program(BtlTranspiler* t, ObjFunction* main_fn, ObjModule* module);

#endif
