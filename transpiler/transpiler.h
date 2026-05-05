// BTL bytecode to C transpiler.

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
} BtlTranspilerConfig;

#define BTL_MAX_GLOBAL_SLOTS 512

typedef struct {
    FILE* out;
    BtlTranspilerConfig config;

    BtlFunctionList fns;

    ObjFunction* current_fn;
    int current_fn_id;
    ObjModule* module;
    VM* vm;

    // Reverse lookup table built once per module: global slot index -> name.
    const char* globalNameBySlot[BTL_MAX_GLOBAL_SLOTS];
    int globalNameCount;
} BtlTranspiler;

BtlTranspiler* btl_transpiler_new(BtlTranspilerConfig config, VM* vm);
void btl_transpiler_free(BtlTranspiler* t);
bool btl_transpiler_emit_program(BtlTranspiler* t, ObjFunction* main_fn, ObjModule* module);

#endif
