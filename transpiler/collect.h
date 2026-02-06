// ============================================================================
// collect.h - BTL Function Collector
//
// Walks bytecode starting from the top-level function and discovers all
// ObjFunction* values referenced in constant pools (via OP_CLOSURE).
// Assigns each a stable integer ID used as the C function suffix.
// ============================================================================

#ifndef btl_collect_h
#define btl_collect_h

#include <stdbool.h>

typedef struct ObjFunction ObjFunction;

// ----------------------------------------------------------------------------
// Function List
// ----------------------------------------------------------------------------

typedef struct {
    ObjFunction** functions;
    int count;
    int capacity;
} BtlFunctionList;

// ----------------------------------------------------------------------------
// Public API
// ----------------------------------------------------------------------------

// Recursively collect all functions reachable from main_fn
void btl_collect_functions(ObjFunction* main_fn, BtlFunctionList* out);

// Find the ID of a function (its index in the list), or -1
int btl_function_id(BtlFunctionList* list, ObjFunction* fn);

void btl_function_list_init(BtlFunctionList* list);
void btl_function_list_free(BtlFunctionList* list);

#endif
