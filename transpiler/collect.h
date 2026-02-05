/*
 * Function Collector
 *
 * Walks bytecode starting from the top-level function and discovers all
 * ObjFunction* values referenced in constant pools (via OP_CLOSURE).
 * Assigns each a stable integer ID used as the C function suffix.
 */

#ifndef btl_collect_h
#define btl_collect_h

#include <stdbool.h>

typedef struct ObjFunction ObjFunction;

typedef struct {
    ObjFunction** functions;
    int count;
    int capacity;
} FunctionList;

// Recursively collect all functions reachable from main_fn
void collect_functions(ObjFunction* main_fn, FunctionList* out);

// Find the ID of a function (its index in the list), or -1
int function_id(FunctionList* list, ObjFunction* fn);

void function_list_init(FunctionList* list);
void function_list_free(FunctionList* list);

#endif // BTL_COLLECT_H