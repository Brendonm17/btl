// Walks bytecode from the top-level function, discovers every ObjFunction*
// referenced in any constant pool (via OP_CLOSURE). Each gets a stable
// integer id used as the C function suffix.

#ifndef btl_collect_h
#define btl_collect_h

#include <stdbool.h>

typedef struct ObjFunction ObjFunction;

typedef struct {
    ObjFunction** functions;
    int count;
    int capacity;
} BtlFunctionList;

void btl_collect_functions(ObjFunction* main_fn, BtlFunctionList* out);

// Returns -1 if not found.
int btl_function_id(BtlFunctionList* list, ObjFunction* fn);

void btl_function_list_init(BtlFunctionList* list);
void btl_function_list_free(BtlFunctionList* list);

#endif
