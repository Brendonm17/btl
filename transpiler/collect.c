// ============================================================================
// collect.c - BTL Function Collector Implementation
//
// Scans constant pools for ObjFunction values. Each OP_CLOSURE references
// a function in the constant pool; we walk those recursively.
// ============================================================================

#include "collect.h"
#include "../src/object.h"
#include "../src/chunk.h"
#include "../src/value.h"
#include <stdlib.h>
#include <string.h>

// ----------------------------------------------------------------------------
// Function List Management
// ----------------------------------------------------------------------------

void btl_function_list_init(BtlFunctionList* list) {
    list->functions = NULL;
    list->count = 0;
    list->capacity = 0;
}

void btl_function_list_free(BtlFunctionList* list) {
    free(list->functions);
    list->functions = NULL;
    list->count = 0;
    list->capacity = 0;
}

static void ensure_capacity(BtlFunctionList* list) {
    if (list->count < list->capacity) return;
    int new_cap = list->capacity < 8 ? 8 : list->capacity * 2;
    list->functions = realloc(list->functions, sizeof(ObjFunction*) * new_cap);
    list->capacity = new_cap;
}

static bool already_collected(BtlFunctionList* list, ObjFunction* fn) {
    for (int i = 0; i < list->count; i++) {
        if (list->functions[i] == fn) return true;
    }
    return false;
}

// ----------------------------------------------------------------------------
// Recursive Collection
// ----------------------------------------------------------------------------

static void collect_recursive(ObjFunction* fn, BtlFunctionList* out) {
    if (fn == NULL || already_collected(out, fn)) return;

    // Add this function
    ensure_capacity(out);
    out->functions[out->count++] = fn;

    // Scan its constant pool for nested functions
    BtlValueArray* constants = &fn->chunk.constants;
    for (int i = 0; i < constants->count; i++) {
        BtlValue v = constants->values[i];
        if (IS_OBJ(v) && OBJ_TYPE(v) == BTL_OBJ_FUNCTION) {
            collect_recursive(AS_FUNCTION(v), out);
        }
    }
}

void btl_collect_functions(ObjFunction* main_fn, BtlFunctionList* out) {
    collect_recursive(main_fn, out);
}

int btl_function_id(BtlFunctionList* list, ObjFunction* fn) {
    for (int i = 0; i < list->count; i++) {
        if (list->functions[i] == fn) return i;
    }
    return -1;
}
