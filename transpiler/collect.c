// Walks constant pools for ObjFunction values reachable from a top-level
// function. OP_CLOSURE references a function in the pool; we recurse from
// there. ObjFunction->transpilerId gives O(1) dedupe and ID lookup.

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
    // Clear transpilerId on all collected functions so the tag doesn't leak
    for (int i = 0; i < list->count; i++) {
        list->functions[i]->transpilerId = -1;
    }
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

// ----------------------------------------------------------------------------
// Recursive Collection
//
// Uses transpilerId >= 0 as the "already collected" marker (O(1) check).
// ----------------------------------------------------------------------------

static void collect_recursive(ObjFunction* fn, BtlFunctionList* out) {
    if (fn == NULL || fn->transpilerId >= 0) return;

    // Assign ID and add to list
    ensure_capacity(out);
    fn->transpilerId = out->count;
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

// O(1) function ID lookup via the pre-assigned transpilerId field.
int btl_function_id(BtlFunctionList* list, ObjFunction* fn) {
    (void)list;  // no longer needed for lookup
    if (fn == NULL) return -1;
    return fn->transpilerId;
}
