/*
 * Function Collector implementation
 *
 * Scans constant pools for ObjFunction values. Each OP_CLOSURE references
 * a function in the constant pool; we walk those recursively.
 */

#include "collect.h"
#include "../src/object.h"
#include "../src/chunk.h"
#include "../src/value.h"
#include <stdlib.h>
#include <string.h>

void function_list_init(FunctionList* list) {
    list->functions = NULL;
    list->count = 0;
    list->capacity = 0;
}

void function_list_free(FunctionList* list) {
    free(list->functions);
    list->functions = NULL;
    list->count = 0;
    list->capacity = 0;
}

static void ensure_capacity(FunctionList* list) {
    if (list->count < list->capacity) return;
    int new_cap = list->capacity < 8 ? 8 : list->capacity * 2;
    list->functions = realloc(list->functions, sizeof(ObjFunction*) * new_cap);
    list->capacity = new_cap;
}

static bool already_collected(FunctionList* list, ObjFunction* fn) {
    for (int i = 0; i < list->count; i++) {
        if (list->functions[i] == fn) return true;
    }
    return false;
}

static void collect_recursive(ObjFunction* fn, FunctionList* out) {
    if (fn == NULL || already_collected(out, fn)) return;

    // Add this function
    ensure_capacity(out);
    out->functions[out->count++] = fn;

    // Scan its constant pool for nested functions
    ValueArray* constants = &fn->chunk.constants;
    for (int i = 0; i < constants->count; i++) {
        Value v = constants->values[i];
        if (IS_OBJ(v) && OBJ_TYPE(v) == OBJ_FUNCTION) {
            collect_recursive(AS_FUNCTION(v), out);
        }
    }
}

void collect_functions(ObjFunction* main_fn, FunctionList* out) {
    collect_recursive(main_fn, out);
}

int function_id(FunctionList* list, ObjFunction* fn) {
    for (int i = 0; i < list->count; i++) {
        if (list->functions[i] == fn) return i;
    }
    return -1;
}