#include <stdio.h>
#include <string.h>
#include "object.h"
#include "memory.h"
#include "value.h"

void initValueArray(ValueArray* array) {
    array->values = NULL; array->capacity = 0; array->count = 0;
}

void writeValueArray(struct VM* vm, ValueArray* array, Value value) {
    if (array->capacity < array->count + 1) {
        int old = array->capacity; array->capacity = GROW_CAPACITY(old);
        array->values = GROW_ARRAY(vm, Value, array->values, old, array->capacity);
    }
    array->values[array->count] = value; array->count++;
}

void freeValueArray(struct VM* vm, ValueArray* array) {
    FREE_ARRAY(vm, Value, array->values, array->capacity); initValueArray(array);
}

void printValueStderr(Value value) {
#ifdef NAN_BOXING
    if (IS_NUMBER(value)) {
        fprintf(stderr, "%g", AS_NUMBER(value));
    } else if (IS_NIL(value)) {
        fprintf(stderr, "nil");
    } else if (IS_BOOL(value)) {
        fprintf(stderr, AS_BOOL(value) ? "true" : "false");
    } else if (IS_OBJ(value)) {
        // Handle Object types
        switch (OBJ_TYPE(value)) {
        case OBJ_BOUND_METHOD:
            fprintf(stderr, "<method %s>", AS_BOUND_METHOD(value)->method->function->name->chars);
            break;
        case OBJ_CLASS:
            fprintf(stderr, "%s", AS_CLASS(value)->name->chars);
            break;
        case OBJ_CLOSURE:
            fprintf(stderr, "<fn %s>", AS_CLOSURE(value)->function->name->chars);
            break;
        case OBJ_FUNCTION:
            fprintf(stderr, "<fn %s>", AS_FUNCTION(value)->name->chars);
            break;
        case OBJ_INSTANCE:
            fprintf(stderr, "%s instance", AS_INSTANCE(value)->klass->name->chars);
            break;
        case OBJ_NATIVE:
            fprintf(stderr, "<native fn>");
            break;
        case OBJ_STRING:
            fprintf(stderr, "%s", AS_CSTRING(value));
            break;
        case OBJ_UPVALUE:
            fprintf(stderr, "upvalue");
            break;
        case OBJ_LIST:
            fprintf(stderr, "[list]");
            break;
        case OBJ_MODULE:
            fprintf(stderr, "<module %s>", AS_MODULE(value)->name->chars);
            break;
        default:
            fprintf(stderr, "<unknown obj type %d>", OBJ_TYPE(value));
            break;
        }
    } else {
        fprintf(stderr, "<unknown nan-boxed value %lx>", value);
    }
#else
    // Fallback for non-NaN-boxed builds (Tagged Union)
    switch (value.type) {
    case VAL_BOOL:   fprintf(stderr, AS_BOOL(value) ? "true" : "false"); break;
    case VAL_NIL:    fprintf(stderr, "nil"); break;
    case VAL_NUMBER: fprintf(stderr, "%g", AS_NUMBER(value)); break;
    case VAL_OBJ:    printObjectStderr(value); break;
    case VAL_EMPTY: printf("<empty>"); break;
    }
#endif
}

void printValue(Value value) {
#ifdef NAN_BOXING
    if (IS_BOOL(value)) printf(AS_BOOL(value) ? "true" : "false");
    else if (IS_NIL(value)) printf("nil");
    else if (IS_NUMBER(value)) printf("%g", AS_NUMBER(value));
    else if (IS_EMPTY(value)) printf("<empty>");
    else if (IS_OBJ(value)) printObject(value);
#else
    switch (value.type) {
    case VAL_BOOL: printf(AS_BOOL(value) ? "true" : "false"); break;
    case VAL_NIL: printf("nil"); break;
    case VAL_NUMBER: printf("%g", AS_NUMBER(value)); break;
    case VAL_OBJ: printObject(value); break;
    case VAL_EMPTY: printf("<empty>"); break;
    }
#endif
}

bool valuesEqual(Value a, Value b) {
#ifdef NAN_BOXING
    if (IS_NUMBER(a) && IS_NUMBER(b)) return AS_NUMBER(a) == AS_NUMBER(b);
    return a == b;
#else
    if (a.type != b.type) return false;
    switch (a.type) {
    case VAL_BOOL: return AS_BOOL(a) == AS_BOOL(b);
    case VAL_NIL: return true;
    case VAL_NUMBER: return AS_NUMBER(a) == AS_NUMBER(b);
    case VAL_OBJ: return AS_OBJ(a) == AS_OBJ(b);
    case VAL_EMPTY: return true;
    default: return false;
    }
#endif
}