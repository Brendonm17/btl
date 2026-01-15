#include <stdio.h>
#include <string.h>
#include "object.h"
#include "memory.h"
#include "value.h"

void initValueArray(ValueArray* array) {
    array->values = NULL;
    array->capacity = 0;
    array->count = 0;
}

void writeValueArray(struct VM* vm, ValueArray* array, Value value) {
    if (array->capacity < array->count + 1) {
        int old = array->capacity;
        array->capacity = GROW_CAPACITY(old);
        array->values = GROW_ARRAY(vm, Value, array->values, old, array->capacity);
    }
    array->values[array->count] = value;
    array->count++;
}

void freeValueArray(struct VM* vm, ValueArray* array) {
    FREE_ARRAY(vm, Value, array->values, array->capacity);
    initValueArray(array);
}

void printValue(Value value) {
#ifdef NAN_BOXING
    if (IS_BOOL(value)) printf(AS_BOOL(value) ? "true" : "false");
    else if (IS_NIL(value)) printf("nil");
    else if (IS_NUMBER(value)) printf("%g", AS_NUMBER(value));
    else if (IS_OBJ(value)) printObject(value);
#endif
}

bool valuesEqual(Value a, Value b) {
#ifdef NAN_BOXING
    if (IS_NUMBER(a) && IS_NUMBER(b)) return AS_NUMBER(a) == AS_NUMBER(b);
    return a == b;
#endif
}