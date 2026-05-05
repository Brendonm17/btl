#include <stdio.h>
#include <string.h>
#include "object.h"
#include "memory.h"
#include "value.h"

void btl_value_array_init(BtlValueArray* array) {
    array->values = NULL;
    array->capacity = 0;
    array->count = 0;
}

void btl_value_array_write(struct VM* vm, BtlValueArray* array, BtlValue value) {
    if (array->capacity < array->count + 1) {
        int oldCapacity = array->capacity;
        array->capacity = BTL_GROW_CAPACITY(oldCapacity);
        array->values = BTL_GROW_ARRAY(vm, BtlValue, array->values,
                                        oldCapacity, array->capacity);
    }

    array->values[array->count] = value;
    array->count++;
}

void btl_value_array_free(struct VM* vm, BtlValueArray* array) {
    BTL_FREE_ARRAY(vm, BtlValue, array->values, array->capacity);
    btl_value_array_init(array);
}

void btl_value_print(BtlValue value) {
#ifdef BTL_NAN_BOXING
    if (IS_BOOL(value)) {
        printf(AS_BOOL(value) ? "true" : "false");
    } else if (IS_NULL(value)) {
        printf("null");
    } else if (IS_INT(value)) {
        printf("%" PRId64, AS_INT(value));
    } else if (IS_NUMBER(value)) {
        printf("%g", AS_NUMBER(value));
    } else if (IS_EMPTY(value)) {
        printf("<empty>");
    } else if (IS_OBJ(value)) {
        btl_object_print(value);
    }
#else
    switch (value.type) {
        case BTL_VAL_BOOL:
            printf(AS_BOOL(value) ? "true" : "false");
            break;
        case BTL_VAL_NIL:
            printf("null");
            break;
        case BTL_VAL_INT:
            printf("%" PRId64, AS_INT(value));
            break;
        case BTL_VAL_NUMBER:
            printf("%g", AS_NUMBER(value));
            break;
        case BTL_VAL_OBJ:
            btl_object_print(value);
            break;
        case BTL_VAL_EMPTY:
            printf("<empty>");
            break;
    }
#endif
}

bool btl_values_equal(BtlValue a, BtlValue b) {
#ifdef BTL_NAN_BOXING
    if (IS_INT(a) && IS_INT(b)) {
        return AS_INT(a) == AS_INT(b);
    }
    // Cross-type numeric: promote int to double
    if (IS_INT(a) && IS_NUMBER(b)) {
        return (double)AS_INT(a) == AS_NUMBER(b);
    }
    if (IS_NUMBER(a) && IS_INT(b)) {
        return AS_NUMBER(a) == (double)AS_INT(b);
    }
    // NaN != NaN, so compare numbers by value
    if (IS_NUMBER(a) && IS_NUMBER(b)) {
        return AS_NUMBER(a) == AS_NUMBER(b);
    }
    // Entities compare by id, not pointer
    if (IS_OBJ(a) && IS_OBJ(b) &&
        AS_OBJ(a)->type == BTL_OBJ_ENTITY && AS_OBJ(b)->type == BTL_OBJ_ENTITY) {
        return ((ObjEntity*)AS_OBJ(a))->id == ((ObjEntity*)AS_OBJ(b))->id;
    }
    return a == b;
#else
    if ((a.type == BTL_VAL_INT && b.type == BTL_VAL_NUMBER) ||
        (a.type == BTL_VAL_NUMBER && b.type == BTL_VAL_INT)) {
        return btl_numeric_to_double(a) == btl_numeric_to_double(b);
    }

    if (a.type != b.type) {
        return false;
    }

    switch (a.type) {
        case BTL_VAL_BOOL:
            return AS_BOOL(a) == AS_BOOL(b);
        case BTL_VAL_NIL:
            return true;
        case BTL_VAL_NUMBER:
            return AS_NUMBER(a) == AS_NUMBER(b);
        case BTL_VAL_INT:
            return AS_INT(a) == AS_INT(b);
        case BTL_VAL_OBJ:
            // Entities compare by id, not pointer
            if (AS_OBJ(a)->type == BTL_OBJ_ENTITY && AS_OBJ(b)->type == BTL_OBJ_ENTITY) {
                return ((ObjEntity*)AS_OBJ(a))->id == ((ObjEntity*)AS_OBJ(b))->id;
            }
            return AS_OBJ(a) == AS_OBJ(b);
        case BTL_VAL_EMPTY:
            return true;
        default:
            return false;
    }
#endif
}
