// ============================================================================
// value.c - BTL Value Operations
//
// This file implements operations on BTL values including equality comparison,
// printing, and dynamic array management for value collections.
// ============================================================================

#include <stdio.h>
#include <string.h>
#include "object.h"
#include "memory.h"
#include "value.h"

// ----------------------------------------------------------------------------
// btl_value_array_init
//
// Initialize a value array to empty state. Must be called before using
// the array.
// ----------------------------------------------------------------------------
void btl_value_array_init(BtlValueArray* array) {
    array->values = NULL;
    array->capacity = 0;
    array->count = 0;
}

// ----------------------------------------------------------------------------
// btl_value_array_write
//
// Append a value to the array. If the array is full, it will be grown
// automatically. This may trigger garbage collection.
//
// Parameters:
//   vm    - Virtual machine (for memory allocation)
//   array - The value array to append to
//   value - The value to append
// ----------------------------------------------------------------------------
void btl_value_array_write(struct VM* vm, BtlValueArray* array, BtlValue value) {
    // Check if we need to grow the array
    if (array->capacity < array->count + 1) {
        int oldCapacity = array->capacity;
        array->capacity = BTL_GROW_CAPACITY(oldCapacity);
        array->values = BTL_GROW_ARRAY(vm, BtlValue, array->values,
                                        oldCapacity, array->capacity);
    }

    // Store the value and increment count
    array->values[array->count] = value;
    array->count++;
}

// ----------------------------------------------------------------------------
// btl_value_array_free
//
// Free all memory used by a value array and reset it to empty state.
//
// Parameters:
//   vm    - Virtual machine (for memory deallocation)
//   array - The value array to free
// ----------------------------------------------------------------------------
void btl_value_array_free(struct VM* vm, BtlValueArray* array) {
    BTL_FREE_ARRAY(vm, BtlValue, array->values, array->capacity);
    btl_value_array_init(array);
}

// ----------------------------------------------------------------------------
// btl_value_print
//
// Print a value to stdout. Used for debugging and the REPL.
// Handles all value types including objects.
//
// Parameters:
//   value - The value to print
// ----------------------------------------------------------------------------
void btl_value_print(BtlValue value) {
#ifdef BTL_NAN_BOXING
    // NaN boxing: check type by examining bit patterns
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
    // Tagged union: switch on type field
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

// ----------------------------------------------------------------------------
// btl_values_equal
//
// Compare two values for equality. Numbers are compared by value,
// objects are compared by identity (pointer equality), and other
// types use direct comparison.
//
// Parameters:
//   a - First value
//   b - Second value
//
// Returns:
//   true if values are equal, false otherwise
// ----------------------------------------------------------------------------
bool btl_values_equal(BtlValue a, BtlValue b) {
#ifdef BTL_NAN_BOXING
    // Int-int comparison
    if (IS_INT(a) && IS_INT(b)) {
        return AS_INT(a) == AS_INT(b);
    }
    // Cross-type numeric comparison (int == number): promote and compare
    if (IS_INT(a) && IS_NUMBER(b)) {
        return (double)AS_INT(a) == AS_NUMBER(b);
    }
    if (IS_NUMBER(a) && IS_INT(b)) {
        return AS_NUMBER(a) == (double)AS_INT(b);
    }
    // Special case for numbers: must compare by value since NaN != NaN
    if (IS_NUMBER(a) && IS_NUMBER(b)) {
        return AS_NUMBER(a) == AS_NUMBER(b);
    }
    // All other types: direct bit comparison works
    return a == b;
#else
    // Cross-type numeric comparison (int == number)
    if ((a.type == BTL_VAL_INT && b.type == BTL_VAL_NUMBER) ||
        (a.type == BTL_VAL_NUMBER && b.type == BTL_VAL_INT)) {
        return btl_numeric_to_double(a) == btl_numeric_to_double(b);
    }

    // Tagged union: types must match first
    if (a.type != b.type) {
        return false;
    }

    switch (a.type) {
        case BTL_VAL_BOOL:
            return AS_BOOL(a) == AS_BOOL(b);
        case BTL_VAL_NIL:
            return true;  // All nils are equal
        case BTL_VAL_NUMBER:
            return AS_NUMBER(a) == AS_NUMBER(b);
        case BTL_VAL_INT:
            return AS_INT(a) == AS_INT(b);
        case BTL_VAL_OBJ:
            return AS_OBJ(a) == AS_OBJ(b);  // Identity comparison
        case BTL_VAL_EMPTY:
            return true;  // All emptys are equal
        default:
            return false;
    }
#endif
}
