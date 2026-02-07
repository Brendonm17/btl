// ============================================================================
// value.h - BTL Value Representation
//
// This header defines how BTL values are represented in memory. BTL uses
// NaN boxing when enabled, which packs type information into the unused
// bits of IEEE 754 NaN values, allowing all values to fit in 64 bits.
//
// Value types:
// - Number: IEEE 754 double-precision float
// - Int: 48-bit signed integer (NaN-boxed immediate, no heap allocation)
// - Bool: true or false
// - Nil: null/none value
// - Object: Heap-allocated objects (strings, functions, classes, etc.)
// - Empty: Internal sentinel for uninitialized slots
// ============================================================================

#ifndef btl_value_h
#define btl_value_h

#include <string.h>
#include <stdint.h>
#include <inttypes.h>
#include "common.h"

// ----------------------------------------------------------------------------
// Forward declarations
// ----------------------------------------------------------------------------
struct Obj;
struct VM;

typedef struct BtlObj BtlObj;
typedef struct ObjString BtlObjString;

// ----------------------------------------------------------------------------
// NaN Boxing Implementation
//
// IEEE 754 double has this bit layout:
//   Sign (1 bit) | Exponent (11 bits) | Mantissa (52 bits)
//
// A quiet NaN has all exponent bits set and the quiet bit (bit 51) set.
// We use the remaining 51 bits to encode type tags and object pointers.
//
// Encoding scheme:
// - Numbers: Any bit pattern that is NOT a quiet NaN
// - Nil/Bool/Empty: QNAN | tag (tag in lowest bits)
// - Objects: SIGN_BIT | QNAN | pointer (pointer in lower 48 bits)
// ----------------------------------------------------------------------------
#ifdef BTL_NAN_BOXING

// Sign bit for distinguishing objects from other tagged values
#define BTL_SIGN_BIT ((uint64_t)0x8000000000000000)

// Quiet NaN pattern (all exponent bits + quiet bit set)
#define BTL_QNAN     ((uint64_t)0x7ffc000000000000)

// Type tags for non-object values (stored in low bits of QNAN)
#define BTL_TAG_NIL   1  // null/nil value
#define BTL_TAG_FALSE 2  // boolean false
#define BTL_TAG_TRUE  3  // boolean true
#define BTL_TAG_EMPTY 4  // uninitialized slot sentinel

// Integer tag: bit 48 set within QNAN space (does not conflict with tags 1-4
// which have bit 48 clear, or objects which have SIGN_BIT set)
#define BTL_INT_BIT   ((uint64_t)1 << 48)
#define BTL_INT_TAG   (BTL_QNAN | BTL_INT_BIT)
#define BTL_INT_MASK  ((uint64_t)0x0000FFFFFFFFFFFF)  // lower 48 bits

// Value is a 64-bit unsigned integer
typedef uint64_t BtlValue;

// ----------------------------------------------------------------------------
// Type checking macros
// ----------------------------------------------------------------------------

// Check if value is a boolean (either true or false)
#define IS_BOOL(v)      (((v) | 1) == BTL_TRUE_VAL)

// Check if value is nil
#define IS_NULL(v)      ((v) == BTL_NULL_VAL)

// Check if value is the empty sentinel
#define IS_EMPTY(v)     ((v) == BTL_EMPTY_VAL)

// Check if value is a number (not a quiet NaN)
#define IS_NUMBER(v)    (((v) & BTL_QNAN) != BTL_QNAN)

// Check if value is a 48-bit integer (QNAN + INT_BIT set, SIGN_BIT clear)
#define IS_INT(v)        (((v) & (BTL_QNAN | BTL_INT_BIT | BTL_SIGN_BIT)) == BTL_INT_TAG)

// Check if value is any numeric type (double or int)
#define IS_NUMERIC(v)    (IS_NUMBER(v) || IS_INT(v))

// Check if value is a heap object (has sign bit and QNAN set)
#define IS_OBJ(v)       (((v) & (BTL_QNAN | BTL_SIGN_BIT)) == (BTL_QNAN | BTL_SIGN_BIT))

// ----------------------------------------------------------------------------
// Value extraction macros
// ----------------------------------------------------------------------------

// Extract boolean value
#define AS_BOOL(v)      ((v) == BTL_TRUE_VAL)

// Extract number value (uses helper function for type punning)
#define AS_NUMBER(v)    btl_value_to_num(v)

// Extract integer value (sign-extend 48-bit two's complement)
#define AS_INT(v)        btl_value_to_int(v)

// Extract object pointer (mask off tag bits)
#define AS_OBJ(v)       ((struct BtlObj*)(uintptr_t)((v) & ~(BTL_SIGN_BIT | BTL_QNAN)))

// ----------------------------------------------------------------------------
// Value creation macros
// ----------------------------------------------------------------------------

// Create boolean value
#define BOOL_VAL(b)     ((b) ? BTL_TRUE_VAL : BTL_FALSE_VAL)

// Predefined boolean values
#define BTL_FALSE_VAL   ((BtlValue)(uint64_t)(BTL_QNAN | BTL_TAG_FALSE))
#define BTL_TRUE_VAL    ((BtlValue)(uint64_t)(BTL_QNAN | BTL_TAG_TRUE))

// Nil value
#define BTL_NULL_VAL    ((BtlValue)(uint64_t)(BTL_QNAN | BTL_TAG_NIL))

// Empty sentinel value
#define BTL_EMPTY_VAL   ((BtlValue)(uint64_t)(BTL_QNAN | BTL_TAG_EMPTY))

// Create number value (uses helper function for type punning)
#define NUMBER_VAL(num) btl_num_to_value(num)

// Create integer value (48-bit two's complement in QNAN space)
#define INT_VAL(i)      btl_int_to_value(i)

// Create object value (set sign bit and QNAN, embed pointer)
#define OBJ_VAL(obj)    (BtlValue)(BTL_SIGN_BIT | BTL_QNAN | (uint64_t)(uintptr_t)(obj))

// ----------------------------------------------------------------------------
// Helper functions for type punning (avoids strict aliasing violations)
// ----------------------------------------------------------------------------

// Convert NaN-boxed value to double
static inline double btl_value_to_num(BtlValue v) {
    double num;
    memcpy(&num, &v, sizeof(BtlValue));
    return num;
}

// Convert double to NaN-boxed value
static inline BtlValue btl_num_to_value(double num) {
    BtlValue v;
    memcpy(&v, &num, sizeof(double));
    return v;
}

// Convert 64-bit integer to NaN-boxed int value (truncated to 48 bits)
static inline BtlValue btl_int_to_value(int64_t i) {
    return BTL_INT_TAG | ((uint64_t)i & BTL_INT_MASK);
}

// Extract 48-bit two's complement integer, sign-extended to int64_t
static inline int64_t btl_value_to_int(BtlValue v) {
    int64_t raw = (int64_t)(v & BTL_INT_MASK);
    // Sign-extend from bit 47
    if (raw & ((int64_t)1 << 47)) {
        raw |= (int64_t)0xFFFF000000000000LL;
    }
    return raw;
}

// Extract any numeric value as double (works for both int and number)
static inline double btl_numeric_to_double(BtlValue v) {
    if (IS_INT(v)) return (double)btl_value_to_int(v);
    return btl_value_to_num(v);
}

#else // !BTL_NAN_BOXING

// ----------------------------------------------------------------------------
// Tagged Union Implementation (fallback when NaN boxing is disabled)
// ----------------------------------------------------------------------------

// Value type enumeration
typedef enum {
    BTL_VAL_BOOL,    // Boolean value
    BTL_VAL_NIL,     // Nil/null value
    BTL_VAL_NUMBER,  // Number value
    BTL_VAL_INT,     // Integer value
    BTL_VAL_OBJ,     // Heap object
    BTL_VAL_EMPTY    // Empty sentinel
} BtlValueType;

// Tagged union value structure
typedef struct {
    BtlValueType type;
    union {
        bool boolean;
        double number;
        int64_t integer;
        struct BtlObj* obj;
    } as;
} BtlValue;

// Type checking macros
#define IS_BOOL(v)    ((v).type == BTL_VAL_BOOL)
#define IS_NULL(v)    ((v).type == BTL_VAL_NIL)
#define IS_EMPTY(v)   ((v).type == BTL_VAL_EMPTY)
#define IS_NUMBER(v)  ((v).type == BTL_VAL_NUMBER)
#define IS_INT(v)     ((v).type == BTL_VAL_INT)
#define IS_NUMERIC(v) ((v).type == BTL_VAL_NUMBER || (v).type == BTL_VAL_INT)
#define IS_OBJ(v)     ((v).type == BTL_VAL_OBJ)

// Value extraction macros
#define AS_BOOL(v)    ((v).as.boolean)
#define AS_NUMBER(v)  ((v).as.number)
#define AS_INT(v)     ((v).as.integer)
#define AS_OBJ(v)     ((v).as.obj)

// Value creation macros
#define BOOL_VAL(v)   ((BtlValue){BTL_VAL_BOOL, {.boolean = v}})
#define BTL_NULL_VAL  ((BtlValue){BTL_VAL_NIL, {.number = 0}})
#define BTL_EMPTY_VAL ((BtlValue){BTL_VAL_EMPTY, {.number = 0}})
#define NUMBER_VAL(v) ((BtlValue){BTL_VAL_NUMBER, {.number = v}})
#define INT_VAL(v)    ((BtlValue){BTL_VAL_INT, {.integer = v}})
#define OBJ_VAL(v)    ((BtlValue){BTL_VAL_OBJ, {.obj = (struct BtlObj*)v}})

// Extract any numeric value as double (works for both int and number)
static inline double btl_numeric_to_double(BtlValue v) {
    if (v.type == BTL_VAL_INT) return (double)v.as.integer;
    return v.as.number;
}

#endif // BTL_NAN_BOXING

// ----------------------------------------------------------------------------
// Dynamic Value Array
//
// A growable array of values, used for constant pools and other collections.
// ----------------------------------------------------------------------------
typedef struct {
    int capacity;     // Allocated capacity
    int count;        // Number of values stored
    BtlValue* values; // Array of values
} BtlValueArray;

// ----------------------------------------------------------------------------
// Value Operations
// ----------------------------------------------------------------------------

// Compare two values for equality
bool btl_values_equal(BtlValue a, BtlValue b);

// Initialize a value array
void btl_value_array_init(BtlValueArray* array);

// Append a value to the array (may trigger GC)
void btl_value_array_write(struct VM* vm, BtlValueArray* array, BtlValue value);

// Free a value array
void btl_value_array_free(struct VM* vm, BtlValueArray* array);

// Print a value to stdout (for debugging)
void btl_value_print(BtlValue value);

#endif // btl_value_h
