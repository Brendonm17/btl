// BTL value representation. With BTL_NAN_BOXING all values pack into 64 bits
// using IEEE 754 NaN bits. Without it, falls back to a tagged union.
//
// Types: number (double), int (48-bit), bool, null, object, empty (sentinel
// for uninitialized slots).

#ifndef btl_value_h
#define btl_value_h

#include <string.h>
#include <stdint.h>
#include <inttypes.h>
#include "common.h"

struct Obj;
struct VM;

typedef struct BtlObj BtlObj;
typedef struct ObjString BtlObjString;

#ifdef BTL_NAN_BOXING

// Quiet NaN has all exponent bits + the quiet bit set. The remaining 51 bits
// hold a tag or pointer:
//   numbers   any bit pattern that is NOT a quiet NaN
//   nil/bool  QNAN | tag (in low bits)
//   ints      QNAN | INT_BIT | 48-bit two's complement
//   objects   SIGN_BIT | QNAN | 48-bit pointer

#define BTL_SIGN_BIT ((uint64_t)0x8000000000000000)
#define BTL_QNAN     ((uint64_t)0x7ffc000000000000)

#define BTL_TAG_NIL   1
#define BTL_TAG_FALSE 2
#define BTL_TAG_TRUE  3
#define BTL_TAG_EMPTY 4

// INT_BIT is bit 48. Tags 1..4 have bit 48 clear, objects have SIGN_BIT set,
// so int encoding is unambiguous.
#define BTL_INT_BIT   ((uint64_t)1 << 48)
#define BTL_INT_TAG   (BTL_QNAN | BTL_INT_BIT)
#define BTL_INT_MASK  ((uint64_t)0x0000FFFFFFFFFFFF)

typedef uint64_t BtlValue;

#define IS_BOOL(v)      (((v) | 1) == BTL_TRUE_VAL)
#define IS_NULL(v)      ((v) == BTL_NULL_VAL)
#define IS_EMPTY(v)     ((v) == BTL_EMPTY_VAL)
#define IS_NUMBER(v)    (((v) & BTL_QNAN) != BTL_QNAN)
#define IS_INT(v)       (((v) & (BTL_QNAN | BTL_INT_BIT | BTL_SIGN_BIT)) == BTL_INT_TAG)
#define IS_NUMERIC(v)   (IS_NUMBER(v) || IS_INT(v))
#define IS_OBJ(v)       (((v) & (BTL_QNAN | BTL_SIGN_BIT)) == (BTL_QNAN | BTL_SIGN_BIT))

#define AS_BOOL(v)      ((v) == BTL_TRUE_VAL)
#define AS_NUMBER(v)    btl_value_to_num(v)
#define AS_INT(v)       btl_value_to_int(v)
#define AS_OBJ(v)       ((struct BtlObj*)(uintptr_t)((v) & ~(BTL_SIGN_BIT | BTL_QNAN)))

#define BOOL_VAL(b)     ((b) ? BTL_TRUE_VAL : BTL_FALSE_VAL)
#define BTL_FALSE_VAL   ((BtlValue)(uint64_t)(BTL_QNAN | BTL_TAG_FALSE))
#define BTL_TRUE_VAL    ((BtlValue)(uint64_t)(BTL_QNAN | BTL_TAG_TRUE))
#define BTL_NULL_VAL    ((BtlValue)(uint64_t)(BTL_QNAN | BTL_TAG_NIL))
#define BTL_EMPTY_VAL   ((BtlValue)(uint64_t)(BTL_QNAN | BTL_TAG_EMPTY))
#define NUMBER_VAL(num) btl_num_to_value(num)
#define INT_VAL(i)      btl_int_to_value(i)
#define OBJ_VAL(obj)    (BtlValue)(BTL_SIGN_BIT | BTL_QNAN | (uint64_t)(uintptr_t)(obj))

// memcpy for type punning to keep strict aliasing happy.
static inline double btl_value_to_num(BtlValue v) {
    double num;
    memcpy(&num, &v, sizeof(BtlValue));
    return num;
}

static inline BtlValue btl_num_to_value(double num) {
    BtlValue v;
    memcpy(&v, &num, sizeof(double));
    return v;
}

// 64-bit int truncated to 48 bits.
static inline BtlValue btl_int_to_value(int64_t i) {
    return BTL_INT_TAG | ((uint64_t)i & BTL_INT_MASK);
}

// Sign-extend the 48-bit two's complement back to int64.
static inline int64_t btl_value_to_int(BtlValue v) {
    int64_t raw = (int64_t)(v & BTL_INT_MASK);
    if (raw & ((int64_t)1 << 47)) {
        raw |= (int64_t)0xFFFF000000000000LL;
    }
    return raw;
}

static inline double btl_numeric_to_double(BtlValue v) {
    if (IS_INT(v)) return (double)btl_value_to_int(v);
    return btl_value_to_num(v);
}

#else // !BTL_NAN_BOXING

typedef enum {
    BTL_VAL_BOOL,
    BTL_VAL_NIL,
    BTL_VAL_NUMBER,
    BTL_VAL_INT,
    BTL_VAL_OBJ,
    BTL_VAL_EMPTY
} BtlValueType;

typedef struct {
    BtlValueType type;
    union {
        bool boolean;
        double number;
        int64_t integer;
        struct BtlObj* obj;
    } as;
} BtlValue;

#define IS_BOOL(v)    ((v).type == BTL_VAL_BOOL)
#define IS_NULL(v)    ((v).type == BTL_VAL_NIL)
#define IS_EMPTY(v)   ((v).type == BTL_VAL_EMPTY)
#define IS_NUMBER(v)  ((v).type == BTL_VAL_NUMBER)
#define IS_INT(v)     ((v).type == BTL_VAL_INT)
#define IS_NUMERIC(v) ((v).type == BTL_VAL_NUMBER || (v).type == BTL_VAL_INT)
#define IS_OBJ(v)     ((v).type == BTL_VAL_OBJ)

#define AS_BOOL(v)    ((v).as.boolean)
#define AS_NUMBER(v)  ((v).as.number)
#define AS_INT(v)     ((v).as.integer)
#define AS_OBJ(v)     ((v).as.obj)

#define BOOL_VAL(v)   ((BtlValue){BTL_VAL_BOOL, {.boolean = v}})
#define BTL_NULL_VAL  ((BtlValue){BTL_VAL_NIL, {.number = 0}})
#define BTL_EMPTY_VAL ((BtlValue){BTL_VAL_EMPTY, {.number = 0}})
#define NUMBER_VAL(v) ((BtlValue){BTL_VAL_NUMBER, {.number = v}})
#define INT_VAL(v)    ((BtlValue){BTL_VAL_INT, {.integer = v}})
#define OBJ_VAL(v)    ((BtlValue){BTL_VAL_OBJ, {.obj = (struct BtlObj*)v}})

static inline double btl_numeric_to_double(BtlValue v) {
    if (v.type == BTL_VAL_INT) return (double)v.as.integer;
    return v.as.number;
}

#endif

// Growable value array. Used for constant pools and similar.
typedef struct {
    int capacity;
    int count;
    BtlValue* values;
} BtlValueArray;

bool btl_values_equal(BtlValue a, BtlValue b);

void btl_value_array_init(BtlValueArray* array);
void btl_value_array_write(struct VM* vm, BtlValueArray* array, BtlValue value);
void btl_value_array_free(struct VM* vm, BtlValueArray* array);

void btl_value_print(BtlValue value);

#endif
