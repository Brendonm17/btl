#ifndef btl_value_h
#define btl_value_h

#include <string.h>
#include "common.h"

typedef struct Obj Obj;
typedef struct ObjString ObjString;

#ifdef NAN_BOXING

#define SIGN_BIT ((uint64_t)0x8000000000000000)
#define QNAN     ((uint64_t)0x7ffc000000000000)

#define TAG_NIL   1
#define TAG_FALSE 2
#define TAG_TRUE  3

typedef uint64_t Value;

#define IS_BOOL(v)      (((v) | 1) == TRUE_VAL)
#define IS_NIL(v)       ((v) == NIL_VAL)
#define IS_NUMBER(v)    (((v) & QNAN) != QNAN)
#define IS_OBJ(v)       (((v) & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT))

#define AS_BOOL(v)      ((v) == TRUE_VAL)
#define AS_NUMBER(v)    valueToNum(v)
#define AS_OBJ(v)       ((Obj*)(uintptr_t)((v) & ~(SIGN_BIT | QNAN)))

#define BOOL_VAL(b)     ((b) ? TRUE_VAL : FALSE_VAL)
#define FALSE_VAL       ((Value)(uint64_t)(QNAN | TAG_FALSE))
#define TRUE_VAL        ((Value)(uint64_t)(QNAN | TAG_TRUE))
#define NIL_VAL         ((Value)(uint64_t)(QNAN | TAG_NIL))
#define NUMBER_VAL(num) numToValue(num)
#define OBJ_VAL(obj)    (Value)(SIGN_BIT | QNAN | (uint64_t)(uintptr_t)(obj))

static inline double valueToNum(Value v) {
    double num; memcpy(&num, &v, sizeof(Value)); return num;
}

static inline Value numToValue(double num) {
    Value v; memcpy(&v, &num, sizeof(double)); return v;
}

#endif

typedef struct {
    int capacity;
    int count;
    Value* values;
} ValueArray;

bool valuesEqual(Value a, Value b);
void initValueArray(ValueArray* array);
void writeValueArray(struct VM* vm, ValueArray* array, Value value);
void freeValueArray(struct VM* vm, ValueArray* array);
void printValue(Value value);

#endif