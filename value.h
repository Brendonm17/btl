#ifndef btl_value_h
#define btl_value_h

#include <string.h>
#include "common.h"

struct Obj;
struct VM;

#ifdef NAN_BOXING

#define SIGN_BIT ((uint64_t)0x8000000000000000)
#define QNAN     ((uint64_t)0x7ffc000000000000)

#define TAG_NIL   1
#define TAG_FALSE 2
#define TAG_TRUE  3
#define TAG_EMPTY 4 

typedef uint64_t Value;

#define IS_BOOL(v)      (((v) | 1) == TRUE_VAL)
#define IS_NIL(v)       ((v) == NIL_VAL)
#define IS_EMPTY(v)     ((v) == EMPTY_VAL)
#define IS_NUMBER(v)    (((v) & QNAN) != QNAN)
#define IS_OBJ(v)       (((v) & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT))

#define AS_BOOL(v)      ((v) == TRUE_VAL)
#define AS_NUMBER(v)    valueToNum(v)
#define AS_OBJ(v)       ((struct Obj*)(uintptr_t)((v) & ~(SIGN_BIT | QNAN)))

#define BOOL_VAL(b)     ((b) ? TRUE_VAL : FALSE_VAL)
#define FALSE_VAL       ((Value)(uint64_t)(QNAN | TAG_FALSE))
#define TRUE_VAL        ((Value)(uint64_t)(QNAN | TAG_TRUE))
#define NIL_VAL         ((Value)(uint64_t)(QNAN | TAG_NIL))
#define EMPTY_VAL       ((Value)(uint64_t)(QNAN | TAG_EMPTY))
#define NUMBER_VAL(num) numToValue(num)
#define OBJ_VAL(obj)    (Value)(SIGN_BIT | QNAN | (uint64_t)(uintptr_t)(obj))

static inline double valueToNum(Value v) {
    double num; memcpy(&num, &v, sizeof(Value)); return num;
}
static inline Value numToValue(double num) {
    Value v; memcpy(&v, &num, sizeof(double)); return v;
}

#else

typedef enum {
    VAL_BOOL, VAL_NIL, VAL_NUMBER, VAL_OBJ, VAL_EMPTY
} ValueType;
typedef struct {
    ValueType type; union {
        bool boolean; double number; struct Obj* obj;
    } as;
} Value;
#define IS_BOOL(v)    ((v).type == VAL_BOOL)
#define IS_NIL(v)     ((v).type == VAL_NIL)
#define IS_EMPTY(v)   ((v).type == VAL_EMPTY)
#define IS_NUMBER(v)  ((v).type == VAL_NUMBER)
#define IS_OBJ(v)     ((v).type == VAL_OBJ)
#define AS_BOOL(v)    ((v).as.boolean)
#define AS_NUMBER(v)  ((v).as.number)
#define AS_OBJ(v)     ((v).as.obj)
#define BOOL_VAL(v)   ((Value){VAL_BOOL, {.boolean = v}})
#define NIL_VAL       ((Value){VAL_NIL, {.number = 0}})
#define EMPTY_VAL     ((Value){VAL_EMPTY, {.number = 0}})
#define NUMBER_VAL(v) ((Value){VAL_NUMBER, {.number = v}})
#define OBJ_VAL(v)    ((Value){VAL_OBJ, {.obj = (struct Obj*)v}})

#endif

typedef struct {
    int capacity; int count; Value* values;
} ValueArray;

bool valuesEqual(Value a, Value b);
void initValueArray(ValueArray* array);
void writeValueArray(struct VM* vm, ValueArray* array, Value value);
void freeValueArray(struct VM* vm, ValueArray* array);
void printValue(Value value);
void printValueStderr(Value value);

#endif