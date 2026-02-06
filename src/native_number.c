// ============================================================================
// native_number.c - BTL Number Native Methods
//
// Implements built-in methods for number values including math operations,
// type checking, conversions, and formatting.
// ============================================================================

#include <stdio.h>
#include <math.h>
#include "native_number.h"
#include "object.h"
#include "memory.h"

// ----------------------------------------------------------------------------
// Math Methods
// ----------------------------------------------------------------------------

// abs() - returns absolute value
static BtlValue numberAbs(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) vm; (void) argCount; (void) args;
    return NUMBER_VAL(fabs(AS_NUMBER(receiver)));
}

// floor() - rounds down
static BtlValue numberFloor(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) vm; (void) argCount; (void) args;
    return NUMBER_VAL(floor(AS_NUMBER(receiver)));
}

// ceil() - rounds up
static BtlValue numberCeil(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) vm; (void) argCount; (void) args;
    return NUMBER_VAL(ceil(AS_NUMBER(receiver)));
}

// round() - rounds to nearest integer
static BtlValue numberRound(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) vm; (void) argCount; (void) args;
    return NUMBER_VAL(round(AS_NUMBER(receiver)));
}

// trunc() - truncates toward zero
static BtlValue numberTrunc(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) vm; (void) argCount; (void) args;
    return NUMBER_VAL(trunc(AS_NUMBER(receiver)));
}

// sqrt() - returns square root
static BtlValue numberSqrt(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) vm; (void) argCount; (void) args;
    return NUMBER_VAL(sqrt(AS_NUMBER(receiver)));
}

// pow(exp) - raises to power
static BtlValue numberPow(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount;
    if (!IS_NUMBER(args[0])) {
        btl_runtime_error(vm, "Exponent must be a number.");
        return BTL_NULL_VAL;
    }
    return NUMBER_VAL(pow(AS_NUMBER(receiver), AS_NUMBER(args[0])));
}

static BtlValue numberSin(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) vm; (void) argCount; (void) args;
    return NUMBER_VAL(sin(AS_NUMBER(receiver)));
}

static BtlValue numberCos(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) vm; (void) argCount; (void) args;
    return NUMBER_VAL(cos(AS_NUMBER(receiver)));
}

static BtlValue numberTan(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) vm; (void) argCount; (void) args;
    return NUMBER_VAL(tan(AS_NUMBER(receiver)));
}

static BtlValue numberAsin(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) vm; (void) argCount; (void) args;
    return NUMBER_VAL(asin(AS_NUMBER(receiver)));
}

static BtlValue numberAcos(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) vm; (void) argCount; (void) args;
    return NUMBER_VAL(acos(AS_NUMBER(receiver)));
}

static BtlValue numberAtan(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) vm; (void) argCount; (void) args;
    return NUMBER_VAL(atan(AS_NUMBER(receiver)));
}

static BtlValue numberLog(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) vm; (void) argCount; (void) args;
    return NUMBER_VAL(log(AS_NUMBER(receiver)));
}

static BtlValue numberLog10(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) vm; (void) argCount; (void) args;
    return NUMBER_VAL(log10(AS_NUMBER(receiver)));
}

static BtlValue numberLog2(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) vm; (void) argCount; (void) args;
    return NUMBER_VAL(log2(AS_NUMBER(receiver)));
}

static BtlValue numberExp(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) vm; (void) argCount; (void) args;
    return NUMBER_VAL(exp(AS_NUMBER(receiver)));
}

static BtlValue numberSign(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) vm; (void) argCount; (void) args;
    double n = AS_NUMBER(receiver);
    if (n > 0) return NUMBER_VAL(1);
    if (n < 0) return NUMBER_VAL(-1);
    return NUMBER_VAL(0);
}

static BtlValue numberIsNan(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) vm; (void) argCount; (void) args;
    return BOOL_VAL(isnan(AS_NUMBER(receiver)));
}

static BtlValue numberIsInf(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) vm; (void) argCount; (void) args;
    return BOOL_VAL(isinf(AS_NUMBER(receiver)));
}

static BtlValue numberIsFinite(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) vm; (void) argCount; (void) args;
    return BOOL_VAL(isfinite(AS_NUMBER(receiver)));
}

static BtlValue numberIsInt(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) vm; (void) argCount; (void) args;
    double n = AS_NUMBER(receiver);
    return BOOL_VAL(isfinite(n) && n == floor(n));
}

static BtlValue numberToInt(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) vm; (void) argCount; (void) args;
    return NUMBER_VAL((double) (long long) AS_NUMBER(receiver));
}

static BtlValue numberToString(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount; (void) args;
    char buffer[32];
    int len = snprintf(buffer, sizeof(buffer), "%g", AS_NUMBER(receiver));
    return OBJ_VAL(btl_string_copy(vm, buffer, len));
}

static BtlValue numberToFixed(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount;
    if (!IS_NUMBER(args[0])) {
        btl_runtime_error(vm, "Precision must be a number.");
        return BTL_NULL_VAL;
    }
    int precision = (int) AS_NUMBER(args[0]);
    if (precision < 0) precision = 0;
    if (precision > 20) precision = 20;
    char buffer[64];
    int len = snprintf(buffer, sizeof(buffer), "%.*f", precision, AS_NUMBER(receiver));
    return OBJ_VAL(btl_string_copy(vm, buffer, len));
}

static BtlValue numberToHex(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount; (void) args;
    char buffer[32];
    long long n = (long long) AS_NUMBER(receiver);
    int len = snprintf(buffer, sizeof(buffer), "0x%llx", n);
    return OBJ_VAL(btl_string_copy(vm, buffer, len));
}

static BtlValue numberToBinary(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount; (void) args;
    unsigned long long n = (unsigned long long)AS_NUMBER(receiver);
    char buffer[68];
    buffer[0] = '0';
    buffer[1] = 'b';
    int pos = 2;
    if (n == 0) {
        buffer[pos++] = '0';
    } else {
        int start = 63;
        while (start > 0 && !((n >> start) & 1)) start--;
        for (int i = start; i >= 0; i--) {
            buffer[pos++] = ((n >> i) & 1) ? '1' : '0';
        }
    }
    buffer[pos] = '\0';
    return OBJ_VAL(btl_string_copy(vm, buffer, pos));
}

static BtlValue numberChr(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount; (void) args;
    int n = (int) AS_NUMBER(receiver);
    if (n < 0 || n > 255) {
        btl_runtime_error(vm, "Character code must be 0-255.");
        return BTL_NULL_VAL;
    }
    char c = (char) n;
    return OBJ_VAL(btl_string_copy(vm, &c, 1));
}

static BtlValue numberClamp(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount;
    if (!IS_NUMBER(args[0]) || !IS_NUMBER(args[1])) {
        btl_runtime_error(vm, "Arguments must be numbers.");
        return BTL_NULL_VAL;
    }
    double n = AS_NUMBER(receiver);
    double minVal = AS_NUMBER(args[0]);
    double maxVal = AS_NUMBER(args[1]);
    if (n < minVal) return NUMBER_VAL(minVal);
    if (n > maxVal) return NUMBER_VAL(maxVal);
    return NUMBER_VAL(n);
}

static BtlValue numberLerp(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount;
    if (!IS_NUMBER(args[0]) || !IS_NUMBER(args[1])) {
        btl_runtime_error(vm, "Arguments must be numbers.");
        return BTL_NULL_VAL;
    }
    double t = AS_NUMBER(receiver);
    double a = AS_NUMBER(args[0]);
    double b = AS_NUMBER(args[1]);
    return NUMBER_VAL(a + t * (b - a));
}

static BtlValue numberMod(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount;
    if (!IS_NUMBER(args[0])) {
        btl_runtime_error(vm, "Argument must be a number.");
        return BTL_NULL_VAL;
    }
    double a = AS_NUMBER(receiver);
    double b = AS_NUMBER(args[0]);
    double result = fmod(a, b);
    if (result < 0) result += fabs(b);
    return NUMBER_VAL(result);
}

static BtlValue numberBetween(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount;
    if (!IS_NUMBER(args[0]) || !IS_NUMBER(args[1])) {
        btl_runtime_error(vm, "Arguments must be numbers.");
        return BTL_NULL_VAL;
    }
    double n = AS_NUMBER(receiver);
    double minVal = AS_NUMBER(args[0]);
    double maxVal = AS_NUMBER(args[1]);
    return BOOL_VAL(n >= minVal && n <= maxVal);
}

// ----------------------------------------------------------------------------
// Number Class Initialization
// ----------------------------------------------------------------------------

void btl_number_class_init(VM* vm) {
    vm->numberClass = btl_native_class_new(vm, "Number");
    btl_native_class_add_method(vm, vm->numberClass, "abs", numberAbs, 0);
    btl_native_class_add_method(vm, vm->numberClass, "floor", numberFloor, 0);
    btl_native_class_add_method(vm, vm->numberClass, "ceil", numberCeil, 0);
    btl_native_class_add_method(vm, vm->numberClass, "round", numberRound, 0);
    btl_native_class_add_method(vm, vm->numberClass, "trunc", numberTrunc, 0);
    btl_native_class_add_method(vm, vm->numberClass, "sqrt", numberSqrt, 0);
    btl_native_class_add_method(vm, vm->numberClass, "pow", numberPow, 1);
    btl_native_class_add_method(vm, vm->numberClass, "sin", numberSin, 0);
    btl_native_class_add_method(vm, vm->numberClass, "cos", numberCos, 0);
    btl_native_class_add_method(vm, vm->numberClass, "tan", numberTan, 0);
    btl_native_class_add_method(vm, vm->numberClass, "asin", numberAsin, 0);
    btl_native_class_add_method(vm, vm->numberClass, "acos", numberAcos, 0);
    btl_native_class_add_method(vm, vm->numberClass, "atan", numberAtan, 0);
    btl_native_class_add_method(vm, vm->numberClass, "log", numberLog, 0);
    btl_native_class_add_method(vm, vm->numberClass, "log10", numberLog10, 0);
    btl_native_class_add_method(vm, vm->numberClass, "log2", numberLog2, 0);
    btl_native_class_add_method(vm, vm->numberClass, "exp", numberExp, 0);
    btl_native_class_add_method(vm, vm->numberClass, "sign", numberSign, 0);
    btl_native_class_add_method(vm, vm->numberClass, "isNan", numberIsNan, 0);
    btl_native_class_add_method(vm, vm->numberClass, "isInf", numberIsInf, 0);
    btl_native_class_add_method(vm, vm->numberClass, "isFinite", numberIsFinite, 0);
    btl_native_class_add_method(vm, vm->numberClass, "isInt", numberIsInt, 0);
    btl_native_class_add_method(vm, vm->numberClass, "toInt", numberToInt, 0);
    btl_native_class_add_method(vm, vm->numberClass, "toString", numberToString, 0);
    btl_native_class_add_method(vm, vm->numberClass, "toFixed", numberToFixed, 1);
    btl_native_class_add_method(vm, vm->numberClass, "toHex", numberToHex, 0);
    btl_native_class_add_method(vm, vm->numberClass, "toBinary", numberToBinary, 0);
    btl_native_class_add_method(vm, vm->numberClass, "chr", numberChr, 0);
    btl_native_class_add_method(vm, vm->numberClass, "clamp", numberClamp, 2);
    btl_native_class_add_method(vm, vm->numberClass, "lerp", numberLerp, 2);
    btl_native_class_add_method(vm, vm->numberClass, "mod", numberMod, 1);
    btl_native_class_add_method(vm, vm->numberClass, "between", numberBetween, 2);
}