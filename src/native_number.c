#include <stdio.h>
#include <math.h>
#include "native_number.h"
#include "object.h"
#include "memory.h"

static Value numberAbs(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) argCount; (void) args;
    return NUMBER_VAL(fabs(AS_NUMBER(receiver)));
}

static Value numberFloor(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) argCount; (void) args;
    return NUMBER_VAL(floor(AS_NUMBER(receiver)));
}

static Value numberCeil(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) argCount; (void) args;
    return NUMBER_VAL(ceil(AS_NUMBER(receiver)));
}

static Value numberRound(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) argCount; (void) args;
    return NUMBER_VAL(round(AS_NUMBER(receiver)));
}

static Value numberTrunc(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) argCount; (void) args;
    return NUMBER_VAL(trunc(AS_NUMBER(receiver)));
}

static Value numberSqrt(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) argCount; (void) args;
    return NUMBER_VAL(sqrt(AS_NUMBER(receiver)));
}

static Value numberPow(VM* vm, Value receiver, int argCount, Value* args) {
    (void) argCount;
    if (!IS_NUMBER(args[0])) {
        runtimeError(vm, "Exponent must be a number.");
        return NULL_VAL;
    }
    return NUMBER_VAL(pow(AS_NUMBER(receiver), AS_NUMBER(args[0])));
}

static Value numberSin(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) argCount; (void) args;
    return NUMBER_VAL(sin(AS_NUMBER(receiver)));
}

static Value numberCos(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) argCount; (void) args;
    return NUMBER_VAL(cos(AS_NUMBER(receiver)));
}

static Value numberTan(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) argCount; (void) args;
    return NUMBER_VAL(tan(AS_NUMBER(receiver)));
}

static Value numberAsin(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) argCount; (void) args;
    return NUMBER_VAL(asin(AS_NUMBER(receiver)));
}

static Value numberAcos(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) argCount; (void) args;
    return NUMBER_VAL(acos(AS_NUMBER(receiver)));
}

static Value numberAtan(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) argCount; (void) args;
    return NUMBER_VAL(atan(AS_NUMBER(receiver)));
}

static Value numberLog(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) argCount; (void) args;
    return NUMBER_VAL(log(AS_NUMBER(receiver)));
}

static Value numberLog10(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) argCount; (void) args;
    return NUMBER_VAL(log10(AS_NUMBER(receiver)));
}

static Value numberLog2(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) argCount; (void) args;
    return NUMBER_VAL(log2(AS_NUMBER(receiver)));
}

static Value numberExp(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) argCount; (void) args;
    return NUMBER_VAL(exp(AS_NUMBER(receiver)));
}

static Value numberSign(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) argCount; (void) args;
    double n = AS_NUMBER(receiver);
    if (n > 0) return NUMBER_VAL(1);
    if (n < 0) return NUMBER_VAL(-1);
    return NUMBER_VAL(0);
}

static Value numberIsNan(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) argCount; (void) args;
    return BOOL_VAL(isnan(AS_NUMBER(receiver)));
}

static Value numberIsInf(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) argCount; (void) args;
    return BOOL_VAL(isinf(AS_NUMBER(receiver)));
}

static Value numberIsFinite(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) argCount; (void) args;
    return BOOL_VAL(isfinite(AS_NUMBER(receiver)));
}

static Value numberIsInt(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) argCount; (void) args;
    double n = AS_NUMBER(receiver);
    return BOOL_VAL(isfinite(n) && n == floor(n));
}

static Value numberToInt(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) argCount; (void) args;
    return NUMBER_VAL((double) (long long) AS_NUMBER(receiver));
}

static Value numberToString(VM* vm, Value receiver, int argCount, Value* args) {
    (void) argCount; (void) args;
    char buffer[32];
    int len = snprintf(buffer, sizeof(buffer), "%g", AS_NUMBER(receiver));
    return OBJ_VAL(copyString(vm, buffer, len));
}

static Value numberToFixed(VM* vm, Value receiver, int argCount, Value* args) {
    (void) argCount;
    if (!IS_NUMBER(args[0])) {
        runtimeError(vm, "Precision must be a number.");
        return NULL_VAL;
    }
    int precision = (int) AS_NUMBER(args[0]);
    if (precision < 0) precision = 0;
    if (precision > 20) precision = 20;
    char buffer[64];
    int len = snprintf(buffer, sizeof(buffer), "%.*f", precision, AS_NUMBER(receiver));
    return OBJ_VAL(copyString(vm, buffer, len));
}

static Value numberToHex(VM* vm, Value receiver, int argCount, Value* args) {
    (void) argCount; (void) args;
    char buffer[32];
    long long n = (long long) AS_NUMBER(receiver);
    int len = snprintf(buffer, sizeof(buffer), "0x%llx", n);
    return OBJ_VAL(copyString(vm, buffer, len));
}

static Value numberToBinary(VM* vm, Value receiver, int argCount, Value* args) {
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
    return OBJ_VAL(copyString(vm, buffer, pos));
}

static Value numberChr(VM* vm, Value receiver, int argCount, Value* args) {
    (void) argCount; (void) args;
    int n = (int) AS_NUMBER(receiver);
    if (n < 0 || n > 255) {
        runtimeError(vm, "Character code must be 0-255.");
        return NULL_VAL;
    }
    char c = (char) n;
    return OBJ_VAL(copyString(vm, &c, 1));
}

static Value numberClamp(VM* vm, Value receiver, int argCount, Value* args) {
    (void) argCount;
    if (!IS_NUMBER(args[0]) || !IS_NUMBER(args[1])) {
        runtimeError(vm, "Arguments must be numbers.");
        return NULL_VAL;
    }
    double n = AS_NUMBER(receiver);
    double minVal = AS_NUMBER(args[0]);
    double maxVal = AS_NUMBER(args[1]);
    if (n < minVal) return NUMBER_VAL(minVal);
    if (n > maxVal) return NUMBER_VAL(maxVal);
    return NUMBER_VAL(n);
}

static Value numberLerp(VM* vm, Value receiver, int argCount, Value* args) {
    (void) argCount;
    if (!IS_NUMBER(args[0]) || !IS_NUMBER(args[1])) {
        runtimeError(vm, "Arguments must be numbers.");
        return NULL_VAL;
    }
    double t = AS_NUMBER(receiver);
    double a = AS_NUMBER(args[0]);
    double b = AS_NUMBER(args[1]);
    return NUMBER_VAL(a + t * (b - a));
}

static Value numberMod(VM* vm, Value receiver, int argCount, Value* args) {
    (void) argCount;
    if (!IS_NUMBER(args[0])) {
        runtimeError(vm, "Argument must be a number.");
        return NULL_VAL;
    }
    double a = AS_NUMBER(receiver);
    double b = AS_NUMBER(args[0]);
    double result = fmod(a, b);
    if (result < 0) result += fabs(b);
    return NUMBER_VAL(result);
}

static Value numberBetween(VM* vm, Value receiver, int argCount, Value* args) {
    (void) argCount;
    if (!IS_NUMBER(args[0]) || !IS_NUMBER(args[1])) {
        runtimeError(vm, "Arguments must be numbers.");
        return NULL_VAL;
    }
    double n = AS_NUMBER(receiver);
    double minVal = AS_NUMBER(args[0]);
    double maxVal = AS_NUMBER(args[1]);
    return BOOL_VAL(n >= minVal && n <= maxVal);
}

void initNumberClass(VM* vm) {
    vm->numberClass = newNativeClass(vm, "Number");
    defineNativeMethod(vm, vm->numberClass, "abs", numberAbs, 0);
    defineNativeMethod(vm, vm->numberClass, "floor", numberFloor, 0);
    defineNativeMethod(vm, vm->numberClass, "ceil", numberCeil, 0);
    defineNativeMethod(vm, vm->numberClass, "round", numberRound, 0);
    defineNativeMethod(vm, vm->numberClass, "trunc", numberTrunc, 0);
    defineNativeMethod(vm, vm->numberClass, "sqrt", numberSqrt, 0);
    defineNativeMethod(vm, vm->numberClass, "pow", numberPow, 1);
    defineNativeMethod(vm, vm->numberClass, "sin", numberSin, 0);
    defineNativeMethod(vm, vm->numberClass, "cos", numberCos, 0);
    defineNativeMethod(vm, vm->numberClass, "tan", numberTan, 0);
    defineNativeMethod(vm, vm->numberClass, "asin", numberAsin, 0);
    defineNativeMethod(vm, vm->numberClass, "acos", numberAcos, 0);
    defineNativeMethod(vm, vm->numberClass, "atan", numberAtan, 0);
    defineNativeMethod(vm, vm->numberClass, "log", numberLog, 0);
    defineNativeMethod(vm, vm->numberClass, "log10", numberLog10, 0);
    defineNativeMethod(vm, vm->numberClass, "log2", numberLog2, 0);
    defineNativeMethod(vm, vm->numberClass, "exp", numberExp, 0);
    defineNativeMethod(vm, vm->numberClass, "sign", numberSign, 0);
    defineNativeMethod(vm, vm->numberClass, "isNan", numberIsNan, 0);
    defineNativeMethod(vm, vm->numberClass, "isInf", numberIsInf, 0);
    defineNativeMethod(vm, vm->numberClass, "isFinite", numberIsFinite, 0);
    defineNativeMethod(vm, vm->numberClass, "isInt", numberIsInt, 0);
    defineNativeMethod(vm, vm->numberClass, "toInt", numberToInt, 0);
    defineNativeMethod(vm, vm->numberClass, "toString", numberToString, 0);
    defineNativeMethod(vm, vm->numberClass, "toFixed", numberToFixed, 1);
    defineNativeMethod(vm, vm->numberClass, "toHex", numberToHex, 0);
    defineNativeMethod(vm, vm->numberClass, "toBinary", numberToBinary, 0);
    defineNativeMethod(vm, vm->numberClass, "chr", numberChr, 0);
    defineNativeMethod(vm, vm->numberClass, "clamp", numberClamp, 2);
    defineNativeMethod(vm, vm->numberClass, "lerp", numberLerp, 2);
    defineNativeMethod(vm, vm->numberClass, "mod", numberMod, 1);
    defineNativeMethod(vm, vm->numberClass, "between", numberBetween, 2);
}