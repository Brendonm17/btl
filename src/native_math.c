#include <math.h>
#include "native_math.h"
#include "object.h"

static Value mathAbs(int argCount, Value* args) {
    (void) argCount;
    return NUMBER_VAL(fabs(AS_NUMBER(args[0])));
}

static Value mathFloor(int argCount, Value* args) {
    (void) argCount;
    return NUMBER_VAL(floor(AS_NUMBER(args[0])));
}

static Value mathCeil(int argCount, Value* args) {
    (void) argCount;
    return NUMBER_VAL(ceil(AS_NUMBER(args[0])));
}

static Value mathRound(int argCount, Value* args) {
    (void) argCount;
    return NUMBER_VAL(round(AS_NUMBER(args[0])));
}

static Value mathTrunc(int argCount, Value* args) {
    (void) argCount;
    return NUMBER_VAL(trunc(AS_NUMBER(args[0])));
}

static Value mathSqrt(int argCount, Value* args) {
    (void) argCount;
    return NUMBER_VAL(sqrt(AS_NUMBER(args[0])));
}

static Value mathPow(int argCount, Value* args) {
    (void) argCount;
    return NUMBER_VAL(pow(AS_NUMBER(args[0]), AS_NUMBER(args[1])));
}

static Value mathSin(int argCount, Value* args) {
    (void) argCount;
    return NUMBER_VAL(sin(AS_NUMBER(args[0])));
}

static Value mathCos(int argCount, Value* args) {
    (void) argCount;
    return NUMBER_VAL(cos(AS_NUMBER(args[0])));
}

static Value mathTan(int argCount, Value* args) {
    (void) argCount;
    return NUMBER_VAL(tan(AS_NUMBER(args[0])));
}

static Value mathAsin(int argCount, Value* args) {
    (void) argCount;
    return NUMBER_VAL(asin(AS_NUMBER(args[0])));
}

static Value mathAcos(int argCount, Value* args) {
    (void) argCount;
    return NUMBER_VAL(acos(AS_NUMBER(args[0])));
}

static Value mathAtan(int argCount, Value* args) {
    (void) argCount;
    return NUMBER_VAL(atan(AS_NUMBER(args[0])));
}

static Value mathAtan2(int argCount, Value* args) {
    (void) argCount;
    return NUMBER_VAL(atan2(AS_NUMBER(args[0]), AS_NUMBER(args[1])));
}

static Value mathLog(int argCount, Value* args) {
    (void) argCount;
    return NUMBER_VAL(log(AS_NUMBER(args[0])));
}

static Value mathLog10(int argCount, Value* args) {
    (void) argCount;
    return NUMBER_VAL(log10(AS_NUMBER(args[0])));
}

static Value mathLog2(int argCount, Value* args) {
    (void) argCount;
    return NUMBER_VAL(log2(AS_NUMBER(args[0])));
}

static Value mathExp(int argCount, Value* args) {
    (void) argCount;
    return NUMBER_VAL(exp(AS_NUMBER(args[0])));
}

static Value mathSign(int argCount, Value* args) {
    (void) argCount;
    double n = AS_NUMBER(args[0]);
    if (n > 0) return NUMBER_VAL(1);
    if (n < 0) return NUMBER_VAL(-1);
    return NUMBER_VAL(0);
}

static Value mathMin(int argCount, Value* args) {
    (void) argCount;
    double a = AS_NUMBER(args[0]);
    double b = AS_NUMBER(args[1]);
    return NUMBER_VAL(a < b ? a : b);
}

static Value mathMax(int argCount, Value* args) {
    (void) argCount;
    double a = AS_NUMBER(args[0]);
    double b = AS_NUMBER(args[1]);
    return NUMBER_VAL(a > b ? a : b);
}

static Value mathClamp(int argCount, Value* args) {
    (void) argCount;
    double n = AS_NUMBER(args[0]);
    double minVal = AS_NUMBER(args[1]);
    double maxVal = AS_NUMBER(args[2]);
    if (n < minVal) return NUMBER_VAL(minVal);
    if (n > maxVal) return NUMBER_VAL(maxVal);
    return NUMBER_VAL(n);
}

static Value mathLerp(int argCount, Value* args) {
    (void) argCount;
    double a = AS_NUMBER(args[0]);
    double b = AS_NUMBER(args[1]);
    double t = AS_NUMBER(args[2]);
    return NUMBER_VAL(a + t * (b - a));
}

static Value mathHypot(int argCount, Value* args) {
    (void) argCount;
    return NUMBER_VAL(hypot(AS_NUMBER(args[0]), AS_NUMBER(args[1])));
}

static Value mathFmod(int argCount, Value* args) {
    (void) argCount;
    return NUMBER_VAL(fmod(AS_NUMBER(args[0]), AS_NUMBER(args[1])));
}

static Value mathDeg(int argCount, Value* args) {
    (void) argCount;
    return NUMBER_VAL(AS_NUMBER(args[0]) * 180.0 / 3.14159265358979323846);
}

static Value mathRad(int argCount, Value* args) {
    (void) argCount;
    return NUMBER_VAL(AS_NUMBER(args[0]) * 3.14159265358979323846 / 180.0);
}

void initMathModule(VM* vm) {
    ObjNativeModule* module = newNativeModule(vm, "math");
    push(vm, OBJ_VAL(module));

    // Constants
    defineNativeModuleValue(vm, module, "PI", NUMBER_VAL(3.14159265358979323846));
    defineNativeModuleValue(vm, module, "E", NUMBER_VAL(2.71828182845904523536));
    defineNativeModuleValue(vm, module, "TAU", NUMBER_VAL(6.28318530717958647692));
    defineNativeModuleValue(vm, module, "INF", NUMBER_VAL(INFINITY));
    defineNativeModuleValue(vm, module, "NAN", NUMBER_VAL(NAN));

    // Functions
    defineNativeModuleFn(vm, module, "abs", mathAbs, 1);
    defineNativeModuleFn(vm, module, "floor", mathFloor, 1);
    defineNativeModuleFn(vm, module, "ceil", mathCeil, 1);
    defineNativeModuleFn(vm, module, "round", mathRound, 1);
    defineNativeModuleFn(vm, module, "trunc", mathTrunc, 1);
    defineNativeModuleFn(vm, module, "sqrt", mathSqrt, 1);
    defineNativeModuleFn(vm, module, "pow", mathPow, 2);
    defineNativeModuleFn(vm, module, "sin", mathSin, 1);
    defineNativeModuleFn(vm, module, "cos", mathCos, 1);
    defineNativeModuleFn(vm, module, "tan", mathTan, 1);
    defineNativeModuleFn(vm, module, "asin", mathAsin, 1);
    defineNativeModuleFn(vm, module, "acos", mathAcos, 1);
    defineNativeModuleFn(vm, module, "atan", mathAtan, 1);
    defineNativeModuleFn(vm, module, "atan2", mathAtan2, 2);
    defineNativeModuleFn(vm, module, "log", mathLog, 1);
    defineNativeModuleFn(vm, module, "log10", mathLog10, 1);
    defineNativeModuleFn(vm, module, "log2", mathLog2, 1);
    defineNativeModuleFn(vm, module, "exp", mathExp, 1);
    defineNativeModuleFn(vm, module, "sign", mathSign, 1);
    defineNativeModuleFn(vm, module, "min", mathMin, 2);
    defineNativeModuleFn(vm, module, "max", mathMax, 2);
    defineNativeModuleFn(vm, module, "clamp", mathClamp, 3);
    defineNativeModuleFn(vm, module, "lerp", mathLerp, 3);
    defineNativeModuleFn(vm, module, "hypot", mathHypot, 2);
    defineNativeModuleFn(vm, module, "fmod", mathFmod, 2);
    defineNativeModuleFn(vm, module, "deg", mathDeg, 1);
    defineNativeModuleFn(vm, module, "rad", mathRad, 1);

    tableSet(vm, &vm->nativeModules, OBJ_VAL(module->name), OBJ_VAL(module));
    pop(vm);
}