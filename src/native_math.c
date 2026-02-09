// ============================================================================
// native_math.c - BTL Math Module
//
// Provides mathematical functions and constants as a native module.
// Includes trigonometry, logarithms, rounding, and utility functions.
// ============================================================================

#include <math.h>
#include "native_math.h"
#include "object.h"

// ----------------------------------------------------------------------------
// Basic Math Functions
// ----------------------------------------------------------------------------

// abs(x) - returns absolute value
static BtlValue mathAbs(VM* vm, int argCount, BtlValue* args) {
    (void) vm; (void) argCount;
    return NUMBER_VAL(fabs(btl_numeric_to_double(args[0])));
}

// floor(x) - rounds down to nearest integer
static BtlValue mathFloor(VM* vm, int argCount, BtlValue* args) {
    (void) vm; (void) argCount;
    return NUMBER_VAL(floor(btl_numeric_to_double(args[0])));
}

// ceil(x) - rounds up to nearest integer
static BtlValue mathCeil(VM* vm, int argCount, BtlValue* args) {
    (void) vm; (void) argCount;
    return NUMBER_VAL(ceil(btl_numeric_to_double(args[0])));
}

// round(x) - rounds to nearest integer
static BtlValue mathRound(VM* vm, int argCount, BtlValue* args) {
    (void) vm; (void) argCount;
    return NUMBER_VAL(round(btl_numeric_to_double(args[0])));
}

// trunc(x) - truncates toward zero
static BtlValue mathTrunc(VM* vm, int argCount, BtlValue* args) {
    (void) vm; (void) argCount;
    return NUMBER_VAL(trunc(btl_numeric_to_double(args[0])));
}

// sqrt(x) - returns square root
static BtlValue mathSqrt(VM* vm, int argCount, BtlValue* args) {
    (void) vm; (void) argCount;
    return NUMBER_VAL(sqrt(btl_numeric_to_double(args[0])));
}

// pow(base, exp) - returns base raised to exp
static BtlValue mathPow(VM* vm, int argCount, BtlValue* args) {
    (void) vm; (void) argCount;
    return NUMBER_VAL(pow(btl_numeric_to_double(args[0]), btl_numeric_to_double(args[1])));
}

// sin(x) - returns sine (x in radians)
static BtlValue mathSin(VM* vm, int argCount, BtlValue* args) {
    (void) vm; (void) argCount;
    return NUMBER_VAL(sin(btl_numeric_to_double(args[0])));
}

// cos(x) - returns cosine (x in radians)
static BtlValue mathCos(VM* vm, int argCount, BtlValue* args) {
    (void) vm; (void) argCount;
    return NUMBER_VAL(cos(btl_numeric_to_double(args[0])));
}

// tan(x) - returns tangent (x in radians)
static BtlValue mathTan(VM* vm, int argCount, BtlValue* args) {
    (void) vm; (void) argCount;
    return NUMBER_VAL(tan(btl_numeric_to_double(args[0])));
}

// asin(x) - returns arc sine (result in radians)
static BtlValue mathAsin(VM* vm, int argCount, BtlValue* args) {
    (void) vm; (void) argCount;
    return NUMBER_VAL(asin(btl_numeric_to_double(args[0])));
}

// acos(x) - returns arc cosine (result in radians)
static BtlValue mathAcos(VM* vm, int argCount, BtlValue* args) {
    (void) vm; (void) argCount;
    return NUMBER_VAL(acos(btl_numeric_to_double(args[0])));
}

// atan(x) - returns arc tangent (result in radians)
static BtlValue mathAtan(VM* vm, int argCount, BtlValue* args) {
    (void) vm; (void) argCount;
    return NUMBER_VAL(atan(btl_numeric_to_double(args[0])));
}

// atan2(y, x) - returns arc tangent of y/x (result in radians)
static BtlValue mathAtan2(VM* vm, int argCount, BtlValue* args) {
    (void) vm; (void) argCount;
    return NUMBER_VAL(atan2(btl_numeric_to_double(args[0]), btl_numeric_to_double(args[1])));
}

// log(x) - returns natural logarithm
static BtlValue mathLog(VM* vm, int argCount, BtlValue* args) {
    (void) vm; (void) argCount;
    return NUMBER_VAL(log(btl_numeric_to_double(args[0])));
}

// log10(x) - returns base-10 logarithm
static BtlValue mathLog10(VM* vm, int argCount, BtlValue* args) {
    (void) vm; (void) argCount;
    return NUMBER_VAL(log10(btl_numeric_to_double(args[0])));
}

// log2(x) - returns base-2 logarithm
static BtlValue mathLog2(VM* vm, int argCount, BtlValue* args) {
    (void) vm; (void) argCount;
    return NUMBER_VAL(log2(btl_numeric_to_double(args[0])));
}

// exp(x) - returns e raised to x
static BtlValue mathExp(VM* vm, int argCount, BtlValue* args) {
    (void) vm; (void) argCount;
    return NUMBER_VAL(exp(btl_numeric_to_double(args[0])));
}

// sign(x) - returns -1, 0, or 1 based on sign
static BtlValue mathSign(VM* vm, int argCount, BtlValue* args) {
    (void) vm; (void) argCount;
    double n = btl_numeric_to_double(args[0]);
    if (n > 0) return NUMBER_VAL(1);
    if (n < 0) return NUMBER_VAL(-1);
    return NUMBER_VAL(0);
}

// min(a, b) - returns smaller value
static BtlValue mathMin(VM* vm, int argCount, BtlValue* args) {
    (void) vm; (void) argCount;
    double a = btl_numeric_to_double(args[0]);
    double b = btl_numeric_to_double(args[1]);
    return NUMBER_VAL(a < b ? a : b);
}

// max(a, b) - returns larger value
static BtlValue mathMax(VM* vm, int argCount, BtlValue* args) {
    (void) vm; (void) argCount;
    double a = btl_numeric_to_double(args[0]);
    double b = btl_numeric_to_double(args[1]);
    return NUMBER_VAL(a > b ? a : b);
}

// clamp(x, min, max) - constrains value to range
static BtlValue mathClamp(VM* vm, int argCount, BtlValue* args) {
    (void) vm; (void) argCount;
    double n = btl_numeric_to_double(args[0]);
    double minVal = btl_numeric_to_double(args[1]);
    double maxVal = btl_numeric_to_double(args[2]);
    if (n < minVal) return NUMBER_VAL(minVal);
    if (n > maxVal) return NUMBER_VAL(maxVal);
    return NUMBER_VAL(n);
}

// lerp(a, b, t) - linear interpolation
static BtlValue mathLerp(VM* vm, int argCount, BtlValue* args) {
    (void) vm; (void) argCount;
    double a = btl_numeric_to_double(args[0]);
    double b = btl_numeric_to_double(args[1]);
    double t = btl_numeric_to_double(args[2]);
    return NUMBER_VAL(a + t * (b - a));
}

// hypot(a, b) - returns sqrt(a^2 + b^2)
static BtlValue mathHypot(VM* vm, int argCount, BtlValue* args) {
    (void) vm; (void) argCount;
    return NUMBER_VAL(hypot(btl_numeric_to_double(args[0]), btl_numeric_to_double(args[1])));
}

// fmod(a, b) - returns floating-point remainder
static BtlValue mathFmod(VM* vm, int argCount, BtlValue* args) {
    (void) vm; (void) argCount;
    return NUMBER_VAL(fmod(btl_numeric_to_double(args[0]), btl_numeric_to_double(args[1])));
}

// deg(rad) - converts radians to degrees
static BtlValue mathDeg(VM* vm, int argCount, BtlValue* args) {
    (void) vm; (void) argCount;
    return NUMBER_VAL(btl_numeric_to_double(args[0]) * 180.0 / 3.14159265358979323846);
}

// rad(deg) - converts degrees to radians
static BtlValue mathRad(VM* vm, int argCount, BtlValue* args) {
    (void) vm; (void) argCount;
    return NUMBER_VAL(btl_numeric_to_double(args[0]) * 3.14159265358979323846 / 180.0);
}

// ----------------------------------------------------------------------------
// Math Module Initialization
// ----------------------------------------------------------------------------

void btl_math_module_init(VM* vm) {
    ObjNativeModule* module = btl_native_module_new(vm, "math");
    btl_push(vm, OBJ_VAL(module));

    // Mathematical constants
    btl_native_module_add_value(vm, module, "PI", NUMBER_VAL(3.14159265358979323846));
    btl_native_module_add_value(vm, module, "E", NUMBER_VAL(2.71828182845904523536));
    btl_native_module_add_value(vm, module, "TAU", NUMBER_VAL(6.28318530717958647692));
    btl_native_module_add_value(vm, module, "INF", NUMBER_VAL(INFINITY));
    btl_native_module_add_value(vm, module, "NAN", NUMBER_VAL(NAN));

    // Mathematical functions
    btl_native_module_add_function(vm, module, "abs", mathAbs, 1);
    btl_native_module_add_function(vm, module, "floor", mathFloor, 1);
    btl_native_module_add_function(vm, module, "ceil", mathCeil, 1);
    btl_native_module_add_function(vm, module, "round", mathRound, 1);
    btl_native_module_add_function(vm, module, "trunc", mathTrunc, 1);
    btl_native_module_add_function(vm, module, "sqrt", mathSqrt, 1);
    btl_native_module_add_function(vm, module, "pow", mathPow, 2);
    btl_native_module_add_function(vm, module, "sin", mathSin, 1);
    btl_native_module_add_function(vm, module, "cos", mathCos, 1);
    btl_native_module_add_function(vm, module, "tan", mathTan, 1);
    btl_native_module_add_function(vm, module, "asin", mathAsin, 1);
    btl_native_module_add_function(vm, module, "acos", mathAcos, 1);
    btl_native_module_add_function(vm, module, "atan", mathAtan, 1);
    btl_native_module_add_function(vm, module, "atan2", mathAtan2, 2);
    btl_native_module_add_function(vm, module, "log", mathLog, 1);
    btl_native_module_add_function(vm, module, "log10", mathLog10, 1);
    btl_native_module_add_function(vm, module, "log2", mathLog2, 1);
    btl_native_module_add_function(vm, module, "exp", mathExp, 1);
    btl_native_module_add_function(vm, module, "sign", mathSign, 1);
    btl_native_module_add_function(vm, module, "min", mathMin, 2);
    btl_native_module_add_function(vm, module, "max", mathMax, 2);
    btl_native_module_add_function(vm, module, "clamp", mathClamp, 3);
    btl_native_module_add_function(vm, module, "lerp", mathLerp, 3);
    btl_native_module_add_function(vm, module, "hypot", mathHypot, 2);
    btl_native_module_add_function(vm, module, "fmod", mathFmod, 2);
    btl_native_module_add_function(vm, module, "deg", mathDeg, 1);
    btl_native_module_add_function(vm, module, "rad", mathRad, 1);

    btl_table_set(vm, &vm->nativeModules, OBJ_VAL(module->name), OBJ_VAL(module));
    btl_pop(vm);
}
