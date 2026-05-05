#include <stdio.h>
#include <math.h>
#include <inttypes.h>
#include "native_int.h"
#include "object.h"
#include "memory.h"

static BtlValue intAbs(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) vm; (void) argCount; (void) args;
    int64_t n = AS_INT(receiver);
    return INT_VAL(n < 0 ? -n : n);
}

static BtlValue intSign(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) vm; (void) argCount; (void) args;
    int64_t n = AS_INT(receiver);
    if (n > 0) return INT_VAL(1);
    if (n < 0) return INT_VAL(-1);
    return INT_VAL(0);
}

static BtlValue intPow(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount;
    if (!IS_NUMERIC(args[0])) {
        btl_runtime_error(vm, "Exponent must be a number.");
        return BTL_NULL_VAL;
    }
    if (IS_INT(args[0])) {
        int64_t base = AS_INT(receiver);
        int64_t exp = AS_INT(args[0]);
        if (exp < 0) {
            return NUMBER_VAL(pow((double)base, (double)exp));
        }
        int64_t result = 1;
        for (int64_t i = 0; i < exp; i++) result *= base;
        return INT_VAL(result);
    }
    return NUMBER_VAL(pow((double)AS_INT(receiver), btl_numeric_to_double(args[0])));
}

static BtlValue intClamp(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount;
    if (!IS_NUMERIC(args[0]) || !IS_NUMERIC(args[1])) {
        btl_runtime_error(vm, "Arguments must be numbers.");
        return BTL_NULL_VAL;
    }
    int64_t n = AS_INT(receiver);
    if (IS_INT(args[0]) && IS_INT(args[1])) {
        int64_t minVal = AS_INT(args[0]);
        int64_t maxVal = AS_INT(args[1]);
        if (n < minVal) return INT_VAL(minVal);
        if (n > maxVal) return INT_VAL(maxVal);
        return INT_VAL(n);
    }
    double nd = (double)n;
    double minVal = btl_numeric_to_double(args[0]);
    double maxVal = btl_numeric_to_double(args[1]);
    if (nd < minVal) return NUMBER_VAL(minVal);
    if (nd > maxVal) return NUMBER_VAL(maxVal);
    return INT_VAL(n);
}

static BtlValue intMod(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount;
    if (!IS_NUMERIC(args[0])) {
        btl_runtime_error(vm, "Argument must be a number.");
        return BTL_NULL_VAL;
    }
    if (IS_INT(args[0])) {
        int64_t a = AS_INT(receiver);
        int64_t b = AS_INT(args[0]);
        if (b == 0) { btl_runtime_error(vm, "Division by zero."); return BTL_NULL_VAL; }
        int64_t result = a % b;
        if (result < 0) result += (b < 0 ? -b : b);
        return INT_VAL(result);
    }
    double a = (double)AS_INT(receiver);
    double b = btl_numeric_to_double(args[0]);
    double result = fmod(a, b);
    if (result < 0) result += fabs(b);
    return NUMBER_VAL(result);
}

static BtlValue intMin(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount;
    if (!IS_NUMERIC(args[0])) {
        btl_runtime_error(vm, "Argument must be a number.");
        return BTL_NULL_VAL;
    }
    int64_t a = AS_INT(receiver);
    if (IS_INT(args[0])) {
        int64_t b = AS_INT(args[0]);
        return INT_VAL(a < b ? a : b);
    }
    double ad = (double)a, bd = btl_numeric_to_double(args[0]);
    return NUMBER_VAL(ad < bd ? ad : bd);
}

static BtlValue intMax(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount;
    if (!IS_NUMERIC(args[0])) {
        btl_runtime_error(vm, "Argument must be a number.");
        return BTL_NULL_VAL;
    }
    int64_t a = AS_INT(receiver);
    if (IS_INT(args[0])) {
        int64_t b = AS_INT(args[0]);
        return INT_VAL(a > b ? a : b);
    }
    double ad = (double)a, bd = btl_numeric_to_double(args[0]);
    return NUMBER_VAL(ad > bd ? ad : bd);
}

static BtlValue intGcd(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount;
    if (!IS_INT(args[0])) {
        btl_runtime_error(vm, "Argument must be an integer.");
        return BTL_NULL_VAL;
    }
    int64_t a = AS_INT(receiver);
    int64_t b = AS_INT(args[0]);
    if (a < 0) a = -a;
    if (b < 0) b = -b;
    while (b != 0) {
        int64_t t = b;
        b = a % b;
        a = t;
    }
    return INT_VAL(a);
}

// ----------------------------------------------------------------------------
// Predicate Methods
// ----------------------------------------------------------------------------

static BtlValue intIsEven(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) vm; (void) argCount; (void) args;
    return BOOL_VAL(AS_INT(receiver) % 2 == 0);
}

static BtlValue intIsOdd(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) vm; (void) argCount; (void) args;
    return BOOL_VAL(AS_INT(receiver) % 2 != 0);
}

static BtlValue intIsZero(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) vm; (void) argCount; (void) args;
    return BOOL_VAL(AS_INT(receiver) == 0);
}

static BtlValue intIsPositive(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) vm; (void) argCount; (void) args;
    return BOOL_VAL(AS_INT(receiver) > 0);
}

static BtlValue intIsNegative(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) vm; (void) argCount; (void) args;
    return BOOL_VAL(AS_INT(receiver) < 0);
}

static BtlValue intBetween(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount;
    if (!IS_NUMERIC(args[0]) || !IS_NUMERIC(args[1])) {
        btl_runtime_error(vm, "Arguments must be numbers.");
        return BTL_NULL_VAL;
    }
    if (IS_INT(args[0]) && IS_INT(args[1])) {
        int64_t n = AS_INT(receiver);
        return BOOL_VAL(n >= AS_INT(args[0]) && n <= AS_INT(args[1]));
    }
    double n = (double)AS_INT(receiver);
    return BOOL_VAL(n >= btl_numeric_to_double(args[0]) && n <= btl_numeric_to_double(args[1]));
}

// ----------------------------------------------------------------------------
// Bitwise Methods
// ----------------------------------------------------------------------------

static BtlValue intBitAnd(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount;
    if (!IS_INT(args[0])) {
        btl_runtime_error(vm, "Argument must be an integer.");
        return BTL_NULL_VAL;
    }
    return INT_VAL(AS_INT(receiver) & AS_INT(args[0]));
}

static BtlValue intBitOr(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount;
    if (!IS_INT(args[0])) {
        btl_runtime_error(vm, "Argument must be an integer.");
        return BTL_NULL_VAL;
    }
    return INT_VAL(AS_INT(receiver) | AS_INT(args[0]));
}

static BtlValue intBitXor(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount;
    if (!IS_INT(args[0])) {
        btl_runtime_error(vm, "Argument must be an integer.");
        return BTL_NULL_VAL;
    }
    return INT_VAL(AS_INT(receiver) ^ AS_INT(args[0]));
}

static BtlValue intBitNot(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) vm; (void) argCount; (void) args;
    return INT_VAL(~AS_INT(receiver));
}

static BtlValue intLeftShift(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount;
    if (!IS_INT(args[0])) {
        btl_runtime_error(vm, "Argument must be an integer.");
        return BTL_NULL_VAL;
    }
    int64_t shift = AS_INT(args[0]);
    if (shift < 0 || shift >= 48) {
        btl_runtime_error(vm, "Shift amount must be 0-47.");
        return BTL_NULL_VAL;
    }
    return INT_VAL(AS_INT(receiver) << shift);
}

static BtlValue intRightShift(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount;
    if (!IS_INT(args[0])) {
        btl_runtime_error(vm, "Argument must be an integer.");
        return BTL_NULL_VAL;
    }
    int64_t shift = AS_INT(args[0]);
    if (shift < 0 || shift >= 48) {
        btl_runtime_error(vm, "Shift amount must be 0-47.");
        return BTL_NULL_VAL;
    }
    return INT_VAL(AS_INT(receiver) >> shift);
}

// ----------------------------------------------------------------------------
// Conversion Methods
// ----------------------------------------------------------------------------

static BtlValue intToString(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount; (void) args;
    char buffer[32];
    int len = snprintf(buffer, sizeof(buffer), "%" PRId64, AS_INT(receiver));
    return OBJ_VAL(btl_string_copy(vm, buffer, len));
}

static BtlValue intToFloat(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) vm; (void) argCount; (void) args;
    return NUMBER_VAL((double)AS_INT(receiver));
}

static BtlValue intToHex(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount; (void) args;
    char buffer[32];
    int64_t n = AS_INT(receiver);
    int len = snprintf(buffer, sizeof(buffer), "0x%" PRIx64, (uint64_t)n);
    return OBJ_VAL(btl_string_copy(vm, buffer, len));
}

static BtlValue intToBinary(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount; (void) args;
    uint64_t n = (uint64_t)AS_INT(receiver);
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

static BtlValue intChr(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount; (void) args;
    int64_t n = AS_INT(receiver);
    if (n < 0 || n > 255) {
        btl_runtime_error(vm, "Character code must be 0-255.");
        return BTL_NULL_VAL;
    }
    char c = (char) n;
    return OBJ_VAL(btl_string_copy(vm, &c, 1));
}

// ----------------------------------------------------------------------------
// Utility Methods
// ----------------------------------------------------------------------------

static BtlValue intTimes(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount;
    int64_t n = AS_INT(receiver);
    if (n < 0) {
        btl_runtime_error(vm, "times() requires a non-negative integer.");
        return BTL_NULL_VAL;
    }
    BtlValue fn = args[0];
    for (int64_t i = 0; i < n; i++) {
        btl_push(vm, fn);
        btl_push(vm, INT_VAL(i));
        if (!btl_call_value(vm, fn, 1)) return BTL_NULL_VAL;
        if (btl_run(vm) != BTL_INTERPRET_OK) return BTL_NULL_VAL;
        btl_pop(vm); // discard return value
    }
    return BTL_NULL_VAL;
}

// ----------------------------------------------------------------------------
// Integer Class Initialization
// ----------------------------------------------------------------------------

void btl_int_class_init(VM* vm) {
    vm->intClass = btl_native_class_new(vm, "Int");

    // Math
    btl_native_class_add_method(vm, vm->intClass, "abs", intAbs, 0);
    btl_native_class_add_method(vm, vm->intClass, "sign", intSign, 0);
    btl_native_class_add_method(vm, vm->intClass, "pow", intPow, 1);
    btl_native_class_add_method(vm, vm->intClass, "clamp", intClamp, 2);
    btl_native_class_add_method(vm, vm->intClass, "mod", intMod, 1);
    btl_native_class_add_method(vm, vm->intClass, "min", intMin, 1);
    btl_native_class_add_method(vm, vm->intClass, "max", intMax, 1);
    btl_native_class_add_method(vm, vm->intClass, "gcd", intGcd, 1);

    // Predicates
    btl_native_class_add_method(vm, vm->intClass, "isEven", intIsEven, 0);
    btl_native_class_add_method(vm, vm->intClass, "isOdd", intIsOdd, 0);
    btl_native_class_add_method(vm, vm->intClass, "isZero", intIsZero, 0);
    btl_native_class_add_method(vm, vm->intClass, "isPositive", intIsPositive, 0);
    btl_native_class_add_method(vm, vm->intClass, "isNegative", intIsNegative, 0);
    btl_native_class_add_method(vm, vm->intClass, "between", intBetween, 2);

    // Bitwise
    btl_native_class_add_method(vm, vm->intClass, "bitAnd", intBitAnd, 1);
    btl_native_class_add_method(vm, vm->intClass, "bitOr", intBitOr, 1);
    btl_native_class_add_method(vm, vm->intClass, "bitXor", intBitXor, 1);
    btl_native_class_add_method(vm, vm->intClass, "bitNot", intBitNot, 0);
    btl_native_class_add_method(vm, vm->intClass, "leftShift", intLeftShift, 1);
    btl_native_class_add_method(vm, vm->intClass, "rightShift", intRightShift, 1);

    // Conversion
    btl_native_class_add_method(vm, vm->intClass, "toString", intToString, 0);
    btl_native_class_add_method(vm, vm->intClass, "toFloat", intToFloat, 0);
    btl_native_class_add_method(vm, vm->intClass, "toHex", intToHex, 0);
    btl_native_class_add_method(vm, vm->intClass, "toBinary", intToBinary, 0);
    btl_native_class_add_method(vm, vm->intClass, "chr", intChr, 0);

    // Utility
    btl_native_class_add_method(vm, vm->intClass, "times", intTimes, 1);
}
