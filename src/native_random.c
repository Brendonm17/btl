// ============================================================================
// native_random.c - BTL Random Module
//
// Provides random number generation functions using xorshift64 PRNG.
// Includes basic random, integers, floats, list operations, and distributions.
// ============================================================================

#include <time.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "native_random.h"
#include "object.h"
#include "memory.h"

// ----------------------------------------------------------------------------
// Internal PRNG State
// ----------------------------------------------------------------------------

static uint64_t randState = 0;
static bool randSeeded = false;

// xorshift64 PRNG - fast, good statistical properties
static uint64_t xorshift64(void) {
    if (!randSeeded) {
        randState = (uint64_t) time(NULL);
        randSeeded = true;
    }
    uint64_t x = randState;
    x ^= x << 13;
    x ^= x >> 7;
    x ^= x << 17;
    randState = x;
    return x;
}

// Returns random double in [0, 1)
static double randomDouble(void) {
    return (double) xorshift64() / (double) UINT64_MAX;
}

// ----------------------------------------------------------------------------
// Random Module Functions
// ----------------------------------------------------------------------------

// random() - returns float in [0, 1)
static BtlValue randomRandom(int argCount, BtlValue* args) {
    (void) argCount; (void) args;
    return NUMBER_VAL(randomDouble());
}

// int(min, max) - returns integer in [min, max] inclusive
static BtlValue randomInt(int argCount, BtlValue* args) {
    (void) argCount;
    int min = (int) AS_NUMBER(args[0]);
    int max = (int) AS_NUMBER(args[1]);
    if (min > max) {
        int tmp = min;
        min = max;
        max = tmp;
    }
    uint64_t range = (uint64_t) (max - min + 1);
    return NUMBER_VAL((double) (min + (int) (xorshift64() % range)));
}

// float(min, max) - returns float in [min, max)
static BtlValue randomFloat(int argCount, BtlValue* args) {
    (void) argCount;
    double min = AS_NUMBER(args[0]);
    double max = AS_NUMBER(args[1]);
    return NUMBER_VAL(min + randomDouble() * (max - min));
}

// seed(n) - seed the RNG
static BtlValue randomSeed(int argCount, BtlValue* args) {
    (void) argCount;
    randState = (uint64_t) AS_NUMBER(args[0]);
    randSeeded = true;
    return BTL_NULL_VAL;
}

// bool() - returns true or false with 50% chance
static BtlValue randomBool(int argCount, BtlValue* args) {
    (void) argCount; (void) args;
    return BOOL_VAL(xorshift64() & 1);
}

// chance(p) - returns true with probability p (0.0 to 1.0)
static BtlValue randomChance(int argCount, BtlValue* args) {
    (void) argCount;
    double p = AS_NUMBER(args[0]);
    return BOOL_VAL(randomDouble() < p);
}

// choice(list) - returns random element from list
static BtlValue randomChoice(int argCount, BtlValue* args) {
    (void) argCount;
    if (!IS_LIST(args[0])) {
        return BTL_NULL_VAL;
    }
    ObjList* list = AS_LIST(args[0]);
    if (list->items.count == 0) {
        return BTL_NULL_VAL;
    }
    int index = (int) (xorshift64() % (uint64_t) list->items.count);
    return list->items.values[index];
}

// shuffle(list) - shuffles list in place using Fisher-Yates, returns it
static BtlValue randomShuffle(int argCount, BtlValue* args) {
    (void) argCount;
    if (!IS_LIST(args[0])) {
        return BTL_NULL_VAL;
    }
    ObjList* list = AS_LIST(args[0]);
    for (int i = list->items.count - 1; i > 0; i--) {
        int j = (int) (xorshift64() % (uint64_t) (i + 1));
        BtlValue temp = list->items.values[i];
        list->items.values[i] = list->items.values[j];
        list->items.values[j] = temp;
    }
    return args[0];
}

// normal(mean, stddev) - returns normally distributed random number (Box-Muller)
static BtlValue randomNormal(int argCount, BtlValue* args) {
    (void) argCount;
    double mean = AS_NUMBER(args[0]);
    double stddev = AS_NUMBER(args[1]);

    // Box-Muller transform
    double u1 = randomDouble();
    double u2 = randomDouble();
    if (u1 < 1e-10) u1 = 1e-10;

    double z = sqrt(-2.0 * log(u1)) * cos(2.0 * 3.14159265358979323846 * u2);
    return NUMBER_VAL(mean + z * stddev);
}

// dice(sides) - roll a die with n sides (1 to sides)
static BtlValue randomDice(int argCount, BtlValue* args) {
    (void) argCount;
    int sides = (int) AS_NUMBER(args[0]);
    if (sides < 1) sides = 1;
    return NUMBER_VAL((double) (1 + (int) (xorshift64() % (uint64_t) sides)));
}

// diceSum(count, sides) - roll 'count' dice with 'sides' sides, return sum
static BtlValue randomDiceSum(int argCount, BtlValue* args) {
    (void) argCount;
    int count = (int) AS_NUMBER(args[0]);
    int sides = (int) AS_NUMBER(args[1]);
    if (count < 1) count = 1;
    if (sides < 1) sides = 1;
    int sum = 0;
    for (int i = 0; i < count; i++) {
        sum += 1 + (int) (xorshift64() % (uint64_t) sides);
    }
    return NUMBER_VAL((double) sum);
}

// ----------------------------------------------------------------------------
// Random Module Initialization
// ----------------------------------------------------------------------------

void btl_random_module_init(VM* vm) {
    ObjNativeModule* module = btl_native_module_new(vm, "random");
    btl_push(vm, OBJ_VAL(module));

    btl_native_module_add_function(vm, module, "random", randomRandom, 0);
    btl_native_module_add_function(vm, module, "int", randomInt, 2);
    btl_native_module_add_function(vm, module, "float", randomFloat, 2);
    btl_native_module_add_function(vm, module, "seed", randomSeed, 1);
    btl_native_module_add_function(vm, module, "bool", randomBool, 0);
    btl_native_module_add_function(vm, module, "chance", randomChance, 1);
    btl_native_module_add_function(vm, module, "choice", randomChoice, 1);
    btl_native_module_add_function(vm, module, "shuffle", randomShuffle, 1);
    btl_native_module_add_function(vm, module, "normal", randomNormal, 2);
    btl_native_module_add_function(vm, module, "dice", randomDice, 1);
    btl_native_module_add_function(vm, module, "diceSum", randomDiceSum, 2);

    btl_table_set(vm, &vm->nativeModules, OBJ_VAL(module->name), OBJ_VAL(module));
    btl_pop(vm);
}