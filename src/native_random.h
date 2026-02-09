// ============================================================================
// native_random.h - BTL Native Random Module
//
// Provides random number generation:
// - random(): Random float [0, 1)
// - randomInt(min, max): Random integer in range
// - randomChoice(list): Random element from list
// - shuffle(list): Shuffle list in place
// - seed(value): Seed the random generator
// ============================================================================

#ifndef btl_native_random_h
#define btl_native_random_h

#include "vm.h"

// Initialize the random native module
void btl_random_module_init(VM* vm);

#endif // btl_native_random_h
