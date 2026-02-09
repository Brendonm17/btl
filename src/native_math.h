// ============================================================================
// native_math.h - BTL Native Math Module
//
// Provides mathematical constants and functions:
// - Constants: PI, E, TAU
// - Basic: abs, floor, ceil, round, sqrt, pow
// - Trigonometric: sin, cos, tan, asin, acos, atan, atan2
// - Logarithmic: log, log10, log2, exp
// - Other: min, max, clamp
// ============================================================================

#ifndef btl_native_math_h
#define btl_native_math_h

#include "vm.h"

// Initialize the math native module
void btl_math_module_init(VM* vm);

#endif // btl_native_math_h
