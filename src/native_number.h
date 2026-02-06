// ============================================================================
// native_number.h - BTL Native Number Methods
//
// Provides built-in methods for number values:
// - toString(): Convert to string
// - toFixed(digits): Format with fixed decimals
// - toInt(): Truncate to integer
// - isNaN(): Check if NaN
// - isFinite(): Check if finite
// ============================================================================

#ifndef btl_native_number_h
#define btl_native_number_h

#include "vm.h"

// Initialize the number native class and register methods
void btl_number_class_init(VM* vm);

#endif // btl_native_number_h
