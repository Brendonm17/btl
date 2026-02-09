// ============================================================================
// native_string.h - BTL Native String Methods
//
// Provides built-in methods for string values:
// - length(): Get string length
// - charAt(index): Get character at index
// - substring(start, end): Extract substring
// - indexOf(substr): Find substring position
// - contains(substr): Check if contains substring
// - startsWith(prefix): Check prefix
// - endsWith(suffix): Check suffix
// - toUpper(): Convert to uppercase
// - toLower(): Convert to lowercase
// - trim(): Remove whitespace
// - split(delimiter): Split into list
// - replace(old, new): Replace occurrences
// ============================================================================

#ifndef btl_native_string_h
#define btl_native_string_h

#include "vm.h"

// Initialize the string native class and register methods
void btl_string_class_init(VM* vm);

#endif // btl_native_string_h
