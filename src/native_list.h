// ============================================================================
// native_list.h - BTL Native List Methods
//
// Provides built-in methods for list objects:
// - length(): Get list size
// - push(value): Add to end
// - pop(): Remove and return last
// - insert(index, value): Insert at index
// - remove(index): Remove at index
// - indexOf(value): Find index of value
// - contains(value): Check if value exists
// - join(separator): Join elements into string
// - reverse(): Reverse in place
// - sort(): Sort in place
// ============================================================================

#ifndef btl_native_list_h
#define btl_native_list_h

#include "vm.h"

// Initialize the list native class and register methods
void btl_list_class_init(VM* vm);

#endif // btl_native_list_h
