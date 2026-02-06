// ============================================================================
// native_table.h - BTL Native Table Methods
//
// Provides built-in methods for table (dictionary) objects:
// - length(): Get number of entries
// - keys(): Get list of keys
// - values(): Get list of values
// - entries(): Get list of [key, value] pairs
// - has(key): Check if key exists
// - remove(key): Remove entry
// - clear(): Remove all entries
// ============================================================================

#ifndef btl_native_table_h
#define btl_native_table_h

#include "vm.h"

// Initialize the table native class and register methods
void btl_table_class_init(VM* vm);

#endif // btl_native_table_h
