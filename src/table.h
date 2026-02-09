// ============================================================================
// table.h - BTL Hash Table
//
// This header defines the hash table implementation used throughout BTL.
// Tables use open addressing with linear probing and support any BTL value
// as keys (strings, numbers, booleans, objects).
//
// The hash table is used for:
// - String interning (deduplication)
// - Module global variables
// - Object tables (user-level dictionaries)
// - Internal mappings
// ============================================================================

#ifndef btl_table_h
#define btl_table_h

#include "common.h"
#include "value.h"

// Forward declaration for ObjString
struct ObjString;

// ----------------------------------------------------------------------------
// BtlEntry Structure
//
// A single entry in the hash table. Uses empty sentinel value for the key
// to indicate unused slots, and tombstones for deleted entries.
//
// Slot states:
// - Empty:     key = EMPTY_VAL, value = NULL_VAL
// - Tombstone: key = EMPTY_VAL, value = BOOL_VAL(true)
// - Occupied:  key = actual key, value = actual value
// ----------------------------------------------------------------------------
typedef struct {
    BtlValue key;      // The key (EMPTY_VAL if slot is empty/tombstone)
    BtlValue value;    // The value (or tombstone marker)
} BtlEntry;

// ----------------------------------------------------------------------------
// BtlTable Structure
//
// A hash table with open addressing. Capacity is always a power of 2
// for fast modulo via bitwise AND.
// ----------------------------------------------------------------------------
typedef struct {
    int count;          // Number of entries (excluding tombstones)
    int capacity;       // Size of entries array (always power of 2)
    BtlEntry* entries;  // Array of entries
} BtlTable;

// ----------------------------------------------------------------------------
// Table Operations
// ----------------------------------------------------------------------------

// Initialize a table to empty state
void btl_table_init(BtlTable* table);

// Free all memory used by a table
void btl_table_free(struct VM* vm, BtlTable* table);

// Get value associated with key. Returns false if not found.
// If found, stores the value in *value.
bool btl_table_get(BtlTable* table, BtlValue key, BtlValue* value);

// Set key to value. Returns true if key was new, false if updating.
bool btl_table_set(struct VM* vm, BtlTable* table, BtlValue key, BtlValue value);

// Delete entry for key. Returns true if found and deleted.
bool btl_table_delete(BtlTable* table, BtlValue key);

// Copy all entries from source table to destination table
void btl_table_add_all(struct VM* vm, BtlTable* from, BtlTable* to);

// Find an interned string in the table by content and hash.
// Used by string interning to find existing strings.
struct ObjString* btl_table_find_string(BtlTable* table, const char* chars,
                                         int length, uint32_t hash);

// Remove entries with unmarked (white) object keys during GC sweep.
// Used for weak references in the string intern table.
void btl_table_remove_white(BtlTable* table);

// Mark all entries in the table as reachable for GC
void btl_table_mark(struct VM* vm, BtlTable* table);

#endif // btl_table_h
