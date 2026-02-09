// ============================================================================
// table.c - BTL Hash Table Implementation
//
// This file implements a hash table using open addressing with linear probing.
// The table supports any BTL value as a key and uses tombstones for deletion
// to maintain probe sequences.
//
// Key features:
// - Power-of-2 capacity for fast modulo (bitwise AND)
// - 75% load factor threshold before resizing
// - Tombstone markers for deletion without breaking probe chains
// - Special string interning support via tableFindString
// ============================================================================

#include <stdlib.h>
#include <string.h>
#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"

// ----------------------------------------------------------------------------
// Constants
// ----------------------------------------------------------------------------

// Maximum load factor before table is resized
#define BTL_TABLE_MAX_LOAD 0.75

// ----------------------------------------------------------------------------
// btl_table_init
//
// Initialize a table to empty state.
//
// Parameters:
//   table - The table to initialize
// ----------------------------------------------------------------------------
void btl_table_init(BtlTable* table) {
    table->count = 0;
    table->capacity = 0;
    table->entries = NULL;
}

// ----------------------------------------------------------------------------
// btl_table_free
//
// Free all memory used by a table and reset to empty state.
//
// Parameters:
//   vm    - Virtual machine (for memory deallocation)
//   table - The table to free
// ----------------------------------------------------------------------------
void btl_table_free(struct VM* vm, BtlTable* table) {
    BTL_FREE_ARRAY(vm, BtlEntry, table->entries, table->capacity);
    btl_table_init(table);
}

// ----------------------------------------------------------------------------
// hashValue
//
// Compute hash for any BTL value. Strings use their pre-computed hash,
// other objects use pointer-based hashing, and primitives use bit mixing.
//
// Parameters:
//   value - The value to hash
//
// Returns:
//   32-bit hash value
// ----------------------------------------------------------------------------
static uint32_t hashValue(BtlValue value) {
#ifdef BTL_NAN_BOXING
    if (IS_OBJ(value)) {
        // Strings have a pre-calculated hash
        // Other objects use pointer as hash basis
        if (IS_STRING(value)) {
            return AS_STRING(value)->hash;
        }
        return (uint32_t)((uintptr_t)AS_OBJ(value) >> 3);
    }

    // For int values, hash the double representation so that
    // INT_VAL(42) and NUMBER_VAL(42.0) hash to the same bucket
    if (IS_INT(value)) {
        double d = (double)AS_INT(value);
        BtlValue numVal = NUMBER_VAL(d);
        uint64_t bits = numVal;
        bits ^= bits >> 33;
        bits *= 0xff51afd7ed558ccd;
        bits ^= bits >> 33;
        return (uint32_t)bits;
    }

    // Mix bits for numbers/bools to get good distribution
    uint64_t bits = value;
    bits ^= bits >> 33;
    bits *= 0xff51afd7ed558ccd;
    bits ^= bits >> 33;
    return (uint32_t)bits;
#else
    switch (value.type) {
        case BTL_VAL_BOOL:
            return AS_BOOL(value) ? 3 : 4;

        case BTL_VAL_NIL:
            return 5;

        case BTL_VAL_INT: {
            // Hash as double so INT_VAL(42) == NUMBER_VAL(42.0) hash the same
            double d = (double)AS_INT(value);
            uint64_t bits;
            memcpy(&bits, &d, sizeof(double));
            return (uint32_t)(bits ^ (bits >> 32));
        }

        case BTL_VAL_NUMBER: {
            uint64_t bits;
            memcpy(&bits, &value.as.number, sizeof(double));
            return (uint32_t)(bits ^ (bits >> 32));
        }

        case BTL_VAL_OBJ:
            return AS_OBJ(value)->hash;

        default:
            return 0;
    }
#endif
}

// ----------------------------------------------------------------------------
// findEntry
//
// Find the entry for a key using linear probing. Returns either:
// - The entry containing the key
// - An empty slot where the key should be inserted
// - A tombstone slot (if no empty slot found before it)
//
// Parameters:
//   entries  - The entries array to search
//   capacity - Size of the entries array
//   key      - The key to find
//
// Returns:
//   Pointer to the entry (may be empty/tombstone if key not found)
// ----------------------------------------------------------------------------
static BtlEntry* findEntry(BtlEntry* entries, int capacity, BtlValue key) {
    uint32_t hash = hashValue(key);
    uint32_t index = hash & (capacity - 1);
    BtlEntry* tombstone = NULL;

    for (;;) {
        BtlEntry* entry = &entries[index];

        if (IS_EMPTY(entry->key)) {
            if (IS_NULL(entry->value)) {
                // Truly empty slot - return tombstone if we found one, else this slot
                return tombstone != NULL ? tombstone : entry;
            } else {
                // This is a tombstone - remember first one found
                if (tombstone == NULL) {
                    tombstone = entry;
                }
            }
        } else if (btl_values_equal(entry->key, key)) {
            // Found the key
            return entry;
        }

        // Linear probe to next slot
        index = (index + 1) & (capacity - 1);
    }
}

// ----------------------------------------------------------------------------
// btl_table_get
//
// Look up a value by key.
//
// Parameters:
//   table - The table to search
//   key   - The key to look up
//   value - Output parameter for the value
//
// Returns:
//   true if key was found (value stored in *value), false otherwise
// ----------------------------------------------------------------------------
bool btl_table_get(BtlTable* table, BtlValue key, BtlValue* value) {
    if (table->count == 0) {
        return false;
    }

    BtlEntry* entry = findEntry(table->entries, table->capacity, key);
    if (IS_EMPTY(entry->key)) {
        return false;
    }

    *value = entry->value;
    return true;
}

// ----------------------------------------------------------------------------
// adjustCapacity
//
// Resize the table to the new capacity. All entries are rehashed into
// the new array. Tombstones are not copied.
//
// Parameters:
//   vm       - Virtual machine (for memory allocation)
//   table    - The table to resize
//   capacity - New capacity (must be power of 2)
// ----------------------------------------------------------------------------
static void adjustCapacity(struct VM* vm, BtlTable* table, int capacity) {
    // Allocate new entries array
    BtlEntry* entries = BTL_ALLOCATE(vm, BtlEntry, capacity);

    // Initialize all slots to empty
    for (int i = 0; i < capacity; i++) {
        entries[i].key = BTL_EMPTY_VAL;
        entries[i].value = BTL_NULL_VAL;
    }

    // Rehash all existing entries (skip tombstones)
    table->count = 0;
    for (int i = 0; i < table->capacity; i++) {
        BtlEntry* entry = &table->entries[i];
        if (IS_EMPTY(entry->key)) {
            continue;
        }

        BtlEntry* dest = findEntry(entries, capacity, entry->key);
        dest->key = entry->key;
        dest->value = entry->value;
        table->count++;
    }

    // Free old array and install new one
    BTL_FREE_ARRAY(vm, BtlEntry, table->entries, table->capacity);
    table->entries = entries;
    table->capacity = capacity;
}

// ----------------------------------------------------------------------------
// btl_table_set
//
// Insert or update a key-value pair.
//
// Parameters:
//   vm    - Virtual machine (for memory allocation if resize needed)
//   table - The table to modify
//   key   - The key to set
//   value - The value to associate with the key
//
// Returns:
//   true if this was a new key, false if updating existing key
// ----------------------------------------------------------------------------
bool btl_table_set(struct VM* vm, BtlTable* table, BtlValue key, BtlValue value) {
    // Check if we need to grow the table
    if (table->count + 1 > table->capacity * BTL_TABLE_MAX_LOAD) {
        int capacity = BTL_GROW_CAPACITY(table->capacity);
        adjustCapacity(vm, table, capacity);
    }

    BtlEntry* entry = findEntry(table->entries, table->capacity, key);
    bool isNewKey = IS_EMPTY(entry->key);

    // Only increment count for truly new keys (not tombstones)
    if (isNewKey && IS_NULL(entry->value)) {
        table->count++;
    }

    entry->key = key;
    entry->value = value;
    return isNewKey;
}

// ----------------------------------------------------------------------------
// btl_table_delete
//
// Remove an entry from the table. Uses tombstone markers to preserve
// probe chains for other entries.
//
// Parameters:
//   table - The table to modify
//   key   - The key to delete
//
// Returns:
//   true if key was found and deleted, false if not found
// ----------------------------------------------------------------------------
bool btl_table_delete(BtlTable* table, BtlValue key) {
    if (table->count == 0) {
        return false;
    }

    BtlEntry* entry = findEntry(table->entries, table->capacity, key);
    if (IS_EMPTY(entry->key)) {
        return false;
    }

    // Place a tombstone: key = EMPTY, value = true (marks it as tombstone)
    entry->key = BTL_EMPTY_VAL;
    entry->value = BOOL_VAL(true);
    table->count--;
    return true;
}

// ----------------------------------------------------------------------------
// btl_table_add_all
//
// Copy all entries from one table to another.
//
// Parameters:
//   vm   - Virtual machine (for memory allocation)
//   from - Source table
//   to   - Destination table
// ----------------------------------------------------------------------------
void btl_table_add_all(struct VM* vm, BtlTable* from, BtlTable* to) {
    if (from == to) {
        return;
    }

    for (int i = 0; i < from->capacity; i++) {
        BtlEntry* entry = &from->entries[i];
        if (!IS_EMPTY(entry->key)) {
            btl_table_set(vm, to, entry->key, entry->value);
        }
    }
}

// ----------------------------------------------------------------------------
// btl_table_find_string
//
// Find an interned string by its content and hash. This is used during
// string creation to check if an identical string already exists.
//
// Unlike regular lookup, this compares string contents directly since
// we're searching for string interning (not by identity).
//
// Parameters:
//   table  - The string intern table
//   chars  - The string content to find
//   length - Length of the string
//   hash   - Pre-computed hash of the string
//
// Returns:
//   Pointer to existing interned string, or NULL if not found
// ----------------------------------------------------------------------------
struct ObjString* btl_table_find_string(BtlTable* table, const char* chars,
                                         int length, uint32_t hash) {
    if (table->capacity == 0) {
        return NULL;
    }

    uint32_t index = hash & (table->capacity - 1);

    for (;;) {
        BtlEntry* entry = &table->entries[index];

        if (IS_EMPTY(entry->key)) {
            // Stop if we hit a truly empty slot (not a tombstone)
            if (IS_NULL(entry->value)) {
                return NULL;
            }
        } else if (IS_STRING(entry->key)) {
            // Compare string contents
            ObjString* string = AS_STRING(entry->key);
            if (string->length == length &&
                string->hash == hash &&
                memcmp(string->chars, chars, length) == 0) {
                return string;
            }
        }

        index = (index + 1) & (table->capacity - 1);
    }
}

// ----------------------------------------------------------------------------
// btl_table_remove_white
//
// Remove entries whose keys are unmarked (white) objects. Used during
// GC sweep phase for weak references like the string intern table.
//
// Parameters:
//   table - The table to clean
// ----------------------------------------------------------------------------
void btl_table_remove_white(BtlTable* table) {
    for (int i = 0; i < table->capacity; i++) {
        BtlEntry* entry = &table->entries[i];

        if (!IS_EMPTY(entry->key) && IS_OBJ(entry->key)) {
            BtlObj* obj = AS_OBJ(entry->key);
            if (!obj->isMarked) {
                // Mark slot as truly empty (not tombstone)
                entry->key = BTL_EMPTY_VAL;
                entry->value = BTL_NULL_VAL;
                table->count--;
            }
        }
    }
}

// ----------------------------------------------------------------------------
// btl_table_mark
//
// Mark all keys and values in the table as reachable for garbage collection.
//
// Parameters:
//   vm    - Virtual machine (for GC marking)
//   table - The table to mark
// ----------------------------------------------------------------------------
void btl_table_mark(struct VM* vm, BtlTable* table) {
    for (int i = 0; i < table->capacity; i++) {
        BtlEntry* entry = &table->entries[i];
        btl_gc_mark_value(vm, entry->key);
        btl_gc_mark_value(vm, entry->value);
    }
}
