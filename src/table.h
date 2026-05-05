#ifndef btl_table_h
#define btl_table_h

#include "common.h"
#include "value.h"

struct ObjString;

// Slot states:
//   empty      key = EMPTY_VAL, value = NULL_VAL
//   tombstone  key = EMPTY_VAL, value = BOOL_VAL(true)
//   occupied   key = real key, value = real value
typedef struct {
    BtlValue key;
    BtlValue value;
} BtlEntry;

// Capacity is always a power of 2 so probing can use bitwise AND.
typedef struct {
    int count;
    int capacity;
    BtlEntry* entries;
} BtlTable;

void btl_table_init(BtlTable* table);
void btl_table_free(struct VM* vm, BtlTable* table);

// Returns false if not found; on hit, writes the value through *value.
bool btl_table_get(BtlTable* table, BtlValue key, BtlValue* value);

// Returns true if the key was new.
bool btl_table_set(struct VM* vm, BtlTable* table, BtlValue key, BtlValue value);

bool btl_table_delete(BtlTable* table, BtlValue key);

void btl_table_add_all(struct VM* vm, BtlTable* from, BtlTable* to);

// Used by string interning to look up an existing string by content.
struct ObjString* btl_table_find_string(BtlTable* table, const char* chars,
                                         int length, uint32_t hash);

// Sweep weak refs: drop entries whose object keys weren't marked.
void btl_table_remove_white(BtlTable* table);

void btl_table_mark(struct VM* vm, BtlTable* table);

#endif
