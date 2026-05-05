// Hash table with open addressing, linear probing, and tombstone deletion.
// Capacity is always a power of 2; load factor maxes at 75%.

#include <stdlib.h>
#include <string.h>
#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"

#define BTL_TABLE_MAX_LOAD 0.75

void btl_table_init(BtlTable* table) {
    table->count = 0;
    table->capacity = 0;
    table->entries = NULL;
}

void btl_table_free(struct VM* vm, BtlTable* table) {
    BTL_FREE_ARRAY(vm, BtlEntry, table->entries, table->capacity);
    btl_table_init(table);
}

static uint32_t hashValue(BtlValue value) {
#ifdef BTL_NAN_BOXING
    if (IS_OBJ(value)) {
        if (IS_STRING(value)) {
            return AS_STRING(value)->hash;
        }
        return (uint32_t)((uintptr_t)AS_OBJ(value) >> 3);
    }

    // Hash int as double so INT_VAL(42) and NUMBER_VAL(42.0) collide.
    if (IS_INT(value)) {
        double d = (double)AS_INT(value);
        BtlValue numVal = NUMBER_VAL(d);
        uint64_t bits = numVal;
        bits ^= bits >> 33;
        bits *= 0xff51afd7ed558ccd;
        bits ^= bits >> 33;
        return (uint32_t)bits;
    }

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

// Returns the entry holding key, or the slot where it would be inserted.
// Tombstones are tracked so the first one along the probe chain is reused.
static BtlEntry* findEntry(BtlEntry* entries, int capacity, BtlValue key) {
    uint32_t hash = hashValue(key);
    uint32_t index = hash & (capacity - 1);
    BtlEntry* tombstone = NULL;

    for (;;) {
        BtlEntry* entry = &entries[index];

        if (IS_EMPTY(entry->key)) {
            if (IS_NULL(entry->value)) {
                return tombstone != NULL ? tombstone : entry;
            } else {
                if (tombstone == NULL) {
                    tombstone = entry;
                }
            }
        } else if (btl_values_equal(entry->key, key)) {
            return entry;
        }

        index = (index + 1) & (capacity - 1);
    }
}

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

static void adjustCapacity(struct VM* vm, BtlTable* table, int capacity) {
    BtlEntry* entries = BTL_ALLOCATE(vm, BtlEntry, capacity);

    for (int i = 0; i < capacity; i++) {
        entries[i].key = BTL_EMPTY_VAL;
        entries[i].value = BTL_NULL_VAL;
    }

    // Rehash live entries; tombstones get dropped.
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

    BTL_FREE_ARRAY(vm, BtlEntry, table->entries, table->capacity);
    table->entries = entries;
    table->capacity = capacity;
}

bool btl_table_set(struct VM* vm, BtlTable* table, BtlValue key, BtlValue value) {
    if (table->count + 1 > table->capacity * BTL_TABLE_MAX_LOAD) {
        int capacity = BTL_GROW_CAPACITY(table->capacity);
        adjustCapacity(vm, table, capacity);
    }

    BtlEntry* entry = findEntry(table->entries, table->capacity, key);
    bool isNewKey = IS_EMPTY(entry->key);

    // Only count truly new keys, not tombstone reuses.
    if (isNewKey && IS_NULL(entry->value)) {
        table->count++;
    }

    entry->key = key;
    entry->value = value;
    return isNewKey;
}

bool btl_table_delete(BtlTable* table, BtlValue key) {
    if (table->count == 0) {
        return false;
    }

    BtlEntry* entry = findEntry(table->entries, table->capacity, key);
    if (IS_EMPTY(entry->key)) {
        return false;
    }

    // Tombstone: empty key, non-null value.
    entry->key = BTL_EMPTY_VAL;
    entry->value = BOOL_VAL(true);
    table->count--;
    return true;
}

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

// Compares string contents directly. Used during string interning before
// the new ObjString exists in the table.
struct ObjString* btl_table_find_string(BtlTable* table, const char* chars,
                                         int length, uint32_t hash) {
    if (table->capacity == 0) {
        return NULL;
    }

    uint32_t index = hash & (table->capacity - 1);

    for (;;) {
        BtlEntry* entry = &table->entries[index];

        if (IS_EMPTY(entry->key)) {
            if (IS_NULL(entry->value)) {
                return NULL;
            }
        } else if (IS_STRING(entry->key)) {
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

// Sweep weak references during GC: drop entries whose keys are unmarked.
void btl_table_remove_white(BtlTable* table) {
    for (int i = 0; i < table->capacity; i++) {
        BtlEntry* entry = &table->entries[i];

        if (!IS_EMPTY(entry->key) && IS_OBJ(entry->key)) {
            BtlObj* obj = AS_OBJ(entry->key);
            if (!obj->isMarked) {
                entry->key = BTL_EMPTY_VAL;
                entry->value = BTL_NULL_VAL;
                table->count--;
            }
        }
    }
}

void btl_table_mark(struct VM* vm, BtlTable* table) {
    for (int i = 0; i < table->capacity; i++) {
        BtlEntry* entry = &table->entries[i];
        btl_gc_mark_value(vm, entry->key);
        btl_gc_mark_value(vm, entry->value);
    }
}
