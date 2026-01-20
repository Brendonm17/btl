#include <stdlib.h>
#include <string.h>
#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"

#define TABLE_MAX_LOAD 0.75

void initTable(Table* table) {
    table->count = 0; table->capacity = 0; table->entries = NULL;
}

void freeTable(struct VM* vm, Table* table) {
    FREE_ARRAY(vm, Entry, table->entries, table->capacity);
    initTable(table);
}

static uint32_t hashValue(Value value) {
#ifdef NAN_BOXING
    if (IS_OBJ(value)) {
        // Strings have a pre-calculated hash. 
        // For other objects, we use the pointer itself as the basis for the hash.
        if (IS_STRING(value)) return AS_STRING(value)->hash;
        return (uint32_t) ((uintptr_t) AS_OBJ(value) >> 3);
    }
    // Mixing for Numbers/Bools (ensure identical values produce identical hashes)
    uint64_t bits = value;
    bits ^= bits >> 33;
    bits *= 0xff51afd7ed558ccd;
    bits ^= bits >> 33;
    return (uint32_t) bits;
#else
    switch (value.type) {
    case VAL_BOOL:   return AS_BOOL(value) ? 3 : 4;
    case VAL_NIL:    return 5;
    case VAL_NUMBER: {
        uint64_t bits;
        memcpy(&bits, &value.as.number, sizeof(double));
        return (uint32_t) (bits ^ (bits >> 32));
    }
    case VAL_OBJ:    return AS_OBJ(value)->hash;
    default:         return 0;
    }
#endif
}

static Entry* findEntry(Entry* entries, int capacity, Value key) {
    uint32_t hash = hashValue(key);
    uint32_t index = hash & (capacity - 1);
    Entry* tombstone = NULL;

    for (;;) {
        Entry* entry = &entries[index];

        if (IS_EMPTY(entry->key)) {
            if (IS_NIL(entry->value)) {
                // Empty slot
                return tombstone != NULL ? tombstone : entry;
            } else {
                // Tombstone found
                if (tombstone == NULL) tombstone = entry;
            }
        } else if (valuesEqual(entry->key, key)) {
            // Found the key
            return entry;
        }

        index = (index + 1) & (capacity - 1);
    }
}

bool tableGet(Table* table, Value key, Value* value) {
    if (table->count == 0)
        return false;
    Entry* entry = findEntry(table->entries, table->capacity, key);
    if (IS_EMPTY(entry->key))
        return false;
    *value = entry->value;
    return true;
}

static void adjustCapacity(struct VM* vm, Table* table, int capacity) {
    Entry* entries = ALLOCATE(vm, Entry, capacity);
    for (int i = 0; i < capacity; i++) {
        entries[i].key = EMPTY_VAL;
        entries[i].value = NIL_VAL;
    }
    table->count = 0;
    for (int i = 0; i < table->capacity; i++) {
        Entry* entry = &table->entries[i];
        if (IS_EMPTY(entry->key)) continue;

        Entry* dest = findEntry(entries, capacity, entry->key);
        dest->key = entry->key;
        dest->value = entry->value;
        table->count++;
    }
    FREE_ARRAY(vm, Entry, table->entries, table->capacity);
    table->entries = entries;
    table->capacity = capacity;
}

bool tableSet(struct VM* vm, Table* table, Value key, Value value) {
    if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
        int capacity = GROW_CAPACITY(table->capacity);
        adjustCapacity(vm, table, capacity);
    }
    Entry* entry = findEntry(table->entries, table->capacity, key);
    bool isNewKey = IS_EMPTY(entry->key);
    if (isNewKey && IS_NIL(entry->value))
        table->count++;
    entry->key = key;
    entry->value = value;
    return isNewKey;
}

bool tableDelete(Table* table, Value key) {
    if (table->count == 0)
        return false;
    Entry* entry = findEntry(table->entries, table->capacity, key);
    if (IS_EMPTY(entry->key))
        return false;
    // Place a tombstone: key is EMPTY, value is NIL (false/true marker)
    // In our findEntry, IS_EMPTY key + NIL value = truly empty
    // IS_EMPTY key + BOOL true value = tombstone
    entry->key = EMPTY_VAL;
    entry->value = BOOL_VAL(true);
    return true;
}

void tableAddAll(struct VM* vm, Table* from, Table* to) {
    if (from == to) return;
    for (int i = 0; i < from->capacity; i++) {
        Entry* entry = &from->entries[i];
        if (!IS_EMPTY(entry->key)) tableSet(vm, to, entry->key, entry->value);
    }
}

struct ObjString* tableFindString(Table* table, const char* chars, int length, uint32_t hash) {
    if (table->capacity == 0) return NULL;

    uint32_t index = hash & (table->capacity - 1);
    for (;;) {
        Entry* entry = &table->entries[index];
        if (IS_EMPTY(entry->key)) {
            // Stop if we hit a truly empty slot (not a tombstone)
            if (IS_NIL(entry->value)) return NULL;
        } else if (IS_STRING(entry->key)) {
            // We must extract the string object from the Value key first
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

void tableRemoveWhite(Table* table) {
    for (int i = 0; i < table->capacity; i++) {
        Entry* entry = &table->entries[i];
        // If the key is an object (string) and it isn't marked, delete it
        if (!IS_EMPTY(entry->key) && IS_OBJ(entry->key)) {
            Obj* obj = AS_OBJ(entry->key);
            if (!obj->isMarked) {
                tableDelete(table, entry->key);
            }
        }
    }
}

void markTable(struct VM* vm, Table* table) {
    for (int i = 0; i < table->capacity; i++) {
        Entry* entry = &table->entries[i];
        markValue(vm, entry->key);
        markValue(vm, entry->value);
    }
}