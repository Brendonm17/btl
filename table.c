#include <stdlib.h>
#include <string.h>
#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"

#define TABLE_MAX_LOAD 0.75

void initTable(Table* table) {
    table->count = 0;
    table->capacity = 0;
    table->entries = NULL;
}

void freeTable(struct VM* vm, Table* table) {
    FREE_ARRAY(vm, Entry, table->entries, table->capacity);
    initTable(table);
}

static Entry* findEntry(Entry* entries, int capacity, ObjString* key) {
    uint32_t index = key->hash & (capacity - 1);
    Entry* tombstone = NULL;
    for (;;) {
        Entry* entry = &entries[index];
        if (entry->key == NULL) {
            if (IS_NIL(entry->value)) return tombstone != NULL ? tombstone : entry;
            else if (tombstone == NULL) tombstone = entry;
        } else if (entry->key == key) return entry;
        index = (index + 1) & (capacity - 1);
    }
}

bool tableGet(Table* table, ObjString* key, Value* value) {
    if (table->count == 0) return false;
    Entry* entry = findEntry(table->entries, table->capacity, key);
    if (entry->key == NULL) return false;
    *value = entry->value;
    return true;
}

bool tableSet(struct VM* vm, Table* table, ObjString* key, Value value) {
    if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
        int cap = GROW_CAPACITY(table->capacity);
        Entry* entries = ALLOCATE(vm, Entry, cap);
        for (int i = 0; i < cap; i++) {
            entries[i].key = NULL; entries[i].value = NIL_VAL;
        }
        table->count = 0;
        for (int i = 0; i < table->capacity; i++) {
            Entry* e = &table->entries[i];
            if (e->key == NULL) continue;
            Entry* dest = findEntry(entries, cap, e->key);
            dest->key = e->key; dest->value = e->value; table->count++;
        }
        FREE_ARRAY(vm, Entry, table->entries, table->capacity);
        table->entries = entries; table->capacity = cap;
    }
    Entry* entry = findEntry(table->entries, table->capacity, key);
    bool isNew = entry->key == NULL;
    if (isNew && IS_NIL(entry->value)) table->count++;
    entry->key = key; entry->value = value;
    return isNew;
}

bool tableDelete(Table* table, ObjString* key) {
    if (table->count == 0) return false;
    Entry* entry = findEntry(table->entries, table->capacity, key);
    if (entry->key == NULL) return false;
    entry->key = NULL; entry->value = BOOL_VAL(true);
    return true;
}

void tableAddAll(struct VM* vm, Table* from, Table* to) {
    for (int i = 0; i < from->capacity; i++) {
        Entry* e = &from->entries[i];
        if (e->key != NULL) tableSet(vm, to, e->key, e->value);
    }
}

ObjString* tableFindString(Table* table, const char* chars, int len, uint32_t hash) {
    if (table->count == 0) return NULL;
    uint32_t index = hash & (table->capacity - 1);
    for (;;) {
        Entry* e = &table->entries[index];
        if (e->key == NULL) {
            if (IS_NIL(e->value)) return NULL;
        } else if (e->key->length == len && e->key->hash == hash && memcmp(e->key->chars, chars, len) == 0) return e->key;
        index = (index + 1) & (table->capacity - 1);
    }
}

void tableRemoveWhite(Table* table) {
    for (int i = 0; i < table->capacity; i++) {
        Entry* e = &table->entries[i];
        if (e->key != NULL && !e->key->obj.isMarked) tableDelete(table, e->key);
    }
}

void markTable(struct VM* vm, Table* table) {
    for (int i = 0; i < table->capacity; i++) {
        Entry* e = &table->entries[i];
        markObject(vm, (Obj*) e->key);
        markValue(vm, e->value);
    }
}