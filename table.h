#ifndef btl_table_h
#define btl_table_h

#include "common.h"
#include "value.h"

typedef struct {
    struct ObjString* key;
    Value value;
} Entry;

typedef struct {
    int count;
    int capacity;
    Entry* entries;
} Table;

void initTable(Table* table);
void freeTable(struct VM* vm, Table* table);
bool tableGet(Table* table, struct ObjString* key, Value* value);
bool tableSet(struct VM* vm, Table* table, struct ObjString* key, Value value);
bool tableDelete(Table* table, struct ObjString* key);
void tableAddAll(struct VM* vm, Table* from, Table* to);
struct ObjString* tableFindString(Table* table, const char* chars, int length, uint32_t hash);
void tableRemoveWhite(Table* table);
void markTable(struct VM* vm, Table* table);

#endif