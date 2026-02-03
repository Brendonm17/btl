#include "native_table.h"
#include "object.h"
#include "memory.h"

static Value tableLength(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) argCount; (void) args;
    ObjTable* table = AS_TABLE(receiver);
    return NUMBER_VAL(table->table.count);
}

static Value tableKeys(VM* vm, Value receiver, int argCount, Value* args) {
    (void) argCount; (void) args;
    ObjTable* table = AS_TABLE(receiver);
    ObjList* list = newList(vm);
    push(vm, OBJ_VAL(list));
    for (int i = 0; i < table->table.capacity; i++) {
        Entry* entry = &table->table.entries[i];
        if (!IS_EMPTY(entry->key)) {
            writeValueArray(vm, &list->items, entry->key);
        }
    }
    pop(vm);
    return OBJ_VAL(list);
}

static Value tableValues(VM* vm, Value receiver, int argCount, Value* args) {
    (void) argCount; (void) args;
    ObjTable* table = AS_TABLE(receiver);
    ObjList* list = newList(vm);
    push(vm, OBJ_VAL(list));
    for (int i = 0; i < table->table.capacity; i++) {
        Entry* entry = &table->table.entries[i];
        if (!IS_EMPTY(entry->key)) {
            writeValueArray(vm, &list->items, entry->value);
        }
    }
    pop(vm);
    return OBJ_VAL(list);
}

static Value tableHas(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) argCount;
    ObjTable* table = AS_TABLE(receiver);
    Value dummy;
    return BOOL_VAL(tableGet(&table->table, args[0], &dummy));
}

static Value tableRemove(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) argCount;
    ObjTable* table = AS_TABLE(receiver);
    return BOOL_VAL(tableDelete(&table->table, args[0]));
}

static Value tableClear(VM* vm, Value receiver, int argCount, Value* args) {
    (void) argCount; (void) args;
    ObjTable* table = AS_TABLE(receiver);
    freeTable(vm, &table->table);
    initTable(&table->table);
    return receiver;
}

static Value tableClone(VM* vm, Value receiver, int argCount, Value* args) {
    (void) argCount; (void) args;
    ObjTable* table = AS_TABLE(receiver);
    ObjTable* result = newTable(vm);
    push(vm, OBJ_VAL(result));
    tableAddAll(vm, &table->table, &result->table);
    pop(vm);
    return OBJ_VAL(result);
}

void initTableClass(VM* vm) {
    vm->tableClass = newNativeClass(vm, "Table");
    defineNativeMethod(vm, vm->tableClass, "length", tableLength, 0);
    defineNativeMethod(vm, vm->tableClass, "keys", tableKeys, 0);
    defineNativeMethod(vm, vm->tableClass, "values", tableValues, 0);
    defineNativeMethod(vm, vm->tableClass, "has", tableHas, 1);
    defineNativeMethod(vm, vm->tableClass, "remove", tableRemove, 1);
    defineNativeMethod(vm, vm->tableClass, "clear", tableClear, 0);
    defineNativeMethod(vm, vm->tableClass, "clone", tableClone, 0);
}