#include "native_table.h"
#include "object.h"
#include "memory.h"

// length() - returns the number of entries in the table
static BtlValue tableLength(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) vm; (void) argCount; (void) args;
    ObjTable* table = AS_TABLE(receiver);
    return NUMBER_VAL(table->table.count);
}

// keys() - returns list of all keys
static BtlValue tableKeys(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount; (void) args;
    ObjTable* table = AS_TABLE(receiver);
    ObjList* list = btl_list_new(vm);
    btl_push(vm, OBJ_VAL(list));
    for (int i = 0; i < table->table.capacity; i++) {
        BtlEntry* entry = &table->table.entries[i];
        if (!IS_EMPTY(entry->key)) {
            btl_value_array_write(vm, &list->items, entry->key);
            btl_gc_write_barrier(vm, (BtlObj*) list, entry->key);
        }
    }
    btl_pop(vm);
    return OBJ_VAL(list);
}

// values() - returns list of all values
static BtlValue tableValues(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount; (void) args;
    ObjTable* table = AS_TABLE(receiver);
    ObjList* list = btl_list_new(vm);
    btl_push(vm, OBJ_VAL(list));
    for (int i = 0; i < table->table.capacity; i++) {
        BtlEntry* entry = &table->table.entries[i];
        if (!IS_EMPTY(entry->key)) {
            btl_value_array_write(vm, &list->items, entry->value);
            btl_gc_write_barrier(vm, (BtlObj*) list, entry->value);
        }
    }
    btl_pop(vm);
    return OBJ_VAL(list);
}

// has(key) - returns true if key exists in table
static BtlValue tableHas(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) vm; (void) argCount;
    ObjTable* table = AS_TABLE(receiver);
    BtlValue dummy;
    return BOOL_VAL(btl_table_get(&table->table, args[0], &dummy));
}

// remove(key) - removes entry with key, returns true if found
static BtlValue tableRemove(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) vm; (void) argCount;
    ObjTable* table = AS_TABLE(receiver);
    return BOOL_VAL(btl_table_delete(&table->table, args[0]));
}

// clear() - removes all entries, returns table
static BtlValue tableClear(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount; (void) args;
    ObjTable* table = AS_TABLE(receiver);
    btl_table_free(vm, &table->table);
    btl_table_init(&table->table);
    return receiver;
}

// clone() - returns shallow copy of table
static BtlValue tableClone(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount; (void) args;
    ObjTable* table = AS_TABLE(receiver);
    ObjTable* result = btl_table_obj_new(vm);
    btl_push(vm, OBJ_VAL(result));

    // Copy entries with write barriers
    for (int i = 0; i < table->table.capacity; i++) {
        BtlEntry* entry = &table->table.entries[i];
        if (!IS_EMPTY(entry->key)) {
            btl_table_set(vm, &result->table, entry->key, entry->value);
            btl_gc_write_barrier(vm, (BtlObj*) result, entry->key);
            btl_gc_write_barrier(vm, (BtlObj*) result, entry->value);
        }
    }

    btl_pop(vm);
    return OBJ_VAL(result);
}

// ----------------------------------------------------------------------------
// Table Class Initialization
// ----------------------------------------------------------------------------

void btl_table_class_init(VM* vm) {
    vm->tableClass = btl_native_class_new(vm, "Table");
    btl_native_class_add_method(vm, vm->tableClass, "length", tableLength, 0);
    btl_native_class_add_method(vm, vm->tableClass, "keys", tableKeys, 0);
    btl_native_class_add_method(vm, vm->tableClass, "values", tableValues, 0);
    btl_native_class_add_method(vm, vm->tableClass, "has", tableHas, 1);
    btl_native_class_add_method(vm, vm->tableClass, "remove", tableRemove, 1);
    btl_native_class_add_method(vm, vm->tableClass, "clear", tableClear, 0);
    btl_native_class_add_method(vm, vm->tableClass, "clone", tableClone, 0);
}