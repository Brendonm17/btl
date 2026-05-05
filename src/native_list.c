#include <string.h>
#include "native_list.h"
#include "object.h"
#include "memory.h"

static BtlValue listLength(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) vm; (void) argCount; (void) args;
    ObjList* list = AS_LIST(receiver);
    return NUMBER_VAL(list->items.count);
}

static BtlValue listPush(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount;
    ObjList* list = AS_LIST(receiver);
    btl_value_array_write(vm, &list->items, args[0]);
    btl_gc_write_barrier(vm, (BtlObj*) list, args[0]);
    return receiver;
}

static BtlValue listPop(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount; (void) args;
    ObjList* list = AS_LIST(receiver);
    if (list->items.count == 0) {
        btl_runtime_error(vm, "Cannot pop from empty list.");
        return BTL_NULL_VAL;
    }
    BtlValue val = list->items.values[--list->items.count];
    return val;
}

static BtlValue listShift(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount; (void) args;
    ObjList* list = AS_LIST(receiver);
    if (list->items.count == 0) {
        btl_runtime_error(vm, "Cannot shift from empty list.");
        return BTL_NULL_VAL;
    }
    BtlValue val = list->items.values[0];
    for (int i = 0; i < list->items.count - 1; i++) {
        list->items.values[i] = list->items.values[i + 1];
    }
    list->items.count--;
    return val;
}

static BtlValue listUnshift(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount;
    ObjList* list = AS_LIST(receiver);
    btl_value_array_write(vm, &list->items, BTL_NULL_VAL);
    for (int i = list->items.count - 1; i > 0; i--) {
        list->items.values[i] = list->items.values[i - 1];
    }
    list->items.values[0] = args[0];
    btl_gc_write_barrier(vm, (BtlObj*) list, args[0]);
    return receiver;
}

static BtlValue listInsert(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount;
    ObjList* list = AS_LIST(receiver);
    if (!IS_NUMERIC(args[0])) {
        btl_runtime_error(vm, "Index must be a number.");
        return BTL_NULL_VAL;
    }
    int index = (int) btl_numeric_to_double(args[0]);
    if (index < 0 || index > list->items.count) {
        btl_runtime_error(vm, "Index out of bounds.");
        return BTL_NULL_VAL;
    }
    btl_value_array_write(vm, &list->items, BTL_NULL_VAL);
    for (int i = list->items.count - 1; i > index; i--) {
        list->items.values[i] = list->items.values[i - 1];
    }
    list->items.values[index] = args[1];
    btl_gc_write_barrier(vm, (BtlObj*) list, args[1]);
    return receiver;
}

static BtlValue listRemove(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount;
    ObjList* list = AS_LIST(receiver);
    if (!IS_NUMERIC(args[0])) {
        btl_runtime_error(vm, "Index must be a number.");
        return BTL_NULL_VAL;
    }
    int index = (int) btl_numeric_to_double(args[0]);
    if (index < 0 || index >= list->items.count) {
        btl_runtime_error(vm, "Index out of bounds.");
        return BTL_NULL_VAL;
    }
    BtlValue val = list->items.values[index];
    for (int i = index; i < list->items.count - 1; i++) {
        list->items.values[i] = list->items.values[i + 1];
    }
    list->items.count--;
    return val;
}

static BtlValue listClear(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) vm; (void) argCount; (void) args;
    ObjList* list = AS_LIST(receiver);
    list->items.count = 0;
    return receiver;
}

// Returns -1 if not found.
static BtlValue listIndexOf(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) vm; (void) argCount;
    ObjList* list = AS_LIST(receiver);
    for (int i = 0; i < list->items.count; i++) {
        if (btl_values_equal(list->items.values[i], args[0])) {
            return NUMBER_VAL(i);
        }
    }
    return NUMBER_VAL(-1);
}

static BtlValue listContains(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) vm; (void) argCount;
    ObjList* list = AS_LIST(receiver);
    for (int i = 0; i < list->items.count; i++) {
        if (btl_values_equal(list->items.values[i], args[0])) {
            return BTL_TRUE_VAL;
        }
    }
    return BTL_FALSE_VAL;
}

static BtlValue listReverse(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) vm; (void) argCount; (void) args;
    ObjList* list = AS_LIST(receiver);
    int left = 0;
    int right = list->items.count - 1;
    while (left < right) {
        BtlValue temp = list->items.values[left];
        list->items.values[left] = list->items.values[right];
        list->items.values[right] = temp;
        left++;
        right--;
    }
    return receiver;
}

// slice(start, end?). Negative indices count from the end.
static BtlValue listSlice(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    ObjList* list = AS_LIST(receiver);
    if (!IS_NUMERIC(args[0])) {
        btl_runtime_error(vm, "Start index must be a number.");
        return BTL_NULL_VAL;
    }
    int start = (int) btl_numeric_to_double(args[0]);
    int end = list->items.count;
    if (argCount >= 2 && IS_NUMERIC(args[1])) {
        end = (int) btl_numeric_to_double(args[1]);
    }
    if (start < 0) start = list->items.count + start;
    if (end < 0) end = list->items.count + end;
    if (start < 0) start = 0;
    if (end > list->items.count) end = list->items.count;
    if (start >= end) {
        return OBJ_VAL(btl_list_new(vm));
    }
    ObjList* result = btl_list_new(vm);
    btl_push(vm, OBJ_VAL(result));
    for (int i = start; i < end; i++) {
        btl_value_array_write(vm, &result->items, list->items.values[i]);
        btl_gc_write_barrier(vm, (BtlObj*) result, list->items.values[i]);
    }
    btl_pop(vm);
    return OBJ_VAL(result);
}

static BtlValue listJoin(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount;
    ObjList* list = AS_LIST(receiver);
    if (!IS_STRING(args[0])) {
        btl_runtime_error(vm, "Separator must be a string.");
        return BTL_NULL_VAL;
    }
    ObjString* sep = AS_STRING(args[0]);

    if (list->items.count == 0) {
        return OBJ_VAL(btl_string_copy(vm, "", 0));
    }

    int totalLen = 0;
    for (int i = 0; i < list->items.count; i++) {
        if (!IS_STRING(list->items.values[i])) {
            btl_runtime_error(vm, "All list elements must be strings.");
            return BTL_NULL_VAL;
        }
        totalLen += AS_STRING(list->items.values[i])->length;
    }
    totalLen += sep->length * (list->items.count - 1);

    char* result = BTL_ALLOCATE(vm, char, totalLen + 1);
    char* dst = result;
    for (int i = 0; i < list->items.count; i++) {
        if (i > 0) {
            memcpy(dst, sep->chars, sep->length);
            dst += sep->length;
        }
        ObjString* s = AS_STRING(list->items.values[i]);
        memcpy(dst, s->chars, s->length);
        dst += s->length;
    }
    *dst = '\0';

    return OBJ_VAL(btl_string_take(vm, result, totalLen));
}

// Shallow copy.
static BtlValue listClone(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount; (void) args;
    ObjList* list = AS_LIST(receiver);
    ObjList* result = btl_list_new(vm);
    btl_push(vm, OBJ_VAL(result));
    for (int i = 0; i < list->items.count; i++) {
        btl_value_array_write(vm, &result->items, list->items.values[i]);
        btl_gc_write_barrier(vm, (BtlObj*) result, list->items.values[i]);
    }
    btl_pop(vm);
    return OBJ_VAL(result);
}

void btl_list_class_init(VM* vm) {
    vm->listClass = btl_native_class_new(vm, "List");
    btl_native_class_add_method(vm, vm->listClass, "length", listLength, 0);
    btl_native_class_add_method(vm, vm->listClass, "push", listPush, 1);
    btl_native_class_add_method(vm, vm->listClass, "pop", listPop, 0);
    btl_native_class_add_method(vm, vm->listClass, "shift", listShift, 0);
    btl_native_class_add_method(vm, vm->listClass, "unshift", listUnshift, 1);
    btl_native_class_add_method(vm, vm->listClass, "insert", listInsert, 2);
    btl_native_class_add_method(vm, vm->listClass, "remove", listRemove, 1);
    btl_native_class_add_method(vm, vm->listClass, "clear", listClear, 0);
    btl_native_class_add_method(vm, vm->listClass, "indexOf", listIndexOf, 1);
    btl_native_class_add_method(vm, vm->listClass, "contains", listContains, 1);
    btl_native_class_add_method(vm, vm->listClass, "reverse", listReverse, 0);
    btl_native_class_add_method(vm, vm->listClass, "slice", listSlice, -1);
    btl_native_class_add_method(vm, vm->listClass, "join", listJoin, 1);
    btl_native_class_add_method(vm, vm->listClass, "clone", listClone, 0);
}
