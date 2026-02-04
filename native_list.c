#include <string.h>
#include "native_list.h"
#include "object.h"
#include "memory.h"

static Value listLength(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) argCount; (void) args;
    ObjList* list = AS_LIST(receiver);
    return NUMBER_VAL(list->items.count);
}

static Value listPush(VM* vm, Value receiver, int argCount, Value* args) {
    (void) argCount;
    ObjList* list = AS_LIST(receiver);
    writeValueArray(vm, &list->items, args[0]);
    writeBarrier(vm, (Obj*) list, args[0]);
    return receiver;
}

static Value listPop(VM* vm, Value receiver, int argCount, Value* args) {
    (void) argCount; (void) args;
    ObjList* list = AS_LIST(receiver);
    if (list->items.count == 0) {
        runtimeError(vm, "Cannot pop from empty list.");
        return NULL_VAL;
    }
    Value val = list->items.values[--list->items.count];
    return val;
}

static Value listShift(VM* vm, Value receiver, int argCount, Value* args) {
    (void) argCount; (void) args;
    ObjList* list = AS_LIST(receiver);
    if (list->items.count == 0) {
        runtimeError(vm, "Cannot shift from empty list.");
        return NULL_VAL;
    }
    Value val = list->items.values[0];
    for (int i = 0; i < list->items.count - 1; i++) {
        list->items.values[i] = list->items.values[i + 1];
    }
    list->items.count--;
    return val;
}

static Value listUnshift(VM* vm, Value receiver, int argCount, Value* args) {
    (void) argCount;
    ObjList* list = AS_LIST(receiver);
    writeValueArray(vm, &list->items, NULL_VAL);
    for (int i = list->items.count - 1; i > 0; i--) {
        list->items.values[i] = list->items.values[i - 1];
    }
    list->items.values[0] = args[0];
    writeBarrier(vm, (Obj*) list, args[0]);
    return receiver;
}

static Value listInsert(VM* vm, Value receiver, int argCount, Value* args) {
    (void) argCount;
    ObjList* list = AS_LIST(receiver);
    if (!IS_NUMBER(args[0])) {
        runtimeError(vm, "Index must be a number.");
        return NULL_VAL;
    }
    int index = (int) AS_NUMBER(args[0]);
    if (index < 0 || index > list->items.count) {
        runtimeError(vm, "Index out of bounds.");
        return NULL_VAL;
    }
    writeValueArray(vm, &list->items, NULL_VAL);
    for (int i = list->items.count - 1; i > index; i--) {
        list->items.values[i] = list->items.values[i - 1];
    }
    list->items.values[index] = args[1];
    writeBarrier(vm, (Obj*) list, args[1]);
    return receiver;
}

static Value listRemove(VM* vm, Value receiver, int argCount, Value* args) {
    (void) argCount;
    ObjList* list = AS_LIST(receiver);
    if (!IS_NUMBER(args[0])) {
        runtimeError(vm, "Index must be a number.");
        return NULL_VAL;
    }
    int index = (int) AS_NUMBER(args[0]);
    if (index < 0 || index >= list->items.count) {
        runtimeError(vm, "Index out of bounds.");
        return NULL_VAL;
    }
    Value val = list->items.values[index];
    for (int i = index; i < list->items.count - 1; i++) {
        list->items.values[i] = list->items.values[i + 1];
    }
    list->items.count--;
    return val;
}

static Value listClear(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) argCount; (void) args;
    ObjList* list = AS_LIST(receiver);
    list->items.count = 0;
    return receiver;
}

static Value listIndexOf(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) argCount;
    ObjList* list = AS_LIST(receiver);
    for (int i = 0; i < list->items.count; i++) {
        if (valuesEqual(list->items.values[i], args[0])) {
            return NUMBER_VAL(i);
        }
    }
    return NUMBER_VAL(-1);
}

static Value listContains(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) argCount;
    ObjList* list = AS_LIST(receiver);
    for (int i = 0; i < list->items.count; i++) {
        if (valuesEqual(list->items.values[i], args[0])) {
            return TRUE_VAL;
        }
    }
    return FALSE_VAL;
}

static Value listReverse(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) argCount; (void) args;
    ObjList* list = AS_LIST(receiver);
    int left = 0;
    int right = list->items.count - 1;
    while (left < right) {
        Value temp = list->items.values[left];
        list->items.values[left] = list->items.values[right];
        list->items.values[right] = temp;
        left++;
        right--;
    }
    return receiver;
}

static Value listSlice(VM* vm, Value receiver, int argCount, Value* args) {
    ObjList* list = AS_LIST(receiver);
    if (!IS_NUMBER(args[0])) {
        runtimeError(vm, "Start index must be a number.");
        return NULL_VAL;
    }
    int start = (int) AS_NUMBER(args[0]);
    int end = list->items.count;
    if (argCount >= 2 && IS_NUMBER(args[1])) {
        end = (int) AS_NUMBER(args[1]);
    }
    if (start < 0) start = list->items.count + start;
    if (end < 0) end = list->items.count + end;
    if (start < 0) start = 0;
    if (end > list->items.count) end = list->items.count;
    if (start >= end) {
        return OBJ_VAL(newList(vm));
    }
    ObjList* result = newList(vm);
    push(vm, OBJ_VAL(result));
    for (int i = start; i < end; i++) {
        writeValueArray(vm, &result->items, list->items.values[i]);
        writeBarrier(vm, (Obj*) result, list->items.values[i]);  // Fixed: was 'list'
    }
    pop(vm);
    return OBJ_VAL(result);
}

static Value listJoin(VM* vm, Value receiver, int argCount, Value* args) {
    (void) argCount;
    ObjList* list = AS_LIST(receiver);
    if (!IS_STRING(args[0])) {
        runtimeError(vm, "Separator must be a string.");
        return NULL_VAL;
    }
    ObjString* sep = AS_STRING(args[0]);

    if (list->items.count == 0) {
        return OBJ_VAL(copyString(vm, "", 0));
    }

    int totalLen = 0;
    for (int i = 0; i < list->items.count; i++) {
        if (!IS_STRING(list->items.values[i])) {
            runtimeError(vm, "All list elements must be strings.");
            return NULL_VAL;
        }
        totalLen += AS_STRING(list->items.values[i])->length;
    }
    totalLen += sep->length * (list->items.count - 1);

    char* result = ALLOCATE(vm, char, totalLen + 1);
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

    return OBJ_VAL(takeString(vm, result, totalLen));
}

static Value listClone(VM* vm, Value receiver, int argCount, Value* args) {
    (void) argCount; (void) args;
    ObjList* list = AS_LIST(receiver);
    ObjList* result = newList(vm);
    push(vm, OBJ_VAL(result));
    for (int i = 0; i < list->items.count; i++) {
        writeValueArray(vm, &result->items, list->items.values[i]);
        writeBarrier(vm, (Obj*) result, list->items.values[i]);  // Fixed: was 'list'
    }
    pop(vm);
    return OBJ_VAL(result);
}

void initListClass(VM* vm) {
    vm->listClass = newNativeClass(vm, "List");
    defineNativeMethod(vm, vm->listClass, "length", listLength, 0);
    defineNativeMethod(vm, vm->listClass, "push", listPush, 1);
    defineNativeMethod(vm, vm->listClass, "pop", listPop, 0);
    defineNativeMethod(vm, vm->listClass, "shift", listShift, 0);
    defineNativeMethod(vm, vm->listClass, "unshift", listUnshift, 1);
    defineNativeMethod(vm, vm->listClass, "insert", listInsert, 2);
    defineNativeMethod(vm, vm->listClass, "remove", listRemove, 1);
    defineNativeMethod(vm, vm->listClass, "clear", listClear, 0);
    defineNativeMethod(vm, vm->listClass, "indexOf", listIndexOf, 1);
    defineNativeMethod(vm, vm->listClass, "contains", listContains, 1);
    defineNativeMethod(vm, vm->listClass, "reverse", listReverse, 0);
    defineNativeMethod(vm, vm->listClass, "slice", listSlice, -1);
    defineNativeMethod(vm, vm->listClass, "join", listJoin, 1);
    defineNativeMethod(vm, vm->listClass, "clone", listClone, 0);
}