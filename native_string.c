#include <string.h>
#include <ctype.h>
#include "native_string.h"
#include "object.h"
#include "memory.h"

static Value stringLength(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) argCount; (void) args;
    ObjString* str = AS_STRING(receiver);
    return NUMBER_VAL(str->length);
}

static Value stringUpper(VM* vm, Value receiver, int argCount, Value* args) {
    (void) argCount; (void) args;
    ObjString* str = AS_STRING(receiver);
    char* upper = ALLOCATE(vm, char, str->length + 1);
    for (int i = 0; i < str->length; i++) {
        upper[i] = toupper((unsigned char) str->chars[i]);
    }
    upper[str->length] = '\0';
    return OBJ_VAL(takeString(vm, upper, str->length));
}

static Value stringLower(VM* vm, Value receiver, int argCount, Value* args) {
    (void) argCount; (void) args;
    ObjString* str = AS_STRING(receiver);
    char* lower = ALLOCATE(vm, char, str->length + 1);
    for (int i = 0; i < str->length; i++) {
        lower[i] = tolower((unsigned char) str->chars[i]);
    }
    lower[str->length] = '\0';
    return OBJ_VAL(takeString(vm, lower, str->length));
}

static Value stringTrim(VM* vm, Value receiver, int argCount, Value* args) {
    (void) argCount; (void) args;
    ObjString* str = AS_STRING(receiver);
    int start = 0;
    int end = str->length - 1;
    while (start <= end && isspace((unsigned char) str->chars[start])) start++;
    while (end >= start && isspace((unsigned char) str->chars[end])) end--;
    int len = end - start + 1;
    if (len <= 0) return OBJ_VAL(copyString(vm, "", 0));
    return OBJ_VAL(copyString(vm, str->chars + start, len));
}

static Value stringContains(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) argCount;
    ObjString* str = AS_STRING(receiver);
    if (!IS_STRING(args[0])) {
        runtimeError(vm, "Argument must be a string.");
        return FALSE_VAL;
    }
    ObjString* needle = AS_STRING(args[0]);
    return BOOL_VAL(strstr(str->chars, needle->chars) != NULL);
}

static Value stringStartsWith(VM* vm, Value receiver, int argCount, Value* args) {
    (void) argCount;
    ObjString* str = AS_STRING(receiver);
    if (!IS_STRING(args[0])) {
        runtimeError(vm, "Argument must be a string.");
        return FALSE_VAL;
    }
    ObjString* prefix = AS_STRING(args[0]);
    if (prefix->length > str->length) return FALSE_VAL;
    return BOOL_VAL(memcmp(str->chars, prefix->chars, prefix->length) == 0);
}

static Value stringEndsWith(VM* vm, Value receiver, int argCount, Value* args) {
    (void) argCount;
    ObjString* str = AS_STRING(receiver);
    if (!IS_STRING(args[0])) {
        runtimeError(vm, "Argument must be a string.");
        return FALSE_VAL;
    }
    ObjString* suffix = AS_STRING(args[0]);
    if (suffix->length > str->length) return FALSE_VAL;
    return BOOL_VAL(memcmp(str->chars + str->length - suffix->length, suffix->chars, suffix->length) == 0);
}

static Value stringIndexOf(VM* vm, Value receiver, int argCount, Value* args) {
    (void) argCount;
    ObjString* str = AS_STRING(receiver);
    if (!IS_STRING(args[0])) {
        runtimeError(vm, "Argument must be a string.");
        return NUMBER_VAL(-1);
    }
    ObjString* needle = AS_STRING(args[0]);
    char* found = strstr(str->chars, needle->chars);
    if (found == NULL) return NUMBER_VAL(-1);
    return NUMBER_VAL(found - str->chars);
}

static Value stringSubstring(VM* vm, Value receiver, int argCount, Value* args) {
    ObjString* str = AS_STRING(receiver);
    if (!IS_NUMBER(args[0])) {
        runtimeError(vm, "Start index must be a number.");
        return NULL_VAL;
    }
    int start = (int) AS_NUMBER(args[0]);
    int end = str->length;
    if (argCount >= 2 && IS_NUMBER(args[1])) {
        end = (int) AS_NUMBER(args[1]);
    }
    if (start < 0) start = 0;
    if (end > str->length) end = str->length;
    if (start >= end) return OBJ_VAL(copyString(vm, "", 0));
    return OBJ_VAL(copyString(vm, str->chars + start, end - start));
}

static Value stringSplit(VM* vm, Value receiver, int argCount, Value* args) {
    (void) argCount;
    ObjString* str = AS_STRING(receiver);
    if (!IS_STRING(args[0])) {
        runtimeError(vm, "Delimiter must be a string.");
        return NULL_VAL;
    }
    ObjString* delim = AS_STRING(args[0]);
    ObjList* list = newList(vm);
    push(vm, OBJ_VAL(list));

    if (delim->length == 0) {
        for (int i = 0; i < str->length; i++) {
            ObjString* ch = copyString(vm, str->chars + i, 1);
            writeValueArray(vm, &list->items, OBJ_VAL(ch));
        }
    } else {
        char* start = str->chars;
        char* found;
        while ((found = strstr(start, delim->chars)) != NULL) {
            ObjString* part = copyString(vm, start, (int) (found - start));
            writeValueArray(vm, &list->items, OBJ_VAL(part));
            start = found + delim->length;
        }
        ObjString* part = copyString(vm, start, (int) (str->chars + str->length - start));
        writeValueArray(vm, &list->items, OBJ_VAL(part));
    }

    pop(vm);
    return OBJ_VAL(list);
}

static Value stringReplace(VM* vm, Value receiver, int argCount, Value* args) {
    (void) argCount;
    ObjString* str = AS_STRING(receiver);
    if (!IS_STRING(args[0]) || !IS_STRING(args[1])) {
        runtimeError(vm, "Arguments must be strings.");
        return NULL_VAL;
    }
    ObjString* from = AS_STRING(args[0]);
    ObjString* to = AS_STRING(args[1]);

    if (from->length == 0) return receiver;

    int count = 0;
    char* p = str->chars;
    while ((p = strstr(p, from->chars)) != NULL) {
        count++;
        p += from->length;
    }

    if (count == 0) return receiver;

    int newLen = str->length + count * (to->length - from->length);
    char* result = ALLOCATE(vm, char, newLen + 1);
    char* dst = result;
    char* src = str->chars;

    while ((p = strstr(src, from->chars)) != NULL) {
        int len = (int) (p - src);
        memcpy(dst, src, len);
        dst += len;
        memcpy(dst, to->chars, to->length);
        dst += to->length;
        src = p + from->length;
    }
    strcpy(dst, src);

    return OBJ_VAL(takeString(vm, result, newLen));
}

void initStringClass(VM* vm) {
    vm->stringClass = newNativeClass(vm, "String");
    defineNativeMethod(vm, vm->stringClass, "length", stringLength, 0);
    defineNativeMethod(vm, vm->stringClass, "upper", stringUpper, 0);
    defineNativeMethod(vm, vm->stringClass, "lower", stringLower, 0);
    defineNativeMethod(vm, vm->stringClass, "trim", stringTrim, 0);
    defineNativeMethod(vm, vm->stringClass, "contains", stringContains, 1);
    defineNativeMethod(vm, vm->stringClass, "startsWith", stringStartsWith, 1);
    defineNativeMethod(vm, vm->stringClass, "endsWith", stringEndsWith, 1);
    defineNativeMethod(vm, vm->stringClass, "indexOf", stringIndexOf, 1);
    defineNativeMethod(vm, vm->stringClass, "substring", stringSubstring, -1);
    defineNativeMethod(vm, vm->stringClass, "split", stringSplit, 1);
    defineNativeMethod(vm, vm->stringClass, "replace", stringReplace, 2);
}