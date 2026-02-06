// ============================================================================
// native_string.c - BTL String Native Methods
//
// Implements built-in methods for string objects including case conversion,
// trimming, searching, substring extraction, splitting, and replacement.
// ============================================================================

#include <string.h>
#include <ctype.h>
#include "native_string.h"
#include "object.h"
#include "memory.h"

// ----------------------------------------------------------------------------
// String Methods
// ----------------------------------------------------------------------------

// length() - returns the number of characters in the string
static BtlValue stringLength(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) vm; (void) argCount; (void) args;
    ObjString* str = AS_STRING(receiver);
    return NUMBER_VAL(str->length);
}

// upper() - returns uppercase version of string
static BtlValue stringUpper(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount; (void) args;
    ObjString* str = AS_STRING(receiver);
    char* upper = BTL_ALLOCATE(vm, char, str->length + 1);
    for (int i = 0; i < str->length; i++) {
        upper[i] = toupper((unsigned char) str->chars[i]);
    }
    upper[str->length] = '\0';
    return OBJ_VAL(btl_string_take(vm, upper, str->length));
}

// lower() - returns lowercase version of string
static BtlValue stringLower(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount; (void) args;
    ObjString* str = AS_STRING(receiver);
    char* lower = BTL_ALLOCATE(vm, char, str->length + 1);
    for (int i = 0; i < str->length; i++) {
        lower[i] = tolower((unsigned char) str->chars[i]);
    }
    lower[str->length] = '\0';
    return OBJ_VAL(btl_string_take(vm, lower, str->length));
}

// trim() - returns string with leading/trailing whitespace removed
static BtlValue stringTrim(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount; (void) args;
    ObjString* str = AS_STRING(receiver);
    int start = 0;
    int end = str->length - 1;
    while (start <= end && isspace((unsigned char) str->chars[start])) start++;
    while (end >= start && isspace((unsigned char) str->chars[end])) end--;
    int len = end - start + 1;
    if (len <= 0) return OBJ_VAL(btl_string_copy(vm, "", 0));
    return OBJ_VAL(btl_string_copy(vm, str->chars + start, len));
}

// contains(needle) - returns true if string contains needle
static BtlValue stringContains(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) vm; (void) argCount;
    ObjString* str = AS_STRING(receiver);
    if (!IS_STRING(args[0])) {
        btl_runtime_error(vm, "Argument must be a string.");
        return BTL_FALSE_VAL;
    }
    ObjString* needle = AS_STRING(args[0]);
    return BOOL_VAL(strstr(str->chars, needle->chars) != NULL);
}

// startsWith(prefix) - returns true if string starts with prefix
static BtlValue stringStartsWith(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount;
    ObjString* str = AS_STRING(receiver);
    if (!IS_STRING(args[0])) {
        btl_runtime_error(vm, "Argument must be a string.");
        return BTL_FALSE_VAL;
    }
    ObjString* prefix = AS_STRING(args[0]);
    if (prefix->length > str->length) return BTL_FALSE_VAL;
    return BOOL_VAL(memcmp(str->chars, prefix->chars, prefix->length) == 0);
}

// endsWith(suffix) - returns true if string ends with suffix
static BtlValue stringEndsWith(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount;
    ObjString* str = AS_STRING(receiver);
    if (!IS_STRING(args[0])) {
        btl_runtime_error(vm, "Argument must be a string.");
        return BTL_FALSE_VAL;
    }
    ObjString* suffix = AS_STRING(args[0]);
    if (suffix->length > str->length) return BTL_FALSE_VAL;
    return BOOL_VAL(memcmp(str->chars + str->length - suffix->length, suffix->chars, suffix->length) == 0);
}

// indexOf(needle) - returns index of first occurrence, or -1 if not found
static BtlValue stringIndexOf(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount;
    ObjString* str = AS_STRING(receiver);
    if (!IS_STRING(args[0])) {
        btl_runtime_error(vm, "Argument must be a string.");
        return NUMBER_VAL(-1);
    }
    ObjString* needle = AS_STRING(args[0]);
    char* found = strstr(str->chars, needle->chars);
    if (found == NULL) return NUMBER_VAL(-1);
    return NUMBER_VAL(found - str->chars);
}

// substring(start, end?) - returns substring from start to end (exclusive)
static BtlValue stringSubstring(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    ObjString* str = AS_STRING(receiver);
    if (!IS_NUMBER(args[0])) {
        btl_runtime_error(vm, "Start index must be a number.");
        return BTL_NULL_VAL;
    }
    int start = (int) AS_NUMBER(args[0]);
    int end = str->length;
    if (argCount >= 2 && IS_NUMBER(args[1])) {
        end = (int) AS_NUMBER(args[1]);
    }
    if (start < 0) start = 0;
    if (end > str->length) end = str->length;
    if (start >= end) return OBJ_VAL(btl_string_copy(vm, "", 0));
    return OBJ_VAL(btl_string_copy(vm, str->chars + start, end - start));
}

// split(delimiter) - splits string by delimiter, returns list of parts
static BtlValue stringSplit(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount;
    ObjString* str = AS_STRING(receiver);
    if (!IS_STRING(args[0])) {
        btl_runtime_error(vm, "Delimiter must be a string.");
        return BTL_NULL_VAL;
    }
    ObjString* delim = AS_STRING(args[0]);
    ObjList* list = btl_list_new(vm);
    btl_push(vm, OBJ_VAL(list));

    if (delim->length == 0) {
        // Empty delimiter: split into individual characters
        for (int i = 0; i < str->length; i++) {
            ObjString* ch = btl_string_copy(vm, str->chars + i, 1);
            btl_value_array_write(vm, &list->items, OBJ_VAL(ch));
        }
    } else {
        // Split by delimiter
        char* start = str->chars;
        char* found;
        while ((found = strstr(start, delim->chars)) != NULL) {
            ObjString* part = btl_string_copy(vm, start, (int) (found - start));
            btl_value_array_write(vm, &list->items, OBJ_VAL(part));
            start = found + delim->length;
        }
        ObjString* part = btl_string_copy(vm, start, (int) (str->chars + str->length - start));
        btl_value_array_write(vm, &list->items, OBJ_VAL(part));
    }

    btl_pop(vm);
    return OBJ_VAL(list);
}

// replace(from, to) - replaces all occurrences of 'from' with 'to'
static BtlValue stringReplace(VM* vm, BtlValue receiver, int argCount, BtlValue* args) {
    (void) argCount;
    ObjString* str = AS_STRING(receiver);
    if (!IS_STRING(args[0]) || !IS_STRING(args[1])) {
        btl_runtime_error(vm, "Arguments must be strings.");
        return BTL_NULL_VAL;
    }
    ObjString* from = AS_STRING(args[0]);
    ObjString* to = AS_STRING(args[1]);

    if (from->length == 0) return receiver;

    // Count occurrences
    int count = 0;
    char* p = str->chars;
    while ((p = strstr(p, from->chars)) != NULL) {
        count++;
        p += from->length;
    }

    if (count == 0) return receiver;

    // Allocate result buffer
    int newLen = str->length + count * (to->length - from->length);
    char* result = BTL_ALLOCATE(vm, char, newLen + 1);
    char* dst = result;
    char* src = str->chars;

    // Copy with replacements
    while ((p = strstr(src, from->chars)) != NULL) {
        int len = (int) (p - src);
        memcpy(dst, src, len);
        dst += len;
        memcpy(dst, to->chars, to->length);
        dst += to->length;
        src = p + from->length;
    }
    strcpy(dst, src);

    return OBJ_VAL(btl_string_take(vm, result, newLen));
}

// ----------------------------------------------------------------------------
// String Class Initialization
// ----------------------------------------------------------------------------

void btl_string_class_init(VM* vm) {
    vm->stringClass = btl_native_class_new(vm, "String");
    btl_native_class_add_method(vm, vm->stringClass, "length", stringLength, 0);
    btl_native_class_add_method(vm, vm->stringClass, "upper", stringUpper, 0);
    btl_native_class_add_method(vm, vm->stringClass, "lower", stringLower, 0);
    btl_native_class_add_method(vm, vm->stringClass, "trim", stringTrim, 0);
    btl_native_class_add_method(vm, vm->stringClass, "contains", stringContains, 1);
    btl_native_class_add_method(vm, vm->stringClass, "startsWith", stringStartsWith, 1);
    btl_native_class_add_method(vm, vm->stringClass, "endsWith", stringEndsWith, 1);
    btl_native_class_add_method(vm, vm->stringClass, "indexOf", stringIndexOf, 1);
    btl_native_class_add_method(vm, vm->stringClass, "substring", stringSubstring, -1);
    btl_native_class_add_method(vm, vm->stringClass, "split", stringSplit, 1);
    btl_native_class_add_method(vm, vm->stringClass, "replace", stringReplace, 2);
}