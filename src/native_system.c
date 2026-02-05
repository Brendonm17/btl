// ============================================================================
// NATIVE_SYSTEM.C - Updated for callback routing
// ============================================================================

#define _POSIX_C_SOURCE 199309L

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <unistd.h>
#endif

#include "vm.h"
#include "object.h"
#include "memory.h"

// Store command line args
static int sysArgc = 0;
static const char** sysArgv = NULL;

void setSystemArgs(int argc, const char* argv []) {
    sysArgc = argc;
    sysArgv = argv;
}

// ============================================================================
// Print functions - use btl_print for output
// ============================================================================

static Value printNative(VM* vm, Value receiver, int argCount, Value* args) {
    (void) receiver;
    for (int i = 0; i < argCount; i++) {
        btl_print_value(vm, args[i]);
        if (i < argCount - 1) btl_print(vm, " ");
    }
    return NULL_VAL;
}

static Value printlnNative(VM* vm, Value receiver, int argCount, Value* args) {
    (void) receiver;
    for (int i = 0; i < argCount; i++) {
        btl_print_value(vm, args[i]);
        if (i < argCount - 1) btl_print(vm, " ");
    }
    btl_print(vm, "\n");
    return NULL_VAL;
}

static Value inputNative(VM* vm, Value receiver, int argCount, Value* args) {
    (void) receiver;

    if (argCount > 0 && IS_STRING(args[0])) {
        btl_print(vm, AS_CSTRING(args[0]));
    }

    char buffer[1024];
    if (fgets(buffer, sizeof(buffer), stdin) != NULL) {
        size_t len = strlen(buffer);
        if (len > 0 && buffer[len - 1] == '\n') {
            buffer[len - 1] = '\0';
            len--;
        }
        return OBJ_VAL(copyString(vm, buffer, (int) len));
    }

    return NULL_VAL;
}

// ============================================================================
// Time functions
// ============================================================================

static Value clockNative(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) receiver; (void) argCount; (void) args;
    return NUMBER_VAL((double) clock() / CLOCKS_PER_SEC);
}

static Value timeNative(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) receiver; (void) argCount; (void) args;
    return NUMBER_VAL((double) time(NULL));
}

static Value sleepNative(VM* vm, Value receiver, int argCount, Value* args) {
    (void) receiver;
    if (argCount < 1 || !IS_NUMBER(args[0])) {
        btl_error(vm, "sleep() requires a number argument.\n");
        return NULL_VAL;
    }

    double seconds = AS_NUMBER(args[0]);

#ifdef _WIN32
    Sleep((DWORD) (seconds * 1000));
#else
    struct timespec ts;
    ts.tv_sec = (time_t) seconds;
    ts.tv_nsec = (long) ((seconds - ts.tv_sec) * 1e9);
    nanosleep(&ts, NULL);
#endif

    return NULL_VAL;
}

// ============================================================================
// Type checking functions
// ============================================================================

static Value typeofNative(VM* vm, Value receiver, int argCount, Value* args) {
    (void) receiver;
    if (argCount < 1) return OBJ_VAL(copyString(vm, "null", 4));

    Value value = args[0];

    if (IS_BOOL(value)) return OBJ_VAL(copyString(vm, "bool", 4));
    if (IS_NULL(value)) return OBJ_VAL(copyString(vm, "null", 4));
    if (IS_NUMBER(value)) return OBJ_VAL(copyString(vm, "number", 6));
    if (IS_STRING(value)) return OBJ_VAL(copyString(vm, "string", 6));
    if (IS_LIST(value)) return OBJ_VAL(copyString(vm, "list", 4));
    if (IS_TABLE(value)) return OBJ_VAL(copyString(vm, "table", 5));
    if (IS_FUNCTION(value) || IS_CLOSURE(value) || IS_NATIVE(value))
        return OBJ_VAL(copyString(vm, "function", 8));
    if (IS_CLASS(value)) return OBJ_VAL(copyString(vm, "class", 5));
    if (IS_INSTANCE(value)) return OBJ_VAL(copyString(vm, "instance", 8));
    if (IS_FUTURE(value)) return OBJ_VAL(copyString(vm, "future", 6));
    if (IS_ACTOR(value)) return OBJ_VAL(copyString(vm, "actor", 5));

    return OBJ_VAL(copyString(vm, "object", 6));
}

static Value isNumberNative(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) receiver;
    if (argCount < 1) return BOOL_VAL(false);
    return BOOL_VAL(IS_NUMBER(args[0]));
}

static Value isStringNative(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) receiver;
    if (argCount < 1) return BOOL_VAL(false);
    return BOOL_VAL(IS_STRING(args[0]));
}

static Value isBoolNative(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) receiver;
    if (argCount < 1) return BOOL_VAL(false);
    return BOOL_VAL(IS_BOOL(args[0]));
}

static Value isListNative(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) receiver;
    if (argCount < 1) return BOOL_VAL(false);
    return BOOL_VAL(IS_LIST(args[0]));
}

static Value isTableNative(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) receiver;
    if (argCount < 1) return BOOL_VAL(false);
    return BOOL_VAL(IS_TABLE(args[0]));
}

static Value isFunctionNative(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) receiver;
    if (argCount < 1) return BOOL_VAL(false);
    Value v = args[0];
    return BOOL_VAL(IS_FUNCTION(v) || IS_CLOSURE(v) || IS_NATIVE(v) || IS_BOUND_METHOD(v));
}

static Value isNullNative(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) receiver;
    if (argCount < 1) return BOOL_VAL(true);
    return BOOL_VAL(IS_NULL(args[0]));
}

// ============================================================================
// Conversion functions
// ============================================================================

static Value toStringNative(VM* vm, Value receiver, int argCount, Value* args) {
    (void) receiver;
    if (argCount < 1) return OBJ_VAL(copyString(vm, "", 0));

    Value value = args[0];

    if (IS_STRING(value)) return value;

    char buffer[64];
    int len = 0;

    if (IS_NUMBER(value)) {
        len = snprintf(buffer, sizeof(buffer), "%g", AS_NUMBER(value));
    } else if (IS_BOOL(value)) {
        const char* str = AS_BOOL(value) ? "true" : "false";
        return OBJ_VAL(copyString(vm, str, (int) strlen(str)));
    } else if (IS_NULL(value)) {
        return OBJ_VAL(copyString(vm, "null", 4));
    } else {
        return OBJ_VAL(copyString(vm, "<object>", 8));
    }

    return OBJ_VAL(copyString(vm, buffer, len));
}

static Value toNumberNative(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) receiver;
    if (argCount < 1) return NUMBER_VAL(0);

    Value value = args[0];

    if (IS_NUMBER(value)) return value;
    if (IS_BOOL(value)) return NUMBER_VAL(AS_BOOL(value) ? 1 : 0);
    if (IS_STRING(value)) {
        char* end;
        double num = strtod(AS_CSTRING(value), &end);
        if (end == AS_CSTRING(value)) return NULL_VAL;
        return NUMBER_VAL(num);
    }

    return NULL_VAL;
}

// ============================================================================
// File I/O functions
// ============================================================================

static Value readFileNative(VM* vm, Value receiver, int argCount, Value* args) {
    (void) receiver;
    if (argCount < 1 || !IS_STRING(args[0])) {
        btl_error(vm, "readFile() requires a string path.\n");
        return NULL_VAL;
    }

    const char* path = AS_CSTRING(args[0]);
    FILE* file = fopen(path, "rb");
    if (file == NULL) {
        return NULL_VAL;
    }

    fseek(file, 0, SEEK_END);
    size_t size = ftell(file);
    rewind(file);

    char* buffer = btl_realloc(vm, NULL, 0, size + 1);
    if (buffer == NULL) {
        fclose(file);
        return NULL_VAL;
    }

    size_t bytesRead = fread(buffer, 1, size, file);
    fclose(file);

    buffer[bytesRead] = '\0';

    ObjString* result = copyString(vm, buffer, (int) bytesRead);

    btl_realloc(vm, buffer, size + 1, 0);

    return OBJ_VAL(result);
}

static Value writeFileNative(VM* vm, Value receiver, int argCount, Value* args) {
    (void) receiver;
    if (argCount < 2 || !IS_STRING(args[0]) || !IS_STRING(args[1])) {
        btl_error(vm, "writeFile() requires path and content strings.\n");
        return BOOL_VAL(false);
    }

    const char* path = AS_CSTRING(args[0]);
    ObjString* content = AS_STRING(args[1]);

    FILE* file = fopen(path, "wb");
    if (file == NULL) {
        return BOOL_VAL(false);
    }

    size_t written = fwrite(content->chars, 1, content->length, file);
    fclose(file);

    return BOOL_VAL(written == (size_t) content->length);
}

static Value appendFileNative(VM* vm, Value receiver, int argCount, Value* args) {
    (void) receiver;
    if (argCount < 2 || !IS_STRING(args[0]) || !IS_STRING(args[1])) {
        btl_error(vm, "appendFile() requires path and content strings.\n");
        return BOOL_VAL(false);
    }

    const char* path = AS_CSTRING(args[0]);
    ObjString* content = AS_STRING(args[1]);

    FILE* file = fopen(path, "ab");
    if (file == NULL) {
        return BOOL_VAL(false);
    }

    size_t written = fwrite(content->chars, 1, content->length, file);
    fclose(file);

    return BOOL_VAL(written == (size_t) content->length);
}

static Value fileExistsNative(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) receiver;
    if (argCount < 1 || !IS_STRING(args[0])) {
        return BOOL_VAL(false);
    }

    FILE* file = fopen(AS_CSTRING(args[0]), "r");
    if (file != NULL) {
        fclose(file);
        return BOOL_VAL(true);
    }
    return BOOL_VAL(false);
}

// ============================================================================
// System functions
// ============================================================================

static Value exitNative(VM* vm, Value receiver, int argCount, Value* args) {
    (void) vm; (void) receiver;
    int code = 0;
    if (argCount > 0 && IS_NUMBER(args[0])) {
        code = (int) AS_NUMBER(args[0]);
    }
    exit(code);
    return NULL_VAL;
}

static Value getenvNative(VM* vm, Value receiver, int argCount, Value* args) {
    (void) receiver;
    if (argCount < 1 || !IS_STRING(args[0])) {
        return NULL_VAL;
    }

    const char* value = getenv(AS_CSTRING(args[0]));
    if (value == NULL) {
        return NULL_VAL;
    }

    return OBJ_VAL(copyString(vm, value, (int) strlen(value)));
}

static Value argsNative(VM* vm, Value receiver, int argCount, Value* args) {
    (void) receiver; (void) argCount; (void) args;

    ObjList* list = newList(vm);
    push(vm, OBJ_VAL(list));

    for (int i = 0; i < sysArgc; i++) {
        Value arg = OBJ_VAL(copyString(vm, sysArgv[i], (int) strlen(sysArgv[i])));
        writeValueArray(vm, &list->items, arg);
    }

    pop(vm);
    return OBJ_VAL(list);
}

// ============================================================================
// Assert function
// ============================================================================

static Value assertNative(VM* vm, Value receiver, int argCount, Value* args) {
    (void) receiver;
    if (argCount < 1) {
        btl_error(vm, "Assertion failed: no condition provided\n");
        exit(1);
    }

    if (IS_BOOL(args[0]) && !AS_BOOL(args[0])) {
        if (argCount >= 2 && IS_STRING(args[1])) {
            btl_errorf(vm, "Assertion failed: %s\n", AS_CSTRING(args[1]));
        } else {
            btl_error(vm, "Assertion failed\n");
        }
        exit(1);
    }

    if (IS_NULL(args[0])) {
        if (argCount >= 2 && IS_STRING(args[1])) {
            btl_errorf(vm, "Assertion failed: %s\n", AS_CSTRING(args[1]));
        } else {
            btl_error(vm, "Assertion failed: value was null\n");
        }
        exit(1);
    }

    return NULL_VAL;
}

// ============================================================================
// Module initialization
// ============================================================================

static void defineModuleFunction(VM* vm, ObjNativeModule* module, const char* name,
    NativeMethodFn function, int arity) {
    ObjString* nameStr = copyString(vm, name, (int) strlen(name));
    push(vm, OBJ_VAL(nameStr));

    // Fixed: correct argument order (fn, name, arity) and use const char*
    ObjNativeMethod* method = newNativeMethod(vm, function, name, arity);
    push(vm, OBJ_VAL(method));

    tableSet(vm, &module->globals, OBJ_VAL(nameStr), OBJ_VAL(method));

    pop(vm);
    pop(vm);
}

void initSystemModule(VM* vm) {
    ObjString* name = copyString(vm, "system", 6);
    push(vm, OBJ_VAL(name));

    // Fixed: use name->chars (const char*) instead of name (ObjString*)
    ObjNativeModule* module = newNativeModule(vm, name->chars);
    push(vm, OBJ_VAL(module));

    // I/O
    defineModuleFunction(vm, module, "print", printNative, -1);
    defineModuleFunction(vm, module, "println", printlnNative, -1);
    defineModuleFunction(vm, module, "input", inputNative, -1);

    // Time
    defineModuleFunction(vm, module, "clock", clockNative, 0);
    defineModuleFunction(vm, module, "time", timeNative, 0);
    defineModuleFunction(vm, module, "sleep", sleepNative, 1);

    // Type checking
    defineModuleFunction(vm, module, "typeof", typeofNative, 1);
    defineModuleFunction(vm, module, "isNumber", isNumberNative, 1);
    defineModuleFunction(vm, module, "isString", isStringNative, 1);
    defineModuleFunction(vm, module, "isBool", isBoolNative, 1);
    defineModuleFunction(vm, module, "isList", isListNative, 1);
    defineModuleFunction(vm, module, "isTable", isTableNative, 1);
    defineModuleFunction(vm, module, "isFunction", isFunctionNative, 1);
    defineModuleFunction(vm, module, "isNull", isNullNative, 1);

    // Conversion
    defineModuleFunction(vm, module, "toString", toStringNative, 1);
    defineModuleFunction(vm, module, "toNumber", toNumberNative, 1);

    // File I/O
    defineModuleFunction(vm, module, "readFile", readFileNative, 1);
    defineModuleFunction(vm, module, "writeFile", writeFileNative, 2);
    defineModuleFunction(vm, module, "appendFile", appendFileNative, 2);
    defineModuleFunction(vm, module, "fileExists", fileExistsNative, 1);

    // System
    defineModuleFunction(vm, module, "exit", exitNative, -1);
    defineModuleFunction(vm, module, "getenv", getenvNative, 1);
    defineModuleFunction(vm, module, "args", argsNative, 0);
    defineModuleFunction(vm, module, "assert", assertNative, -1);

    tableSet(vm, &vm->nativeModules, OBJ_VAL(name), OBJ_VAL(module));

    pop(vm);
    pop(vm);
}