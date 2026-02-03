#define _POSIX_C_SOURCE 199309L
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "native_system.h"
#include "object.h"
#include "memory.h"

// clock() - returns time in seconds since program start
static Value systemClock(int argCount, Value* args) {
    (void) argCount; (void) args;
    return NUMBER_VAL((double) clock() / CLOCKS_PER_SEC);
}

// time() - returns Unix timestamp in seconds
static Value systemTime(int argCount, Value* args) {
    (void) argCount; (void) args;
    return NUMBER_VAL((double) time(NULL));
}

// millis() - returns milliseconds since epoch
static Value systemMillis(int argCount, Value* args) {
    (void) argCount; (void) args;
#if defined(_POSIX_TIMERS) && _POSIX_TIMERS > 0
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    return NUMBER_VAL((double) ts.tv_sec * 1000.0 + (double) ts.tv_nsec / 1e6);
#else
    return NUMBER_VAL((double) clock() / (CLOCKS_PER_SEC / 1000.0));
#endif
}

// nanos() - returns nanoseconds (for precise timing)
static Value systemNanos(int argCount, Value* args) {
    (void) argCount; (void) args;
#if defined(_POSIX_TIMERS) && _POSIX_TIMERS > 0
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return NUMBER_VAL((double) ts.tv_sec * 1e9 + (double) ts.tv_nsec);
#else
    return NUMBER_VAL((double) clock() * (1e9 / CLOCKS_PER_SEC));
#endif
}

// sleep(seconds) - pause execution
static Value systemSleep(int argCount, Value* args) {
    (void) argCount;
    double seconds = AS_NUMBER(args[0]);
    if (seconds > 0) {
#if defined(_POSIX_TIMERS) && _POSIX_TIMERS > 0
        struct timespec ts;
        ts.tv_sec = (time_t) seconds;
        ts.tv_nsec = (long) ((seconds - ts.tv_sec) * 1e9);
        nanosleep(&ts, NULL);
#elif defined(_WIN32)
        Sleep((DWORD) (seconds * 1000));
#else
        // Fallback: busy wait (not ideal)
        clock_t end = clock() + (clock_t) (seconds * CLOCKS_PER_SEC);
        while (clock() < end);
#endif
    }
    return NULL_VAL;
}

// exit(code) - exit program with code
static Value systemExit(int argCount, Value* args) {
    int code = 0;
    if (argCount > 0 && IS_NUMBER(args[0])) {
        code = (int) AS_NUMBER(args[0]);
    }
    exit(code);
    return NULL_VAL;
}

// getenv(name) - get environment variable
static Value systemGetenv(int argCount, Value* args) {
    (void) argCount;
    if (!IS_STRING(args[0])) {
        return NULL_VAL;
    }
    const char* value = getenv(AS_CSTRING(args[0]));
    if (value == NULL) {
        return NULL_VAL;
    }
    return OBJ_VAL(copyString(NULL, value, (int) strlen(value)));
}

// write(...) - print without newline
static Value systemWrite(int argCount, Value* args) {
    for (int i = 0; i < argCount; i++) {
        printValue(args[i]);
    }
    fflush(stdout);
    return NULL_VAL;
}

// println(...) - print with newline
static Value systemPrintln(int argCount, Value* args) {
    for (int i = 0; i < argCount; i++) {
        printValue(args[i]);
    }
    printf("\n");
    return NULL_VAL;
}

// input(prompt) - read line from stdin
static Value systemInput(int argCount, Value* args) {
    if (argCount > 0 && IS_STRING(args[0])) {
        printf("%s", AS_CSTRING(args[0]));
        fflush(stdout);
    }
    char buffer[1024];
    if (fgets(buffer, sizeof(buffer), stdin) == NULL) {
        return NULL_VAL;
    }
    int len = (int) strlen(buffer);
    if (len > 0 && buffer[len - 1] == '\n') {
        buffer[--len] = '\0';
    }
    return OBJ_VAL(copyString(NULL, buffer, len));
}

// argc() - get argument count
static int savedArgc = 0;
static const char** savedArgv = NULL;

void setSystemArgs(int argc, const char** argv) {
    savedArgc = argc;
    savedArgv = argv;
}

static Value systemArgc(int argCount, Value* args) {
    (void) argCount; (void) args;
    return NUMBER_VAL((double) savedArgc);
}

// argv(index) - get argument at index
static Value systemArgv(int argCount, Value* args) {
    (void) argCount;
    int index = (int) AS_NUMBER(args[0]);
    if (index < 0 || index >= savedArgc || savedArgv == NULL) {
        return NULL_VAL;
    }
    return OBJ_VAL(copyString(NULL, savedArgv[index], (int) strlen(savedArgv[index])));
}

// args() - get all arguments as a list (excluding program name)
static Value systemArgs(int argCount, Value* args) {
    (void) argCount; (void) args;
    ObjList* list = newList(NULL);
    for (int i = 1; i < savedArgc; i++) {
        writeValueArray(NULL, &list->items,
            OBJ_VAL(copyString(NULL, savedArgv[i], (int) strlen(savedArgv[i]))));
    }
    return OBJ_VAL(list);
}

// platform() - get platform name
static Value systemPlatform(int argCount, Value* args) {
    (void) argCount; (void) args;
#if defined(_WIN32)
    return OBJ_VAL(copyString(NULL, "windows", 7));
#elif defined(__APPLE__)
    return OBJ_VAL(copyString(NULL, "macos", 5));
#elif defined(__linux__)
    return OBJ_VAL(copyString(NULL, "linux", 5));
#else
    return OBJ_VAL(copyString(NULL, "unknown", 7));
#endif
}

// arch() - get architecture
static Value systemArch(int argCount, Value* args) {
    (void) argCount; (void) args;
#if defined(__x86_64__) || defined(_M_X64)
    return OBJ_VAL(copyString(NULL, "x64", 3));
#elif defined(__i386__) || defined(_M_IX86)
    return OBJ_VAL(copyString(NULL, "x86", 3));
#elif defined(__aarch64__) || defined(_M_ARM64)
    return OBJ_VAL(copyString(NULL, "arm64", 5));
#elif defined(__arm__) || defined(_M_ARM)
    return OBJ_VAL(copyString(NULL, "arm", 3));
#else
    return OBJ_VAL(copyString(NULL, "unknown", 7));
#endif
}

// version() - get language version
static Value systemVersion(int argCount, Value* args) {
    (void) argCount; (void) args;
    return OBJ_VAL(copyString(NULL, "1.0.0", 5));
}

// type(value) - get type name as string
static Value systemType(int argCount, Value* args) {
    (void) argCount;
    Value val = args[0];
    const char* type;
    if (IS_NULL(val)) type = "null";
    else if (IS_BOOL(val)) type = "bool";
    else if (IS_NUMBER(val)) type = "number";
    else if (IS_STRING(val)) type = "string";
    else if (IS_LIST(val)) type = "list";
    else if (IS_TABLE(val)) type = "table";
    else if (IS_CLOSURE(val)) type = "function";
    else if (IS_NATIVE(val)) type = "native";
    else if (IS_CLASS(val)) type = "class";
    else if (IS_INSTANCE(val)) type = "instance";
    else if (IS_BOUND_METHOD(val)) type = "method";
    else if (IS_MODULE(val)) type = "module";
    else type = "unknown";
    return OBJ_VAL(copyString(NULL, type, (int) strlen(type)));
}

// isnull(value) - check if null
static Value systemIsNull(int argCount, Value* args) {
    (void) argCount;
    return BOOL_VAL(IS_NULL(args[0]));
}

// isbool(value) - check if bool
static Value systemIsBool(int argCount, Value* args) {
    (void) argCount;
    return BOOL_VAL(IS_BOOL(args[0]));
}

// isnum(value) - check if number
static Value systemIsNum(int argCount, Value* args) {
    (void) argCount;
    return BOOL_VAL(IS_NUMBER(args[0]));
}

// isstr(value) - check if string
static Value systemIsStr(int argCount, Value* args) {
    (void) argCount;
    return BOOL_VAL(IS_STRING(args[0]));
}

// islist(value) - check if list
static Value systemIsList(int argCount, Value* args) {
    (void) argCount;
    return BOOL_VAL(IS_LIST(args[0]));
}

// istable(value) - check if table
static Value systemIsTable(int argCount, Value* args) {
    (void) argCount;
    return BOOL_VAL(IS_TABLE(args[0]));
}

// isfunc(value) - check if function
static Value systemIsFunc(int argCount, Value* args) {
    (void) argCount;
    return BOOL_VAL(IS_CLOSURE(args[0]) || IS_NATIVE(args[0]) || IS_BOUND_METHOD(args[0]));
}

// isclass(value) - check if class
static Value systemIsClass(int argCount, Value* args) {
    (void) argCount;
    return BOOL_VAL(IS_CLASS(args[0]));
}

// isinstance(value) - check if instance
static Value systemIsInstance(int argCount, Value* args) {
    (void) argCount;
    return BOOL_VAL(IS_INSTANCE(args[0]));
}

// tonum(value) - convert to number
static Value systemToNum(int argCount, Value* args) {
    (void) argCount;
    Value val = args[0];
    if (IS_NUMBER(val)) return val;
    if (IS_BOOL(val)) return NUMBER_VAL(AS_BOOL(val) ? 1.0 : 0.0);
    if (IS_STRING(val)) {
        double num = strtod(AS_CSTRING(val), NULL);
        return NUMBER_VAL(num);
    }
    return NULL_VAL;
}

// tostr(value) - convert to string
static Value systemToStr(int argCount, Value* args) {
    (void) argCount;
    Value val = args[0];
    if (IS_STRING(val)) return val;

    char buffer[64];
    int len;
    if (IS_NULL(val)) {
        return OBJ_VAL(copyString(NULL, "null", 4));
    } else if (IS_BOOL(val)) {
        const char* s = AS_BOOL(val) ? "true" : "false";
        return OBJ_VAL(copyString(NULL, s, (int) strlen(s)));
    } else if (IS_NUMBER(val)) {
        len = snprintf(buffer, sizeof(buffer), "%g", AS_NUMBER(val));
        return OBJ_VAL(copyString(NULL, buffer, len));
    }
    return OBJ_VAL(copyString(NULL, "<value>", 7));
}

// tobool(value) - convert to bool
static Value systemToBool(int argCount, Value* args) {
    (void) argCount;
    Value val = args[0];
    if (IS_BOOL(val)) return val;
    if (IS_NULL(val)) return FALSE_VAL;
    if (IS_NUMBER(val)) return BOOL_VAL(AS_NUMBER(val) != 0);
    if (IS_STRING(val)) return BOOL_VAL(AS_STRING(val)->length > 0);
    return TRUE_VAL;
}

// ord(string) - get character code of first character
static Value systemOrd(int argCount, Value* args) {
    (void) argCount;
    if (!IS_STRING(args[0])) return NULL_VAL;
    ObjString* str = AS_STRING(args[0]);
    if (str->length == 0) return NULL_VAL;
    return NUMBER_VAL((double) (unsigned char) str->chars[0]);
}

// chr(number) - convert code to character
static Value systemChr(int argCount, Value* args) {
    (void) argCount;
    if (!IS_NUMBER(args[0])) return NULL_VAL;
    int code = (int) AS_NUMBER(args[0]);
    if (code < 0 || code > 255) return NULL_VAL;
    char c = (char) code;
    return OBJ_VAL(copyString(NULL, &c, 1));
}

// assert(condition, message) - assert condition is true
static Value systemAssert(int argCount, Value* args) {
    Value cond = args[0];
    bool isFalsey = IS_NULL(cond) || (IS_BOOL(cond) && !AS_BOOL(cond));
    if (isFalsey) {
        if (argCount > 1 && IS_STRING(args[1])) {
            fprintf(stderr, "Assertion failed: %s\n", AS_CSTRING(args[1]));
        } else {
            fprintf(stderr, "Assertion failed\n");
        }
        exit(1);
    }
    return NULL_VAL;
}

// error(message) - print error and exit
static Value systemError(int argCount, Value* args) {
    if (argCount > 0 && IS_STRING(args[0])) {
        fprintf(stderr, "Error: %s\n", AS_CSTRING(args[0]));
    } else {
        fprintf(stderr, "Error\n");
    }
    exit(1);
    return NULL_VAL;
}

void initSystemModule(VM* vm) {
    ObjNativeModule* module = newNativeModule(vm, "system");
    push(vm, OBJ_VAL(module));

    // Time functions
    defineNativeModuleFn(vm, module, "clock", systemClock, 0);
    defineNativeModuleFn(vm, module, "time", systemTime, 0);
    defineNativeModuleFn(vm, module, "millis", systemMillis, 0);
    defineNativeModuleFn(vm, module, "nanos", systemNanos, 0);
    defineNativeModuleFn(vm, module, "sleep", systemSleep, 1);

    // Process functions
    defineNativeModuleFn(vm, module, "exit", systemExit, -1);
    defineNativeModuleFn(vm, module, "getenv", systemGetenv, 1);

    // I/O functions
    defineNativeModuleFn(vm, module, "write", systemWrite, -1);
    defineNativeModuleFn(vm, module, "println", systemPrintln, -1);
    defineNativeModuleFn(vm, module, "input", systemInput, -1);

    // Arguments
    defineNativeModuleFn(vm, module, "argc", systemArgc, 0);
    defineNativeModuleFn(vm, module, "argv", systemArgv, 1);
    defineNativeModuleFn(vm, module, "args", systemArgs, 0);

    // System info
    defineNativeModuleFn(vm, module, "platform", systemPlatform, 0);
    defineNativeModuleFn(vm, module, "arch", systemArch, 0);
    defineNativeModuleFn(vm, module, "version", systemVersion, 0);

    // Type checking
    defineNativeModuleFn(vm, module, "type", systemType, 1);
    defineNativeModuleFn(vm, module, "isnull", systemIsNull, 1);
    defineNativeModuleFn(vm, module, "isbool", systemIsBool, 1);
    defineNativeModuleFn(vm, module, "isnum", systemIsNum, 1);
    defineNativeModuleFn(vm, module, "isstr", systemIsStr, 1);
    defineNativeModuleFn(vm, module, "islist", systemIsList, 1);
    defineNativeModuleFn(vm, module, "istable", systemIsTable, 1);
    defineNativeModuleFn(vm, module, "isfunc", systemIsFunc, 1);
    defineNativeModuleFn(vm, module, "isclass", systemIsClass, 1);
    defineNativeModuleFn(vm, module, "isinstance", systemIsInstance, 1);

    // Type conversion
    defineNativeModuleFn(vm, module, "tonum", systemToNum, 1);
    defineNativeModuleFn(vm, module, "tostr", systemToStr, 1);
    defineNativeModuleFn(vm, module, "tobool", systemToBool, 1);
    defineNativeModuleFn(vm, module, "ord", systemOrd, 1);
    defineNativeModuleFn(vm, module, "chr", systemChr, 1);

    // Debug/testing
    defineNativeModuleFn(vm, module, "assert", systemAssert, -1);
    defineNativeModuleFn(vm, module, "error", systemError, -1);

    tableSet(vm, &vm->nativeModules, OBJ_VAL(module->name), OBJ_VAL(module));
    pop(vm);
}