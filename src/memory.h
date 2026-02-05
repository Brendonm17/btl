#ifndef btl_memory_h
#define btl_memory_h

#include "common.h"
#include "value.h"
#include <stdarg.h>

// ============================================================================
// Forward declarations
// ============================================================================
struct VM;
struct Obj;
struct BTLRuntime;

// ============================================================================
// Core allocation functions
// ALL allocations should go through these (except runtime bootstrap)
// ============================================================================

// Main allocation function - routes through runtime callbacks
// Use this for all VM-related allocations
void* btl_realloc(struct VM* vm, void* pointer, size_t oldSize, size_t newSize);

// Runtime-level allocation - for use before VM exists or in runtime itself
// Routes through runtime callbacks if runtime is provided
void* btl_runtime_alloc(struct BTLRuntime* runtime, void* pointer, size_t oldSize, size_t newSize);

// Legacy name for compatibility
#define reallocate(vm, ptr, oldSize, newSize) btl_realloc(vm, ptr, oldSize, newSize)

// ============================================================================
// Allocation macros
// ============================================================================

#define ALLOCATE(vm, type, count) \
    (type*)btl_realloc(vm, NULL, 0, sizeof(type) * (count))

#define FREE(vm, type, pointer) \
    btl_realloc(vm, pointer, sizeof(type), 0)

#define GROW_CAPACITY(capacity) \
    ((capacity) < 8 ? 8 : (capacity) * 2)

#define GROW_ARRAY(vm, type, pointer, oldSize, newCount) \
    (type*)btl_realloc(vm, pointer, sizeof(type) * (oldSize), sizeof(type) * (newCount))

#define FREE_ARRAY(vm, type, pointer, oldCount) \
    btl_realloc(vm, pointer, sizeof(type) * (oldCount), 0)

// ============================================================================
// I/O functions - route through runtime callbacks
// ============================================================================

// Standard output (or custom print callback)
void btl_print(struct VM* vm, const char* text);
void btl_println(struct VM* vm, const char* text);
void btl_printf(struct VM* vm, const char* format, ...);
void btl_vprintf(struct VM* vm, const char* format, va_list args);

// Error output (or custom error callback)
void btl_error(struct VM* vm, const char* text);
void btl_errorln(struct VM* vm, const char* text);
void btl_errorf(struct VM* vm, const char* format, ...);
void btl_verrorf(struct VM* vm, const char* format, va_list args);

// Runtime-level I/O (for use before VM exists)
void btl_runtime_print(struct BTLRuntime* runtime, const char* text);
void btl_runtime_error(struct BTLRuntime* runtime, const char* text);

// Print BTL values
void btl_print_value(struct VM* vm, Value value);
void btl_error_value(struct VM* vm, Value value);

// ============================================================================
// GC Configuration
// ============================================================================

// ============================================================================
// GC Configuration (defaults - can be overridden via BTLConfig)
// ============================================================================

#define DEFAULT_NURSERY_SIZE        (256 * 1024)
#define DEFAULT_NURSERY_THRESHOLD   (DEFAULT_NURSERY_SIZE - 1024)
#define DEFAULT_LARGE_OBJECT_SIZE   (DEFAULT_NURSERY_SIZE / 4)
#define DEFAULT_GC_HEAP_GROW_FACTOR 2.0f
#define DEFAULT_INITIAL_HEAP_SIZE   (1024 * 1024)

// Dynamic thresholds based on actual nursery size
#define NURSERY_THRESHOLD_FOR(size) ((size) - 1024)
#define LARGE_OBJECT_SIZE_FOR(size) ((size) / 4)

// ============================================================================
// Object generations for generational GC
// ============================================================================

typedef enum {
    GEN_NURSERY = 0,
    GEN_OLD = 1
} Generation;

typedef enum {
    GC_WHITE,
    GC_GRAY,
    GC_BLACK
} GCColor;

// ============================================================================
// Nursery structure
// ============================================================================

typedef struct {
    uint8_t* fromSpace;
    uint8_t* toSpace;
    uint8_t* allocPtr;
    uint8_t* limit;
    size_t size;
} Nursery;

// ============================================================================
// Remembered set
// ============================================================================

typedef struct RememberedEntry {
    struct Obj* object;
    struct RememberedEntry* next;
} RememberedEntry;

typedef struct {
    RememberedEntry* entries;
    int count;
    int capacity;
} RememberedSet;

// ============================================================================
// Object allocation
// ============================================================================

typedef enum ObjType ObjType;
void* allocateObject(struct VM* vm, size_t size, ObjType type);

// ============================================================================
// Marking functions
// ============================================================================

void markObject(struct VM* vm, struct Obj* object);
void markValue(struct VM* vm, Value value);

// ============================================================================
// GC functions
// ============================================================================

void initNursery(struct VM* vm, Nursery* nursery);
void freeNursery(struct VM* vm, Nursery* nursery);

void initRememberedSet(RememberedSet* set);
void freeRememberedSet(struct VM* vm, RememberedSet* set);
void rememberObject(struct VM* vm, struct Obj* object);

void minorGC(struct VM* vm);
void majorGC(struct VM* vm);
void collectGarbage(struct VM* vm);

void gcInhibitStart(struct VM* vm);
void gcInhibitEnd(struct VM* vm);

void freeObjects(struct VM* vm);

void writeBarrier(struct VM* vm, struct Obj* container, Value value);

// ============================================================================
// Utility
// ============================================================================

static inline bool isInNursery(Nursery* nursery, void* ptr) {
    uint8_t* p = (uint8_t*) ptr;
    return p >= nursery->fromSpace && p < nursery->fromSpace + nursery->size;
}

#endif