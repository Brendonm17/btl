// Memory allocation, GC, and I/O. All allocations route through btl_realloc
// so custom allocators (BTLConfig) and GC tracking work uniformly.
//
// GC is generational:
//   Nursery (young gen): bump-pointer alloc, copying collection.
//   Old gen: mark-sweep.

#ifndef btl_memory_h
#define btl_memory_h

#include "common.h"
#include "value.h"
#include <stdarg.h>

struct VM;
struct BtlObj;
struct BTLRuntime;

// All VM-related allocations. NULL ptr + nonzero new size acts as alloc;
// nonzero ptr + zero new size acts as free.
void* btl_realloc(struct VM* vm, void* pointer, size_t oldSize, size_t newSize);

// For use before the VM exists (during runtime bootstrap).
void* btl_runtime_alloc(struct BTLRuntime* runtime, void* pointer,
                        size_t oldSize, size_t newSize);

#define BTL_ALLOCATE(vm, type, count) \
    (type*)btl_realloc(vm, NULL, 0, sizeof(type) * (count))

#define BTL_FREE(vm, type, pointer) \
    btl_realloc(vm, pointer, sizeof(type), 0)

// Doubles, min 8.
#define BTL_GROW_CAPACITY(capacity) \
    ((capacity) < 8 ? 8 : (capacity) * 2)

#define BTL_GROW_ARRAY(vm, type, pointer, oldSize, newCount) \
    (type*)btl_realloc(vm, pointer, sizeof(type) * (oldSize), sizeof(type) * (newCount))

#define BTL_FREE_ARRAY(vm, type, pointer, oldCount) \
    btl_realloc(vm, pointer, sizeof(type) * (oldCount), 0)

// All I/O routes through these so BTLConfig.print/error callbacks can override.
void btl_print(struct VM* vm, const char* text);
void btl_println(struct VM* vm, const char* text);
void btl_printf(struct VM* vm, const char* format, ...);
void btl_vprintf(struct VM* vm, const char* format, va_list args);

void btl_error(struct VM* vm, const char* text);
void btl_errorln(struct VM* vm, const char* text);
void btl_errorf(struct VM* vm, const char* format, ...);
void btl_verrorf(struct VM* vm, const char* format, va_list args);

void btl_runtime_print(struct BTLRuntime* runtime, const char* text);
void btl_runtime_err_print(struct BTLRuntime* runtime, const char* text);

void btl_print_value(struct VM* vm, BtlValue value);
void btl_error_value(struct VM* vm, BtlValue value);

// Defaults; overridable via BTLConfig.
#define BTL_DEFAULT_NURSERY_SIZE        (256 * 1024)
#define BTL_DEFAULT_NURSERY_THRESHOLD   (BTL_DEFAULT_NURSERY_SIZE - 1024)
#define BTL_DEFAULT_LARGE_OBJECT_SIZE   (BTL_DEFAULT_NURSERY_SIZE / 4)
#define BTL_DEFAULT_GC_HEAP_GROW_FACTOR 2.0f
#define BTL_DEFAULT_INITIAL_HEAP_SIZE   (1024 * 1024)

#define BTL_NURSERY_THRESHOLD_FOR(size) ((size) - 1024)
#define BTL_LARGE_OBJECT_SIZE_FOR(size) ((size) / 4)

typedef enum {
    BTL_GEN_NURSERY = 0,   // bump-pointer, copying GC
    BTL_GEN_OLD = 1        // mark-sweep
} BtlGeneration;

// Tri-color marking.
typedef enum {
    BTL_GC_WHITE,   // not yet seen
    BTL_GC_GRAY,    // seen, not yet scanned
    BTL_GC_BLACK    // fully scanned
} BtlGCColor;

// Semi-space copying nursery. Live objects copy to toSpace during minor GC,
// then the spaces swap.
typedef struct {
    uint8_t* fromSpace;
    uint8_t* toSpace;
    uint8_t* allocPtr;
    uint8_t* limit;
    size_t size;
} BtlNursery;

// Tracks old-gen objects that point into the nursery. Used as extra roots
// during minor GC.
typedef struct BtlRememberedEntry {
    struct BtlObj* object;
    struct BtlRememberedEntry* next;
} BtlRememberedEntry;

typedef struct {
    BtlRememberedEntry* entries;
    int count;
    int capacity;
} BtlRememberedSet;

typedef enum BtlObjType BtlObjType;

void* btl_object_allocate(struct VM* vm, size_t size, BtlObjType type);

void btl_gc_mark_object(struct VM* vm, struct BtlObj* object);
void btl_gc_mark_value(struct VM* vm, BtlValue value);

void btl_nursery_init(struct VM* vm, BtlNursery* nursery);
void btl_nursery_free(struct VM* vm, BtlNursery* nursery);

void btl_remembered_set_init(BtlRememberedSet* set);
void btl_remembered_set_free(struct VM* vm, BtlRememberedSet* set);
void btl_remembered_set_add(struct VM* vm, struct BtlObj* object);

void btl_gc_minor(struct VM* vm);
void btl_gc_major(struct VM* vm);
void btl_gc_collect(struct VM* vm);

// Nestable.
void btl_gc_inhibit_start(struct VM* vm);
void btl_gc_inhibit_end(struct VM* vm);

void btl_gc_free_all(struct VM* vm);

// Track old-to-young pointer writes.
void btl_gc_write_barrier(struct VM* vm, struct BtlObj* container, BtlValue value);

static inline bool btl_is_in_nursery(BtlNursery* nursery, void* ptr) {
    uint8_t* p = (uint8_t*)ptr;
    return p >= nursery->fromSpace && p < nursery->fromSpace + nursery->size;
}

#endif
