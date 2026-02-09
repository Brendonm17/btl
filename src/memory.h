// ============================================================================
// memory.h - BTL Memory Management
//
// This header defines the memory allocation, garbage collection, and I/O
// functions for the BTL runtime. All memory allocation should go through
// these functions to enable:
// - Custom allocators via BTLConfig
// - Garbage collection tracking
// - Memory usage limits
//
// The GC is generational with:
// - Nursery (young generation): Bump-pointer allocation, copying collection
// - Old generation: Mark-sweep collection
// ============================================================================

#ifndef btl_memory_h
#define btl_memory_h

#include "common.h"
#include "value.h"
#include <stdarg.h>

// ----------------------------------------------------------------------------
// Forward declarations
// ----------------------------------------------------------------------------
struct VM;
struct BtlObj;
struct BTLRuntime;

// ============================================================================
// Core Allocation Functions
//
// ALL allocations should go through these (except runtime bootstrap).
// They route through custom allocators if configured.
// ============================================================================

// Main allocation function - routes through runtime callbacks
// Use this for all VM-related allocations
void* btl_realloc(struct VM* vm, void* pointer, size_t oldSize, size_t newSize);

// Runtime-level allocation - for use before VM exists or in runtime itself
// Routes through runtime callbacks if runtime is provided
void* btl_runtime_alloc(struct BTLRuntime* runtime, void* pointer,
                        size_t oldSize, size_t newSize);

// ============================================================================
// Allocation Macros
//
// Convenience macros for common allocation patterns. All ultimately call
// btl_realloc for tracking and custom allocator support.
// ============================================================================

// Allocate an array of count items of the given type
#define BTL_ALLOCATE(vm, type, count) \
    (type*)btl_realloc(vm, NULL, 0, sizeof(type) * (count))

// Free a single object of the given type
#define BTL_FREE(vm, type, pointer) \
    btl_realloc(vm, pointer, sizeof(type), 0)

// Calculate new capacity for growing arrays (doubles, min 8)
#define BTL_GROW_CAPACITY(capacity) \
    ((capacity) < 8 ? 8 : (capacity) * 2)

// Grow an array to a new size
#define BTL_GROW_ARRAY(vm, type, pointer, oldSize, newCount) \
    (type*)btl_realloc(vm, pointer, sizeof(type) * (oldSize), sizeof(type) * (newCount))

// Free an array
#define BTL_FREE_ARRAY(vm, type, pointer, oldCount) \
    btl_realloc(vm, pointer, sizeof(type) * (oldCount), 0)

// ============================================================================
// I/O Functions
//
// All I/O routes through these functions to support custom print/error
// callbacks configured in BTLConfig.
// ============================================================================

// Standard output (uses custom print callback if configured)
void btl_print(struct VM* vm, const char* text);
void btl_println(struct VM* vm, const char* text);
void btl_printf(struct VM* vm, const char* format, ...);
void btl_vprintf(struct VM* vm, const char* format, va_list args);

// Error output (uses custom error callback if configured)
void btl_error(struct VM* vm, const char* text);
void btl_errorln(struct VM* vm, const char* text);
void btl_errorf(struct VM* vm, const char* format, ...);
void btl_verrorf(struct VM* vm, const char* format, va_list args);

// Runtime-level I/O (for use before VM exists)
void btl_runtime_print(struct BTLRuntime* runtime, const char* text);
void btl_runtime_err_print(struct BTLRuntime* runtime, const char* text);

// Print BTL values using configured output
void btl_print_value(struct VM* vm, BtlValue value);
void btl_error_value(struct VM* vm, BtlValue value);

// ============================================================================
// GC Configuration Defaults
//
// These can be overridden via BTLConfig when creating the runtime.
// ============================================================================

// Default nursery size (256KB)
#define BTL_DEFAULT_NURSERY_SIZE        (256 * 1024)

// Threshold before triggering minor GC (nursery size minus headroom)
#define BTL_DEFAULT_NURSERY_THRESHOLD   (BTL_DEFAULT_NURSERY_SIZE - 1024)

// Objects larger than this go directly to old generation
#define BTL_DEFAULT_LARGE_OBJECT_SIZE   (BTL_DEFAULT_NURSERY_SIZE / 4)

// How much to grow heap threshold after major GC (2x)
#define BTL_DEFAULT_GC_HEAP_GROW_FACTOR 2.0f

// Initial old generation heap size before first major GC
#define BTL_DEFAULT_INITIAL_HEAP_SIZE   (1024 * 1024)

// Dynamic thresholds based on actual nursery size
#define BTL_NURSERY_THRESHOLD_FOR(size) ((size) - 1024)
#define BTL_LARGE_OBJECT_SIZE_FOR(size) ((size) / 4)

// ============================================================================
// Object Generations
//
// Generational GC divides objects into young (nursery) and old generations.
// Young objects are collected more frequently using copying collection.
// ============================================================================

typedef enum {
    BTL_GEN_NURSERY = 0,   // Young generation (bump-pointer, copying GC)
    BTL_GEN_OLD = 1        // Old generation (mark-sweep GC)
} BtlGeneration;

// GC color for tri-color marking
typedef enum {
    BTL_GC_WHITE,   // Not yet seen (will be collected if still white at end)
    BTL_GC_GRAY,    // Seen but not yet fully scanned
    BTL_GC_BLACK    // Fully scanned (reachable)
} BtlGCColor;

// ============================================================================
// Nursery Structure
//
// The nursery uses semi-space copying collection. Objects are allocated
// sequentially in fromSpace. During minor GC, live objects are copied to
// toSpace (and promoted to old gen), then spaces are swapped.
// ============================================================================

typedef struct {
    uint8_t* fromSpace;    // Current allocation space
    uint8_t* toSpace;      // Target space for copying
    uint8_t* allocPtr;     // Next allocation position
    uint8_t* limit;        // End of fromSpace
    size_t size;           // Size of each semi-space
} BtlNursery;

// ============================================================================
// Remembered Set
//
// Tracks old-generation objects that point to nursery objects. These are
// additional roots during minor GC to ensure we don't miss live nursery
// objects referenced only from old generation.
// ============================================================================

typedef struct BtlRememberedEntry {
    struct BtlObj* object;
    struct BtlRememberedEntry* next;
} BtlRememberedEntry;

typedef struct {
    BtlRememberedEntry* entries;
    int count;
    int capacity;
} BtlRememberedSet;

// ============================================================================
// Object Allocation
// ============================================================================

typedef enum BtlObjType BtlObjType;

// Allocate a new object of the given size and type
// May allocate in nursery or old gen depending on size and type
void* btl_object_allocate(struct VM* vm, size_t size, BtlObjType type);

// ============================================================================
// GC Marking Functions
//
// Used during mark phase to mark objects as reachable.
// ============================================================================

// Mark an object as reachable (adds to gray stack)
void btl_gc_mark_object(struct VM* vm, struct BtlObj* object);

// Mark a value as reachable (if it's an object)
void btl_gc_mark_value(struct VM* vm, BtlValue value);

// ============================================================================
// Nursery Management
// ============================================================================

// Initialize nursery with configured or default size
void btl_nursery_init(struct VM* vm, BtlNursery* nursery);

// Free nursery memory
void btl_nursery_free(struct VM* vm, BtlNursery* nursery);

// ============================================================================
// Remembered Set Management
// ============================================================================

// Initialize remembered set
void btl_remembered_set_init(BtlRememberedSet* set);

// Free remembered set
void btl_remembered_set_free(struct VM* vm, BtlRememberedSet* set);

// Add an object to the remembered set
void btl_remembered_set_add(struct VM* vm, struct BtlObj* object);

// ============================================================================
// Garbage Collection
// ============================================================================

// Minor GC - collect nursery, promote survivors to old gen
void btl_gc_minor(struct VM* vm);

// Major GC - mark-sweep collection of old generation
void btl_gc_major(struct VM* vm);

// Trigger appropriate GC based on allocation pressure
void btl_gc_collect(struct VM* vm);

// Temporarily prevent GC (nest-able)
void btl_gc_inhibit_start(struct VM* vm);
void btl_gc_inhibit_end(struct VM* vm);

// Free all objects (during VM shutdown)
void btl_gc_free_all(struct VM* vm);

// Write barrier - track old-to-young references
void btl_gc_write_barrier(struct VM* vm, struct BtlObj* container, BtlValue value);

// ============================================================================
// Utility Functions
// ============================================================================

// Check if a pointer is within the nursery
static inline bool btl_is_in_nursery(BtlNursery* nursery, void* ptr) {
    uint8_t* p = (uint8_t*)ptr;
    return p >= nursery->fromSpace && p < nursery->fromSpace + nursery->size;
}

#endif // btl_memory_h
