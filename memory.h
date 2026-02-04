#ifndef btl_memory_h
#define btl_memory_h

#include "common.h"
#include "value.h"

// Allocation macros
#define ALLOCATE(vm, type, count) (type*)reallocate(vm, NULL, 0, sizeof(type) * (count))
#define FREE(vm, type, pointer) reallocate(vm, pointer, sizeof(type), 0)
#define GROW_CAPACITY(capacity) ((capacity) < 8 ? 8 : (capacity) * 2)
#define GROW_ARRAY(vm, type, pointer, oldSize, newCount) \
    (type*)reallocate(vm, pointer, sizeof(type) * (oldSize), sizeof(type) * (newCount))
#define FREE_ARRAY(vm, type, pointer, oldCount) reallocate(vm, pointer, sizeof(type) * (oldCount), 0)

// GC Configuration
#define NURSERY_SIZE        (256 * 1024)    // 256 KB nursery
#define NURSERY_THRESHOLD   (NURSERY_SIZE - 1024)  // Trigger minor GC before completely full
#define LARGE_OBJECT_SIZE   (NURSERY_SIZE / 4)     // Objects larger than this go directly to old gen
#define GC_HEAP_GROW_FACTOR 2

// Note: Generation enum is defined in object.h (GEN_NURSERY, GEN_OLD)

// Tri-color marking states (for old generation)
typedef enum {
    GC_WHITE,   // Not visited yet (candidate for collection)
    GC_GRAY,    // Visited, but children not yet scanned
    GC_BLACK    // Fully traced (reachable)
} GCColor;

// Nursery structure - bump allocator
typedef struct {
    uint8_t* fromSpace;     // Current allocation space
    uint8_t* toSpace;       // Copy destination (for minor GC)
    uint8_t* allocPtr;      // Current allocation pointer (bump pointer)
    uint8_t* limit;         // End of fromSpace
    size_t size;            // Size of each semi-space
} Nursery;

// Remembered set entry - tracks old->young pointers
typedef struct RememberedEntry {
    struct Obj* object;             // Old gen object that points to nursery
    struct RememberedEntry* next;
} RememberedEntry;

typedef struct {
    RememberedEntry* entries;
    int count;
    int capacity;
} RememberedSet;

// Core memory functions
void* reallocate(struct VM* vm, void* pointer, size_t oldSize, size_t newSize);

// Object allocation - ObjType is defined in object.h
// Forward declare it here to avoid circular dependency
typedef enum ObjType ObjType;
void* allocateObject(struct VM* vm, size_t size, ObjType type);

// Marking functions
void markObject(struct VM* vm, struct Obj* object);
void markValue(struct VM* vm, Value value);

// GC functions
void initNursery(Nursery* nursery);
void freeNursery(Nursery* nursery);

void initRememberedSet(RememberedSet* set);
void freeRememberedSet(struct VM* vm, RememberedSet* set);
void rememberObject(struct VM* vm, struct Obj* object);

void minorGC(struct VM* vm);        // Collect nursery (young gen)
void majorGC(struct VM* vm);        // Collect old gen (full GC)
void collectGarbage(struct VM* vm); // Decides which GC to run

// GC inhibit helpers - use during critical object construction
void gcInhibitStart(struct VM* vm);
void gcInhibitEnd(struct VM* vm);

void freeObjects(struct VM* vm);

// Write barrier - MUST be called when writing a reference
// Call this AFTER the write: obj->field = value; writeBarrier(vm, obj, value);
void writeBarrier(struct VM* vm, struct Obj* container, Value value);

// Check if object is in nursery
static inline bool isInNursery(Nursery* nursery, void* ptr) {
    uint8_t* p = (uint8_t*) ptr;
    return p >= nursery->fromSpace && p < nursery->fromSpace + nursery->size;
}

#endif