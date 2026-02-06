#ifndef btl_ic_h
#define btl_ic_h

#include "common.h"

// Forward declaration for class type
typedef struct ObjClass ObjClass;

// BtlFieldIC - Inline cache for field access operations
// Caches the class and field index to speed up repeated field lookups
typedef struct {
    ObjClass* cachedClass;  // The class this cache entry is valid for
    int fieldIndex;         // Cached field index, -1 if not cached
} BtlFieldIC;

// BtlMethodIC - Inline cache for method invocation operations
// Caches the class and method index to speed up repeated method lookups
typedef struct {
    ObjClass* cachedClass;  // The class this cache entry is valid for
    int methodIndex;        // Cached method index, -1 if not cached
} BtlMethodIC;

// btl_field_ic_init - Initialize an array of field inline caches
// Sets all caches to an uncached state (NULL class, -1 index)
static inline void btl_field_ic_init(BtlFieldIC* ics, int count) {
    for (int i = 0; i < count; i++) {
        ics[i].cachedClass = NULL;
        ics[i].fieldIndex = -1;
    }
}

// btl_method_ic_init - Initialize an array of method inline caches
// Sets all caches to an uncached state (NULL class, -1 index)
static inline void btl_method_ic_init(BtlMethodIC* ics, int count) {
    for (int i = 0; i < count; i++) {
        ics[i].cachedClass = NULL;
        ics[i].methodIndex = -1;
    }
}

#endif
