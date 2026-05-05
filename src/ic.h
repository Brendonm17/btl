#ifndef btl_ic_h
#define btl_ic_h

#include "common.h"

typedef struct ObjClass ObjClass;

// Inline cache for field access. Caches class + field index for fast repeat lookups.
typedef struct {
    ObjClass* cachedClass;
    int fieldIndex;
} BtlFieldIC;

// Inline cache for method invocation.
typedef struct {
    ObjClass* cachedClass;
    int methodIndex;
} BtlMethodIC;

static inline void btl_field_ic_init(BtlFieldIC* ics, int count) {
    for (int i = 0; i < count; i++) {
        ics[i].cachedClass = NULL;
        ics[i].fieldIndex = -1;
    }
}

static inline void btl_method_ic_init(BtlMethodIC* ics, int count) {
    for (int i = 0; i < count; i++) {
        ics[i].cachedClass = NULL;
        ics[i].methodIndex = -1;
    }
}

#endif
