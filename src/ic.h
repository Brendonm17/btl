#ifndef btl_ic_h
#define btl_ic_h

#include "common.h"

typedef struct ObjClass ObjClass;

// Field access cache
typedef struct {
    ObjClass* cachedClass;
    int fieldIndex;  // -1 = not cached
} FieldIC;

// Method invoke cache  
typedef struct {
    ObjClass* cachedClass;
    int methodIndex;  // -1 = not cached
} MethodIC;

static inline void initFieldICs(FieldIC* ics, int count) {
    for (int i = 0; i < count; i++) {
        ics[i].cachedClass = NULL;
        ics[i].fieldIndex = -1;
    }
}

static inline void initMethodICs(MethodIC* ics, int count) {
    for (int i = 0; i < count; i++) {
        ics[i].cachedClass = NULL;
        ics[i].methodIndex = -1;
    }
}

#endif