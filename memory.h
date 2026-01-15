#ifndef btl_memory_h
#define btl_memory_h

#include "common.h"
#include "value.h"
#include "vm.h"

#define ALLOCATE(vm, type, count) \
    (type*)reallocate(vm, NULL, 0, sizeof(type) * (count))

#define FREE(vm, type, pointer) reallocate(vm, pointer, sizeof(type), 0)

#define GROW_CAPACITY(capacity) \
    ((capacity) < 8 ? 8 : (capacity) * 2)

#define GROW_ARRAY(vm, type, pointer, oldSize, newCount) \
    (type*)reallocate(vm, pointer, sizeof(type) * (oldSize), \
        sizeof(type) * (newCount))

#define FREE_ARRAY(vm, type, pointer, oldCount) \
    reallocate(vm, pointer, sizeof(type) * (oldCount), 0)

void* reallocate(VM* vm, void* pointer, size_t oldSize, size_t newSize);
void markObject(VM* vm, struct Obj* object);
void markValue(VM* vm, Value value);
void collectGarbage(VM* vm);
void freeObjects(VM* vm);

#endif