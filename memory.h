#ifndef btl_memory_h
#define btl_memory_h

#include "common.h"
#include "value.h"

#define ALLOCATE(vm, type, count) \
    (type*)reallocate(vm, NULL, 0, sizeof(type) * (count))

#define FREE(vm, type, pointer) reallocate(vm, pointer, sizeof(type), 0)

#define GROW_CAPACITY(cap) ((cap) < 8 ? 8 : (cap) * 2)

#define GROW_ARRAY(vm, type, ptr, old, newCount) \
    (type*)reallocate(vm, ptr, sizeof(type) * (old), sizeof(type) * (newCount))

#define FREE_ARRAY(vm, type, ptr, old) reallocate(vm, ptr, sizeof(type) * (old), 0)

void* reallocate(struct VM* vm, void* pointer, size_t oldSize, size_t newSize);
void markObject(struct VM* vm, Obj* object);
void markValue(struct VM* vm, Value value);
void collectGarbage(struct VM* vm);
void freeObjects(struct VM* vm);

#endif