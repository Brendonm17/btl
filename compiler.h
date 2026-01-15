
#ifndef btl_compiler_h
#define btl_compiler_h


#include "object.h"


#include "vm.h"



ObjFunction* compile(const char* source);


void markCompilerRoots();


#endif