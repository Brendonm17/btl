#ifndef btl_native_system_h
#define btl_native_system_h

#include "vm.h"

void initSystemModule(VM* vm);
void setSystemArgs(int argc, const char** argv);

#endif