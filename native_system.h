#ifndef btl_native_system_h
#define btl_native_system_h

#include "vm.h"

// Set command line arguments (call from main before running)
void setSystemArgs(int argc, const char* argv []);

// Initialize the system module
void initSystemModule(VM* vm);

#endif