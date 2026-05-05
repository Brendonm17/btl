#ifndef btl_native_system_h
#define btl_native_system_h

#include "vm.h"

// Call this from main before running scripts.
void btl_set_system_args(int argc, const char* argv[]);

void btl_system_module_init(VM* vm);

#endif
