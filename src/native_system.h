// ============================================================================
// native_system.h - BTL Native System Module
//
// Provides system-level functions:
// - print(...): Output to stdout
// - println(...): Output with newline
// - clock(): Current time in seconds
// - sleep(ms): Pause execution
// - exit(code): Terminate program
// - args: Command line arguments list
// - getenv(name): Get environment variable
// ============================================================================

#ifndef btl_native_system_h
#define btl_native_system_h

#include "vm.h"

// Set command line arguments (call from main before running)
void btl_set_system_args(int argc, const char* argv[]);

// Initialize the system native module
void btl_system_module_init(VM* vm);

#endif // btl_native_system_h
