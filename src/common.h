#ifndef btl_common_h
#define btl_common_h

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

// Computed gotos give faster bytecode dispatch via direct jumps. GCC/Clang only.
#if defined(__GNUC__) || defined(__clang__)
#define BTL_HAS_COMPUTED_GOTOS
#endif

// NaN boxing packs type tag + value into 64-bit IEEE 754 doubles. Improves
// cache density.
#define BTL_NAN_BOXING

// Debug toggles:
//   BTL_DEBUG_TRACE_EXECUTION  print each instruction as it executes
//   BTL_DEBUG_PRINT_CODE       print bytecode after compile
//   BTL_DEBUG_STRESS_GC        run GC on every allocation
//   BTL_DEBUG_LOG_GC           log GC events
// #define BTL_DEBUG_TRACE_EXECUTION
// #define BTL_DEBUG_PRINT_CODE
// #define BTL_DEBUG_STRESS_GC
// #define BTL_DEBUG_LOG_GC

#define BTL_UINT8_COUNT (UINT8_MAX + 1)

#define BTL_NUM_THREADS 4

#endif
