// ============================================================================
// common.h - BTL Common Definitions
//
// This header contains common includes, macros, and configuration options
// used throughout the BTL virtual machine and compiler.
// ============================================================================

#ifndef btl_common_h
#define btl_common_h

// ----------------------------------------------------------------------------
// Standard library includes
// ----------------------------------------------------------------------------
#include <stdbool.h>  // bool, true, false
#include <stddef.h>   // size_t, NULL
#include <stdint.h>   // uint8_t, uint16_t, uint32_t, int32_t, etc.

// ----------------------------------------------------------------------------
// Computed gotos support
//
// Computed gotos (GCC extension) provide faster bytecode dispatch by using
// direct jumps instead of a switch statement. This is enabled on GCC/Clang.
// ----------------------------------------------------------------------------
// #if defined(__GNUC__) || defined(__clang__)
#define BTL_HAS_COMPUTED_GOTOS
// #endif

// ----------------------------------------------------------------------------
// NaN boxing configuration
//
// NaN boxing stores type information in the NaN bits of IEEE 754 doubles,
// allowing values to fit in 64 bits without a separate type tag. This
// improves cache efficiency and reduces memory usage.
// ----------------------------------------------------------------------------
#define BTL_NAN_BOXING

// ----------------------------------------------------------------------------
// Debug configuration options
//
// Uncomment these to enable various debugging features:
// - BTL_DEBUG_TRACE_EXECUTION: Print each bytecode instruction as it executes
// - BTL_DEBUG_PRINT_CODE: Print compiled bytecode after compilation
// - BTL_DEBUG_STRESS_GC: Run GC on every allocation (finds GC bugs)
// - BTL_DEBUG_LOG_GC: Log garbage collection events
// ----------------------------------------------------------------------------
// #define BTL_DEBUG_TRACE_EXECUTION
// #define BTL_DEBUG_PRINT_CODE
// #define BTL_DEBUG_STRESS_GC
// #define BTL_DEBUG_LOG_GC

// ----------------------------------------------------------------------------
// Constants
// ----------------------------------------------------------------------------

// Maximum number of values addressable by a single byte (256)
#define BTL_UINT8_COUNT (UINT8_MAX + 1)

// Number of worker threads for the thread pool
#define BTL_NUM_THREADS 4

#endif // btl_common_h
