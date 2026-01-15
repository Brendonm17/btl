#ifndef btl_common_h
#define btl_common_h

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#define NAN_BOXING // Comment out to use standard struct-based values

// Detection for Computed Gotos (Labels as values)
// Supported by GCC and Clang.
#if defined(__GNUC__) || defined(__clang__)
#define HAS_COMPUTED_GOTOS
#endif

#define DEBUG_TRACE_EXECUTION
#define DEBUG_PRINT_CODE

#define UINT8_COUNT (UINT8_MAX + 1)

#endif