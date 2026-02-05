#ifndef btl_common_h
#define btl_common_h

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

//#if defined(__GNUC__) || defined(__clang__)
#define HAS_COMPUTED_GOTOS
//#endif

#define NAN_BOXING
// #define DEBUG_TRACE_EXECUTION
// #define DEBUG_PRINT_CODE
// #define DEBUG_STRESS_GC
// #define DEBUG_LOG_GC

#define UINT8_COUNT (UINT8_MAX + 1)
#define BTL_NUM_THREADS 4

#endif