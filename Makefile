# ============================================================================
# BTL — Unified Makefile
#
# Builds:
#   make              → btl (the normal VM interpreter)
#   make debug        → btl_debug (VM with GC tracing)
#   make transpiler   → build/transpiler (the AOT transpiler tool)
#   make transpile    → transpile INPUT file to C
#   make compiled     → build/btl_compiled (native binary from transpiled C)
#   make bench        → transpile + compile + run (with timing)
#   make perf         → run comprehensive VM vs transpiled performance comparison
#   make test         → run test suite against normal VM
#   make test_compiled→ run test suite against transpiled binary
#   make clean
#
# Project layout:
#   project_root/
#   ├── Makefile            ← This file
#   ├── main.c              ← Entry point (REPL + file runner)
#   ├── src/                ← BTL VM source files (.c and .h)
#   ├── transpiler/         ← Transpiler source files
#   ├── obj/                ← VM object files (created by make)
#   ├── build/              ← Transpiler outputs (created by make)
#   ├── test.py             ← Test runner
#   ├── perf_compare.py     ← Performance comparison script
#   └── tests/              ← Test files
# ============================================================================

# --- Compiler & Flags ---
CC       = gcc
CFLAGS   = -Wall -Wextra -std=c11
LIBS     = -lm -lpthread

# --- Targets ---
TARGET          = btl
DEBUG_TARGET    = btl_debug

# --- Directories ---
SRC_DIR         = src
OBJ_DIR         = obj
TRANS_DIR       = transpiler
BUILD_DIR       = build

# --- Entry point (lives in project root, not src/) ---
MAIN_SRC        = main.c
MAIN_OBJ        = $(OBJ_DIR)/main.o

# ============================================================================
# Source file discovery
# ============================================================================

# All VM .c files in src/
VM_SRCS = $(wildcard $(SRC_DIR)/*.c)

# VM object files (for release build)
VM_OBJS = $(VM_SRCS:$(SRC_DIR)/%.c=$(OBJ_DIR)/%.o)

# Note: main.c is in the project root, not src/, so VM_SRCS never includes it.
# Transpiler and compiled binary provide their own main — they just use VM_SRCS directly.

# Transpiler source files
TRANS_SRCS = $(TRANS_DIR)/transpiler.c \
             $(TRANS_DIR)/collect.c \
             $(TRANS_DIR)/transpiler_main.c

# Runtime support for compiled output
COMPILED_SUPPORT = $(TRANS_DIR)/compiled.c
COMPILED_MAIN    = $(TRANS_DIR)/compiled_main.c

# Generated C output
GENERATED = $(BUILD_DIR)/generated.c

# Input file for transpilation (override with: make transpile INPUT=path/to/file.btl)
INPUT ?= examples/fib.btl

# ============================================================================
# Normal VM build (interpreter)
# ============================================================================

all: $(TARGET)

$(TARGET): $(VM_OBJS) $(MAIN_OBJ)
	$(CC) $(CFLAGS) -O3 $^ -o $@ $(LIBS)

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c | $(OBJ_DIR)
	$(CC) $(CFLAGS) -O3 -c $< -o $@

# main.c lives in root, not src/
$(MAIN_OBJ): $(MAIN_SRC) | $(OBJ_DIR)
	$(CC) $(CFLAGS) -O3 -Isrc -c $< -o $@

$(OBJ_DIR):
	mkdir -p $(OBJ_DIR)

# ============================================================================
# Debug VM build
# ============================================================================

debug: $(VM_SRCS) $(MAIN_SRC)
	$(CC) $(CFLAGS) -g -O0 -DDEBUG_LOG_GC -DDEBUG_STRESS_GC -DDEBUG_PRINT_CODE -Isrc $(VM_SRCS) $(MAIN_SRC) -o $(DEBUG_TARGET) $(LIBS)

# ============================================================================
# Transpiler tool build
# ============================================================================

transpiler: $(BUILD_DIR)/transpiler

$(BUILD_DIR)/transpiler: $(TRANS_SRCS) $(VM_SRCS) | $(BUILD_DIR)
	$(CC) $(CFLAGS) -O2 -o $@ $(TRANS_SRCS) $(VM_SRCS) $(LIBS)

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

# ============================================================================
# Transpile a BTL file to C
# ============================================================================

transpile: $(BUILD_DIR)/transpiler
	$(BUILD_DIR)/transpiler $(INPUT) $(GENERATED) --comments
	@echo ""
	@echo "Generated: $(GENERATED)"
	@wc -l $(GENERATED)

# ============================================================================
# Compile generated C into native binary
# ============================================================================

compiled: $(BUILD_DIR)/btl_compiled

$(BUILD_DIR)/btl_compiled: $(GENERATED) $(COMPILED_SUPPORT) $(COMPILED_MAIN) $(VM_SRCS) | $(BUILD_DIR)
	$(CC) $(CFLAGS) -O3 -march=native -I$(TRANS_DIR) -o $@ $^ $(LIBS)
	@echo ""
	@echo "Built: $@"

# ============================================================================
# Bench: transpile + compile + run (with timing)
# ============================================================================

bench: transpile compiled
	@echo ""
	@echo "========================================="
	@echo " Interpreter (./btl)"
	@echo "========================================="
	@time ./$(TARGET) $(INPUT) 2>&1 || true
	@echo ""
	@echo "========================================="
	@echo " Transpiled  (build/btl_compiled)"
	@echo "========================================="
	@time $(BUILD_DIR)/btl_compiled $(INPUT) 2>&1 || true

# ============================================================================
# Performance comparison (VM vs Transpiled)
# ============================================================================

perf: all transpiler
	@python3 perf_compare.py

# ============================================================================
# Test suite
# ============================================================================

test: all
	@python3 test.py

test_compiled: compiled
	@echo "Running tests against transpiled binary..."
	@BTL_BINARY=$(BUILD_DIR)/btl_compiled python3 test.py

# ============================================================================
# Static library (for embedding in other projects like Instar Engine)
# ============================================================================

LIB_TARGET = libbtl.a

lib: CFLAGS += -O3
lib: $(VM_OBJS)
	ar rcs $(LIB_TARGET) $(VM_OBJS)
	@echo "  Built: $(LIB_TARGET)"

# ============================================================================
# Clean
# ============================================================================

clean:
	rm -rf $(OBJ_DIR) $(BUILD_DIR) $(TARGET) $(DEBUG_TARGET) $(LIB_TARGET)

.PHONY: all clean debug test test_compiled transpiler transpile compiled bench perf lib