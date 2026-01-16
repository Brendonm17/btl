# Compiler and Flags
CC = gcc
CFLAGS = -Wall -Wextra -std=c11
LIBS = -lm
TARGET = btl
DEBUG_TARGET = btl_debug

# Directories
SRC_DIR = .
OBJ_DIR = obj

# Find all .c files EXCEPT btl_bin.c
SRCS = $(filter-out $(SRC_DIR)/btl_bin.c, $(wildcard $(SRC_DIR)/*.c))
# Convert .c filenames to .o filenames inside the obj/ folder
OBJS = $(SRCS:$(SRC_DIR)/%.c=$(OBJ_DIR)/%.o)

# Default Rule (Release)
all: CFLAGS += -O3
all: $(TARGET)

# Debug Build
# We use a separate target and don't use the same .o files to avoid 
# mixing optimized code with debug-traced code.
debug: CFLAGS += -g -O0 -DDEBUG_TRACE_EXECUTION -DDEBUG_LOG_GC -DDEBUG_STRESS_GC -DDEBUG_PRINT_CODE
debug: $(SRCS)
	$(CC) $(CFLAGS) $(SRCS) -o $(DEBUG_TARGET) $(LIBS)

# Link the release executable
$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) $^ -o $@ $(LIBS)

# Compile source files to object files
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c | $(OBJ_DIR)
	$(CC) $(CFLAGS) -c $< -o $@

# Create the object directory if it doesn't exist
$(OBJ_DIR):
	mkdir -p $(OBJ_DIR)

# Run the test suite
test: $(TARGET)
	@python3 test.py

# Clean up build artifacts
clean:
	rm -rf $(OBJ_DIR) $(TARGET) $(DEBUG_TARGET)

.PHONY: all clean debug test