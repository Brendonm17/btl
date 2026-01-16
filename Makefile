# Compiler and Flags
CC = gcc
CFLAGS = -Wall -Wextra -std=c11 -O3
TARGET = btl

# Directories (assuming everything is in the root for now)
SRC_DIR = .
OBJ_DIR = obj

# Find all .c files EXCEPT btl_bin.c (since it's unused)
SRCS = $(filter-out $(SRC_DIR)/btl_bin.c, $(wildcard $(SRC_DIR)/*.c))
# Convert .c filenames to .o filenames inside the obj/ folder
OBJS = $(SRCS:$(SRC_DIR)/%.c=$(OBJ_DIR)/%.o)

# Default Rule
all: $(TARGET)

# Link the executable
$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) $^ -o $@

# Compile source files to object files
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c | $(OBJ_DIR)
	$(CC) $(CFLAGS) -c $< -o $@

# Create the object directory if it doesn't exist
$(OBJ_DIR):
	mkdir -p $(OBJ_DIR)

# Debug Build (adds debug symbols and can enable internal clox tracing)
debug: CFLAGS = -Wall -Wextra -std=c11 -g -DDEBUG_TRACE_EXECUTION -DDEBUG_PRINT_CODE
debug: clean $(TARGET)

# Clean up build artifacts
clean:
	rm -rf $(OBJ_DIR) $(TARGET)

.PHONY: all clean debug