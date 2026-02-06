# Building BTL

## Prerequisites

- **GCC or Clang** with C11 support
- **POSIX threads** (pthread)
- **Math library** (libm)
- **Python 3** (optional, for running the test suite)

## Building the Interpreter

```bash
make          # Build release version (./btl)
make debug    # Build debug version with GC tracing
make test     # Run the test suite
make clean    # Clean build artifacts
```

The release build produces a `btl` executable in the project root.

### Running a Script

```bash
./btl script.btl
```

### Interactive REPL

```bash
./btl
```

Running `btl` with no arguments starts an interactive read-eval-print loop.

---

## BTL-to-C Transpiler

BTL includes a transpiler that compiles BTL scripts into optimized C code. The generated C is then compiled with a standard C compiler to produce a native binary, giving significant performance improvements over the bytecode interpreter.

### Building the Transpiler

```bash
make transpiler   # Build the transpiler tool (./build/transpiler)
```

### Usage

Transpiling is a two-step process:

```bash
# Step 1: Transpile BTL source to C
./transpiler myscript.btl output.c

# Step 2: Compile the generated C with the BTL runtime
gcc -O2 output.c compiled.c compiled_main.c src/*.c -o myscript -lm -lpthread
```

### Transpiler Flags

| Flag | Description |
|------|-------------|
| `--comments` | Include bytecode offset comments in the generated C |
| `--lines` | Include source line info for debugging |
| `--checks` | Include bounds and type checks (safer but slower) |

```bash
# Example with all debug options
./transpiler myscript.btl output.c --comments --lines --checks
```

---

## How the Transpiler Works

The transpiler operates in three stages:

1. **Compile** -- The BTL compiler parses your script and produces bytecode, just like normal execution.
2. **Transpile** -- The transpiler walks every function in the bytecode and emits equivalent C code into a single `.c` file.
3. **Build** -- A C compiler compiles the generated code together with the BTL runtime support library into a native executable.

The generated program still requires the BTL runtime (VM, garbage collector, native modules) at link time. The transpiler replaces the interpreter dispatch loop, not the entire runtime.

### Performance Optimizations

The transpiler applies several optimizations that the interpreter cannot:

**Inline stack operations** -- The interpreter calls `push()`/`pop()` functions for every stack access. The transpiled code uses direct pointer arithmetic (`*sp++ = value`) with the stack pointer kept in a C local variable, which the C compiler can promote to a CPU register.

**Cached VM state** -- Hot VM fields (`frame`, `slots`, `stackTop`) are cached in C locals rather than repeatedly dereferencing the VM struct pointer. They are only synced back at call boundaries where callees need to see the real stack.

**Direct call threading** -- When the transpiler can determine that a closure maps to a known transpiled function, it emits a direct C function call (`btl_fn_N(vm)`) instead of going through the generic `callValue()` dispatch and interpreter loop.

**Fused opcode patterns** -- Common multi-opcode sequences are collapsed into single C statements. For example, a loop counter like `i = i + 1` (which is four bytecode instructions) becomes one C assignment. Similarly, loop conditions like `while (i < n)` fuse the local reads, comparison, and conditional branch into a single `if (...) goto` statement.

**Tail call optimization** -- Tail calls to known transpiled functions are converted to a parameter shuffle followed by a `goto` to the function entry point, avoiding new stack frames and function call overhead.

### Generated Code Structure

The transpiler produces a single C file containing:

- Inline stack macros (`PUSH`, `POP`, `PEEK`)
- A forward declaration for each BTL function (`btl_fn_0`, `btl_fn_1`, ...)
- A dispatch table mapping bytecode function pointers to their transpiled C equivalents
- The transpiled body of each function, using `goto`-based control flow for branches and loops
- An entry point (`btl_compiled_run`) that bootstraps the VM and calls `btl_fn_0`

The generated file `#include`s `compiled.h`, which provides helper functions for complex operations (property access, method invocation, class construction, etc.) that are too large to inline.
