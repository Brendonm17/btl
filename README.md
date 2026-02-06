# BTL

**Bren's Tiny Language** -- A lightweight scripting language designed for game development with built-in concurrency support.

## Features

- Clean, familiar syntax inspired by JavaScript and Lua
- Classes with single inheritance
- First-class functions and closures
- Built-in lists and tables (dictionaries)
- Native methods on strings, numbers, lists, and tables
- Switch expressions with comparison operators
- Actors and futures for built-in concurrency
- Module system with native and user modules
- BTL-to-C transpiler for native performance

## Quick Start

```bash
make            # build the interpreter
./btl hello.btl # run a script
```

```js
import "system";

system.println("Hello, World!");
```

## Requirements

- GCC or Clang (C11)
- POSIX threads (pthread)
- Math library (libm)

## Documentation

| Document | Description |
|----------|-------------|
| [Building](docs/BUILDING.md) | Build instructions, transpiler usage, and compilation details |
| [Language Reference](docs/LANGUAGE.md) | Syntax, types, control flow, classes, concurrency, and full grammar |
| [Standard Library](docs/STDLIB.md) | Native modules (`math`, `random`, `system`) and built-in type methods |
| [Examples](docs/EXAMPLES.md) | Code examples covering all language features |

## License

MIT License -- see [LICENSE](LICENSE) for details.
