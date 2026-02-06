# BTL Standard Library

BTL ships with three native modules (`math`, `random`, `system`) and native methods on all built-in types.

---

## math Module

Mathematical constants and functions.

```js
import "math";
```

### Constants

| Name | Description |
|------|-------------|
| `PI` | 3.14159265358979323846 |
| `E` | Euler's number (2.71828182845904523536) |
| `TAU` | 2 * PI (6.28318530717958647693) |
| `INF` | Positive infinity |
| `NAN` | Not-a-number |

### Functions

| Function | Description |
|----------|-------------|
| `abs(n)` | Absolute value |
| `floor(n)` | Round down to nearest integer |
| `ceil(n)` | Round up to nearest integer |
| `round(n)` | Round to nearest integer |
| `trunc(n)` | Truncate toward zero |
| `sqrt(n)` | Square root |
| `pow(base, exp)` | Raise base to exponent |
| `sin(n)` | Sine (radians) |
| `cos(n)` | Cosine (radians) |
| `tan(n)` | Tangent (radians) |
| `asin(n)` | Arc sine (returns radians) |
| `acos(n)` | Arc cosine (returns radians) |
| `atan(n)` | Arc tangent (returns radians) |
| `atan2(y, x)` | Two-argument arc tangent |
| `log(n)` | Natural logarithm (base e) |
| `log10(n)` | Base-10 logarithm |
| `log2(n)` | Base-2 logarithm |
| `exp(n)` | e raised to power n |
| `sign(n)` | Returns -1, 0, or 1 |
| `min(a, b)` | Minimum of two values |
| `max(a, b)` | Maximum of two values |
| `clamp(n, min, max)` | Clamp value to range |
| `lerp(a, b, t)` | Linear interpolation |
| `hypot(x, y)` | Hypotenuse (sqrt(x^2 + y^2)) |
| `fmod(a, b)` | Floating-point modulo |
| `deg(rad)` | Convert radians to degrees |
| `rad(deg)` | Convert degrees to radians |

---

## random Module

Random number generation using xorshift64 PRNG.

```js
import "random";
```

### Functions

| Function | Description |
|----------|-------------|
| `random()` | Random float in [0, 1) |
| `int(min, max)` | Random integer in [min, max] inclusive |
| `float(min, max)` | Random float in [min, max) |
| `seed(n)` | Seed the random generator |
| `bool()` | Random true or false |
| `chance(p)` | True with probability p (0-1) |
| `choice(list)` | Random element from list |
| `shuffle(list)` | Shuffle list in place, returns it |
| `normal(mean, stddev)` | Normal distribution sample |
| `dice(sides)` | Roll a die (1 to sides) |
| `diceSum(count, sides)` | Sum of multiple dice rolls |

---

## system Module

System utilities, I/O, type checking, and conversions.

```js
import "system";
```

### I/O

| Function | Description |
|----------|-------------|
| `write(...)` | Print without newline |
| `println(...)` | Print with newline |
| `input(prompt)` | Read line from stdin |

### Time

| Function | Description |
|----------|-------------|
| `clock()` | CPU time in seconds (float) |
| `time()` | Unix timestamp in seconds (float) |
| `millis()` | Milliseconds since epoch |
| `nanos()` | Nanoseconds since epoch |
| `sleep(seconds)` | Pause execution |

### Process

| Function | Description |
|----------|-------------|
| `exit(code)` | Exit with status code |
| `getenv(name)` | Get environment variable (nil if not set) |

### Command Line Arguments

| Function | Description |
|----------|-------------|
| `argc()` | Number of arguments |
| `argv(index)` | Get argument at index |
| `args()` | Get all arguments as list |

### System Info

| Function | Description |
|----------|-------------|
| `platform()` | OS name (`"linux"`, `"macos"`, `"windows"`, `"unknown"`) |
| `arch()` | CPU architecture (`"x86_64"`, `"arm64"`, `"x86"`, `"arm"`, `"unknown"`) |
| `version()` | BTL version string |

### Type Checking

| Function | Description |
|----------|-------------|
| `type(v)` | Type name as string |
| `isnull(v)` | True if nil |
| `isbool(v)` | True if boolean |
| `isnum(v)` | True if number |
| `isstr(v)` | True if string |
| `islist(v)` | True if list |
| `istable(v)` | True if table |
| `isfunc(v)` | True if function |
| `isclass(v)` | True if class |
| `isinstance(v)` | True if class instance |
| `isfuture(v)` | True if future |
| `isactor(v)` | True if actor |

### Type Conversion

| Function | Description |
|----------|-------------|
| `tonum(v)` | Convert to number |
| `tostr(v)` | Convert to string |
| `tobool(v)` | Convert to boolean |
| `ord(s)` | Character to ASCII code |
| `chr(n)` | ASCII code to character |

### Debug / Error

| Function | Description |
|----------|-------------|
| `assert(cond, msg)` | Assert condition, error if false |
| `error(msg)` | Raise runtime error |

---

## Native Methods on Built-in Types

### String Methods

```js
var s = "Hello, World!";
s.upper();  // "HELLO, WORLD!"
```

| Method | Description |
|--------|-------------|
| `length()` | String length |
| `upper()` | Convert to uppercase |
| `lower()` | Convert to lowercase |
| `trim()` | Remove leading/trailing whitespace |
| `contains(s)` | Check if contains substring |
| `startsWith(s)` | Check if starts with prefix |
| `endsWith(s)` | Check if ends with suffix |
| `indexOf(s)` | Index of substring (-1 if not found) |
| `substring(start, end)` | Extract substring |
| `split(delim)` | Split into list by delimiter |
| `replace(from, to)` | Replace all occurrences |

### Number Methods

```js
var n = 3.14159;
n.toFixed(2);  // "3.14"
(65).chr();    // "A"
```

| Method | Description |
|--------|-------------|
| `abs()` | Absolute value |
| `floor()` | Round down |
| `ceil()` | Round up |
| `round()` | Round to nearest |
| `trunc()` | Truncate toward zero |
| `sqrt()` | Square root |
| `pow(n)` | Raise to power n |
| `sin()`, `cos()`, `tan()` | Trigonometric functions |
| `asin()`, `acos()`, `atan()` | Inverse trig functions |
| `log()`, `log10()`, `log2()` | Logarithms |
| `exp()` | e^x |
| `sign()` | Returns -1, 0, or 1 |
| `isNan()` | True if NaN |
| `isInf()` | True if infinite |
| `isFinite()` | True if finite |
| `isInt()` | True if integer |
| `toInt()` | Truncate to integer |
| `toString()` | Convert to string |
| `toFixed(n)` | Format with n decimal places |
| `toHex()` | Convert to hex string |
| `toBinary()` | Convert to binary string |
| `chr()` | ASCII code to character |
| `clamp(min, max)` | Clamp to range |
| `lerp(a, b)` | Linear interpolation (this is t) |
| `mod(n)` | Modulo |
| `between(min, max)` | True if in range [min, max] |

### List Methods

```js
var list = [1, 2, 3];
list.push(4);      // [1, 2, 3, 4]
list.reverse();    // [4, 3, 2, 1]
```

| Method | Description |
|--------|-------------|
| `length()` | Number of elements |
| `push(v)` | Add to end, returns list |
| `pop()` | Remove and return last element |
| `shift()` | Remove and return first element |
| `unshift(v)` | Add to beginning, returns list |
| `insert(i, v)` | Insert at index, returns list |
| `remove(i)` | Remove at index, returns removed value |
| `clear()` | Remove all elements, returns list |
| `indexOf(v)` | Index of value (-1 if not found) |
| `contains(v)` | True if contains value |
| `reverse()` | Reverse in place, returns list |
| `slice(start, end)` | Extract sublist |
| `join(sep)` | Join elements with separator |
| `clone()` | Shallow copy |

### Table Methods

```js
var table = ["a": 1, "b": 2];
table.keys();    // ["a", "b"]
table.has("a");  // true
```

| Method | Description |
|--------|-------------|
| `length()` | Number of entries |
| `keys()` | List of all keys |
| `values()` | List of all values |
| `has(key)` | True if key exists |
| `remove(key)` | Remove entry, returns old value or nil |
| `clear()` | Remove all entries, returns table |
| `clone()` | Shallow copy |

### Future Operations

```js
var f = actor.someMethod();
f();  // Block and get result
```

| Operation | Description |
|-----------|-------------|
| `future()` | Call to block and get result |
| `future == null` | Check if pending |
| `if (future)` | Check if ready (truthy) |
| `if (!future)` | Check if error (falsey) |

### Actor Operations

```js
var actor = do SomeClass();
actor.method();  // Returns future
```

| Operation | Description |
|-----------|-------------|
| `actor.method(args)` | Send message, returns future |
| `actor == null` | Check if dead |
| `if (actor)` | Check if alive (truthy) |
