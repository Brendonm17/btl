# BTL Language

Bren's Tiny Language - A lightweight scripting language designed for game development with built-in concurrency support.

## Features

- **Clean, familiar syntax** inspired by JavaScript and Lua
- **Classes with inheritance** - single inheritance, constructors, super calls
- **First-class functions** - closures, anonymous functions
- **Built-in data structures** - lists and tables (dictionaries)
- **Native methods** - methods directly on strings, numbers, lists, and tables
- **Switch expressions** - pattern matching with comparison operators
- **Actors and Futures** - built-in concurrency without callbacks
- **Module system** - import statement with native and user modules

---

# BTL Language Grammar

A complete grammar specification for the BTL programming language.

## Notation
```
|       alternation
( )     grouping
*       zero or more
+       one or more
?       optional
"x"     terminal/keyword/symbol
CAPS    token type
```

---

## Program Structure
```ebnf
program        → declaration* EOF

declaration    → classDecl
               | funcDecl
               | varDecl
               | importDecl
               | statement
```

### Class Declaration
```ebnf
classDecl      → "class" IDENTIFIER ( "<" IDENTIFIER )? "{" classBody* "}"

classBody      → fieldDecl
               | methodDecl

fieldDecl      → "var" IDENTIFIER ( "=" expression )? 
                 ( "," IDENTIFIER ( "=" expression )? )* ";"

methodDecl     → "func" IDENTIFIER "(" parameters? ")" block
```

### Function Declaration
```ebnf
funcDecl       → "func" IDENTIFIER "(" parameters? ")" block

parameters     → IDENTIFIER ( "," IDENTIFIER )*
```

### Variable Declaration
```ebnf
varDecl        → "var" IDENTIFIER ( "=" expression )? 
                 ( "," IDENTIFIER ( "=" expression )? )* ";"
```

### Import Declaration
```ebnf
importDecl     → "import" STRING ( "as" IDENTIFIER )? ";"
```

---

## Statements
```ebnf
statement      → exprStmt
               | ifStmt
               | whileStmt
               | forStmt
               | returnStmt
               | breakStmt
               | continueStmt
               | switchStmt
               | block

exprStmt       → expression ";"

ifStmt         → "if" "(" expression ")" statement ( "else" statement )?

whileStmt      → "while" "(" expression ")" statement

forStmt        → "for" "(" forInit? ";" expression? ";" expression? ")" statement
forInit        → varDecl | expression

returnStmt     → "return" expression? ";"

breakStmt      → "break" expression? ";"

continueStmt   → "continue" ";"

block          → "{" declaration* "}"
```

### Switch Statement
```ebnf
switchStmt     → "switch" "(" expression ")" "{" caseClause* defaultClause? caseClause* "}"

caseClause     → "case" caseExpr ( ( "," | "or" | "and" ) caseExpr )* ":" statement*

caseExpr       → comparisonOp expression
               | expression

comparisonOp   → "<" | "<=" | ">" | ">=" | "==" | "!="

defaultClause  → "default" ":" statement*
```

---

## Expressions
```ebnf
expression     → assignment

assignment     → ( call "." )? IDENTIFIER assignOp expression
               | logic_or

assignOp       → "=" | "+=" | "-=" | "*=" | "/=" | "%="
```

### Logical Operators
```ebnf
logic_or       → logic_and ( "or" logic_and )*

logic_and      → equality ( "and" equality )*
```

### Comparison Operators
```ebnf
equality       → comparison ( ( "==" | "!=" ) comparison )*

comparison     → term ( ( "<" | "<=" | ">" | ">=" ) term )*
```

### Arithmetic Operators
```ebnf
term           → factor ( ( "+" | "-" ) factor )*

factor         → unary ( ( "*" | "/" | "%" ) unary )*
```

### Unary and Postfix Operators
```ebnf
unary          → ( "!" | "-" ) unary
               | prefixIncDec
               | postfix

prefixIncDec   → ( "++" | "--" ) ( IDENTIFIER | IDENTIFIER "." IDENTIFIER )

postfix        → call ( "++" | "--" )?
```

### Call Expressions
```ebnf
call           → primary ( "(" arguments? ")" | "." IDENTIFIER | "[" expression "]" )*

arguments      → expression ( "," expression )*
```

### Do Expressions (Actors/Async)
```ebnf
doExpr         → "do" IDENTIFIER "(" arguments? ")"           // Create actor or async call
               | "do" IDENTIFIER "." IDENTIFIER "(" arguments? ")"  // Explicit actor message (optional)
               | "do" "func" "(" parameters? ")" block        // Async anonymous function
```

### Primary Expressions
```ebnf
primary        → "true" | "false" | "null"
               | NUMBER
               | STRING
               | IDENTIFIER
               | "this"
               | "super" "." IDENTIFIER ( "(" arguments? ")" )?
               | "(" expression ")"
               | listLiteral
               | tableLiteral
               | anonymousFunc
               | switchExpr
               | doExpr

listLiteral    → "[" ( expression ( "," expression )* )? "]"

tableLiteral   → "[" ":" "]"
               | "[" expression ":" expression ( "," expression ":" expression )* "]"

anonymousFunc  → "func" "(" parameters? ")" block

switchExpr     → "switch" "(" expression ")" "{" ... "}"
```

---

## Lexical Grammar

### Tokens
```ebnf
NUMBER         → DIGIT+ ( "." DIGIT+ )?

STRING         → '"' ( <any char except '"' and newline> )* '"'

IDENTIFIER     → ALPHA ( ALPHA | DIGIT )*

ALPHA          → "a"..."z" | "A"..."Z" | "_"

DIGIT          → "0"..."9"
```

### Comments
```ebnf
COMMENT        → "//" <any char except newline>* newline
```

---

## Keywords

| Keyword | Description |
|---------|-------------|
| `and` | Logical AND |
| `as` | Import alias |
| `break` | Exit loop or switch (optionally with value) |
| `case` | Switch case label |
| `class` | Class declaration |
| `continue` | Skip to next loop iteration |
| `default` | Switch default label |
| `do` | Create actor or run async function |
| `else` | If-else branch |
| `false` | Boolean false |
| `for` | For loop |
| `func` | Function declaration |
| `if` | Conditional |
| `import` | Import module |
| `null` | Null value |
| `or` | Logical OR |
| `return` | Return from function |
| `super` | Superclass reference |
| `switch` | Switch statement/expression |
| `this` | Current instance |
| `true` | Boolean true |
| `var` | Variable declaration |
| `while` | While loop |

---

## Operators

### Arithmetic

| Operator | Description |
|----------|-------------|
| `+` | Addition / String concatenation |
| `-` | Subtraction / Negation |
| `*` | Multiplication |
| `/` | Division |
| `%` | Modulo |

### Increment/Decrement

| Operator | Description |
|----------|-------------|
| `++` | Increment (prefix or postfix) |
| `--` | Decrement (prefix or postfix) |

### Compound Assignment

| Operator | Description |
|----------|-------------|
| `=` | Assignment |
| `+=` | Add and assign |
| `-=` | Subtract and assign |
| `*=` | Multiply and assign |
| `/=` | Divide and assign |
| `%=` | Modulo and assign |

### Comparison

| Operator | Description |
|----------|-------------|
| `==` | Equal |
| `!=` | Not equal |
| `<` | Less than |
| `<=` | Less than or equal |
| `>` | Greater than |
| `>=` | Greater than or equal |

### Logical

| Operator | Description |
|----------|-------------|
| `!` | Logical NOT |
| `and` | Logical AND (short-circuit) |
| `or` | Logical OR (short-circuit) |

### Other

| Symbol | Description |
|--------|-------------|
| `.` | Property/method access |
| `[]` | Index access / List literal |
| `()` | Call / Grouping |
| `{}` | Block |
| `,` | Separator |
| `;` | Statement terminator |
| `:` | Key-value separator / Case label |

---

## Operator Precedence

From lowest to highest:

| Precedence | Operators | Associativity |
|------------|-----------|---------------|
| 1 | `=` `+=` `-=` `*=` `/=` `%=` | Right |
| 2 | `or` | Left |
| 3 | `and` | Left |
| 4 | `==` `!=` | Left |
| 5 | `<` `<=` `>` `>=` | Left |
| 6 | `+` `-` | Left |
| 7 | `*` `/` `%` | Left |
| 8 | `!` `-` (unary) `++` `--` (prefix) | Right |
| 9 | `()` `.` `[]` `++` `--` (postfix) | Left |

---

## Semantics

### Truthiness

- `false` and `null` are falsey
- Everything else is truthy (including `0` and `""`)
- Futures: `false` if error, `null`-like if pending, `true` if ready
- Actors: `false` if dead, `true` if alive

### String Concatenation

The `+` operator concatenates when either operand is a string:
```js
"Hello " + "World"  // "Hello World"
"Value: " + 42      // "Value: 42"
123 + "!"           // "123!"
```

### Variable Scoping

- Variables are lexically scoped
- Inner scopes can shadow outer variables
- Closures capture variables by reference

### Class Inheritance

- Single inheritance via `<` syntax
- `super` accesses parent class methods
- Fields are declared with `var` in class body
- `init` is the constructor method (auto-generated if fields have initializers)

### Loop Control

- `break` exits the innermost loop or switch
- `continue` skips to the next iteration of the innermost loop

### Switch Semantics

- Cases fall through by default (like C)
- Use `break` to exit a case
- Multiple conditions per case with `,` or `or` (any match)
- Use `and` to require multiple conditions
- Comparison operators allowed in case expressions (e.g., `case >= 90:`)
- `break` without value exits the switch
- `break value` exits and returns a value (switch as expression)

---

## Actors and Futures

BTL has built-in support for concurrent programming using **actors** and **futures**. This enables parallel execution without the complexity of manual thread management or callback hell.

### Creating Actors

Use the `do` keyword before a class instantiation to create an **actor**:

```js
class Counter {
    var count = 0;
    
    func init(start) {
        this.count = start;
    }
    
    func increment() {
        this.count = this.count + 1;
        return this.count;
    }
    
    func getCount() {
        return this.count;
    }
}

// Create an actor - runs on its own thread
var counter = do Counter(10);
```

### Actor Method Calls Return Futures

When you call a method on an actor, it returns a **future** immediately:

```js
var future = counter.increment();  // Returns immediately with a future
var result = future();             // Blocks until result is ready
system.println(result);            // 11
```

### Future States

Futures have three states that can be checked:

```js
var f = counter.getCount();

// Check if pending (not yet complete)
if (f == null) {
    system.println("Still computing...");
}

// Check if ready (has value)
if (f) {
    system.println("Ready!");
}

// Check if error
if (!f) {
    system.println("Error occurred");
}

// Block and get the result
var result = f();
```

### Fire and Forget

You can ignore the returned future if you don't need the result:

```js
counter.increment();  // Fire and forget
counter.increment();
counter.increment();

// Later, get the final state
var count = counter.getCount()();
```

### Async Functions

Use `do` with an anonymous function to run code asynchronously:

```js
var future = do func() {
    var sum = 0;
    for (var i = 1; i <= 1000000; i++) {
        sum = sum + i;
    }
    return sum;
};

// Do other work while computation runs...

var result = future();  // Block and get result
```

### Multiple Actors

Create multiple actors to parallelize work:

```js
class Worker {
    var id;
    
    func init(id) {
        this.id = id;
    }
    
    func compute(n) {
        var result = 0;
        for (var i = 1; i <= n; i++) {
            result = result + i;
        }
        return result;
    }
}

// Create worker pool
var w1 = do Worker(1);
var w2 = do Worker(2);
var w3 = do Worker(3);
var w4 = do Worker(4);

// Start parallel computations
var f1 = w1.compute(1000000);
var f2 = w2.compute(1000000);
var f3 = w3.compute(1000000);
var f4 = w4.compute(1000000);

// Gather results
var total = f1() + f2() + f3() + f4();
```

### Actor State Isolation

Each actor has its own isolated state. Data passed to actors is deep-copied:

```js
var actor1 = do Counter(100);
var actor2 = do Counter(200);

actor1.increment();
actor2.increment();

// Each actor maintains its own state
system.println(actor1.getCount()());  // 101
system.println(actor2.getCount()());  // 201
```

### Summary

| Syntax | Description |
|--------|-------------|
| `do Class(args)` | Create an actor (class instance on its own thread) |
| `do func() { }` | Run anonymous function asynchronously |
| `actor.method()` | Send message to actor, returns future |
| `future()` | Block and get result (or throw on error) |
| `future == null` | Check if future is still pending |
| `if (future)` | Check if future is ready (truthy) |
| `if (!future)` | Check if future has error (falsey) |

---

## Examples

### Hello World
```js
import "system";

system.println("Hello, World!");
```

### Variables
```js
var x = 10;
var a, b, c = 1, 2, 3;
var name = "BTL";
```

### Functions
```js
import "system";

func greet(name) {
    system.println("Hello, " + name + "!");
}

func factorial(n) {
    if (n <= 1) return 1;
    return n * factorial(n - 1);
}

// Anonymous function
var double = func(x) { return x * 2; };

greet("World");
system.println(factorial(5));  // 120
system.println(double(21));    // 42
```

### Classes
```js
import "system";

class Animal {
    var name;
    
    func init(name) {
        this.name = name;
    }
    
    func speak() {
        system.println(this.name + " makes a sound");
    }
}

class Dog < Animal {
    var breed;
    
    func init(name, breed) {
        super.init(name);
        this.breed = breed;
    }
    
    func speak() {
        system.println(this.name + " barks");
    }
}

var dog = Dog("Rex", "German Shepherd");
dog.speak();  // Rex barks
```

### Control Flow
```js
import "system";

var x = 5;

// If-else
if (x > 0) {
    system.println("positive");
} else if (x < 0) {
    system.println("negative");
} else {
    system.println("zero");
}

// While loop
var i = 0;
while (i < 10) {
    if (i == 5) {
        i++;
        continue;  // Skip 5
    }
    system.println(i);
    i++;
}

// For loop
for (var i = 0; i < 10; i++) {
    if (i == 8) break;  // Exit early
    system.println(i);
}
```

### Switch Statement
```js
import "system";

var day = 3;

// Cases fall through without break
switch (day) {
    case 1:
    case 2:
    case 3:
    case 4:
    case 5:
        system.println("weekday");
        break;
    case 6:
    case 7:
        system.println("weekend");
        break;
    default:
        system.println("invalid");
}

// Multiple values with comma (OR logic)
switch (day) {
    case 1, 2, 3, 4, 5:
        system.println("weekday");
        break;
    case 6, 7:
        system.println("weekend");
        break;
}

// Comparison operators in case
var score = 85;
switch (score) {
    case >= 90:
        system.println("A");
        break;
    case >= 80:
        system.println("B");
        break;
    case >= 70:
        system.println("C");
        break;
    default:
        system.println("F");
}
```

### Switch Expression
```js
import "system";

var score = 85;

// Switch as expression with break value
var grade = switch (score) {
    case >= 90: break "A";
    case >= 80: break "B";
    case >= 70: break "C";
    default: break "F";
};

system.println(grade);  // B
```

### Lists and Tables
```js
import "system";

// Lists
var list = [1, 2, 3, 4, 5];
list.push(6);
system.println(list[0]);       // 1
system.println(list.length()); // 6
list[0] = 10;                  // Modify element

// Tables (dictionaries)
var person = ["name": "Alice", "age": 30];
system.println(person["name"]);  // Alice
system.println(person.keys());   // ["name", "age"]
person["city"] = "NYC";          // Add new key

// Empty table
var empty = [:];
```

### Imports and Modules
```js
import "math";
import "random";
import "system";

system.println(math.sqrt(16));    // 4
system.println(math.PI);          // 3.14159...

random.seed(42);
system.println(random.int(1, 100));  // Random integer

system.println(system.platform());   // "linux", "macos", or "windows"
```

### Native Methods
```js
import "system";

// String methods
var s = "  Hello, World!  ";
system.println(s.trim());           // "Hello, World!"
system.println(s.upper());          // "  HELLO, WORLD!  "
system.println(s.split(", "));      // ["  Hello", "World!  "]

// Number methods
var n = 3.14159;
system.println(n.floor());          // 3
system.println(n.toFixed(2));       // "3.14"
system.println((65).chr());         // "A"

// List methods
var list = [3, 1, 4, 1, 5];
list.push(9).reverse();             // Method chaining
system.println(list);               // [9, 5, 1, 4, 1, 3]
system.println(list.join("-"));     // "9-5-1-4-1-3"

// Table methods
var t = ["a": 1, "b": 2];
system.println(t.keys());           // ["a", "b"]
system.println(t.values());         // [1, 2]
system.println(t.has("a"));         // true
```

### Increment/Decrement and Compound Assignment
```js
import "system";

var x = 5;

// Postfix (returns old value)
system.println(x++);  // 5
system.println(x);    // 6

// Prefix (returns new value)
system.println(++x);  // 7
system.println(x);    // 7

// Compound assignment
x += 10;    // x = x + 10
x *= 2;     // x = x * 2
system.println(x);    // 34

// Works with properties too
var obj = ["count": 0];
obj["count"]++;
system.println(obj["count"]);  // 1
```

### Closures
```js
import "system";

func makeCounter() {
    var count = 0;
    return func() {
        count++;
        return count;
    };
}

var counter = makeCounter();
system.println(counter());  // 1
system.println(counter());  // 2
system.println(counter());  // 3
```

### Actors and Concurrency
```js
import "system";

class Calculator {
    var result = 0;
    
    func init() {
        this.result = 0;
    }
    
    func add(n) {
        this.result = this.result + n;
        return this.result;
    }
    
    func multiply(n) {
        this.result = this.result * n;
        return this.result;
    }
    
    func getResult() {
        return this.result;
    }
}

// Create an actor
var calc = do Calculator();

// Method calls return futures
var f1 = calc.add(10);
var f2 = calc.multiply(2);
var f3 = calc.add(5);

// Get results (blocks until ready)
system.println(f1());  // 10
system.println(f2());  // 20
system.println(f3());  // 25

// Async anonymous function
var asyncSum = do func() {
    var sum = 0;
    for (var i = 1; i <= 100; i++) {
        sum = sum + i;
    }
    return sum;
};

system.println(asyncSum());  // 5050
```

### Parallel Processing
```js
import "system";

class Worker {
    func compute(start, end) {
        var sum = 0;
        for (var i = start; i <= end; i++) {
            sum = sum + i;
        }
        return sum;
    }
}

// Create 4 workers
var w1 = do Worker();
var w2 = do Worker();
var w3 = do Worker();
var w4 = do Worker();

// Divide work
var f1 = w1.compute(1, 250000);
var f2 = w2.compute(250001, 500000);
var f3 = w3.compute(500001, 750000);
var f4 = w4.compute(750001, 1000000);

// Combine results
var total = f1() + f2() + f3() + f4();
system.println(total);  // 500000500000
```

### Type Checking and Conversion
```js
import "system";

var value = 42;

system.println(system.type(value));      // "number"
system.println(system.isnum(value));     // true
system.println(system.tostr(value));     // "42"

var str = "123";
system.println(system.tonum(str));       // 123

system.println(system.ord("A"));         // 65
system.println(system.chr(65));          // "A"
```

### Error Handling
```js
import "system";

func divide(a, b) {
    system.assert(b != 0, "Division by zero!");
    return a / b;
}

system.println(divide(10, 2));  // 5
system.println(divide(10, 0));  // Error: Division by zero!
```

### Command Line Arguments
```js
import "system";

system.println("Argument count: " + system.argc());
system.println("Program name: " + system.argv(0));
system.println("All arguments: " + system.args());
```

---

# BTL Native Modules Reference

## **math** module

Mathematical constants and functions.
```js
import "math";
```

### Constants

| Name | Description |
|------|-------------|
| `PI` | π (3.14159265358979323846) |
| `E` | Euler's number (2.71828182845904523536) |
| `TAU` | 2π (6.28318530717958647693) |
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
| `hypot(x, y)` | Hypotenuse (sqrt(x² + y²)) |
| `fmod(a, b)` | Floating-point modulo |
| `deg(rad)` | Convert radians to degrees |
| `rad(deg)` | Convert degrees to radians |

---

## **random** module

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

## **system** module

System utilities, I/O, type checking, and conversions.
```js
import "system";
```

### Time Functions

| Function | Description |
|----------|-------------|
| `clock()` | CPU time in seconds (float) |
| `time()` | Unix timestamp in seconds (float) |
| `millis()` | Milliseconds since epoch |
| `nanos()` | Nanoseconds since epoch |
| `sleep(seconds)` | Pause execution |

### Process Functions

| Function | Description |
|----------|-------------|
| `exit(code)` | Exit with status code |
| `getenv(name)` | Get environment variable (nil if not set) |

### I/O Functions

| Function | Description |
|----------|-------------|
| `write(...)` | Print without newline |
| `println(...)` | Print with newline |
| `input(prompt)` | Read line from stdin |

### Command Line Arguments

| Function | Description |
|----------|-------------|
| `argc()` | Number of arguments |
| `argv(index)` | Get argument at index |
| `args()` | Get all arguments as list |

### System Info

| Function | Description |
|----------|-------------|
| `platform()` | OS name ("linux", "macos", "windows", "unknown") |
| `arch()` | CPU architecture ("x86_64", "arm64", "x86", "arm", "unknown") |
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

### Debug/Error

| Function | Description |
|----------|-------------|
| `assert(cond, msg)` | Assert condition, error if false |
| `error(msg)` | Raise runtime error |

---

## Native Methods on Built-in Types

### **String** methods
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

### **Number** methods
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

### **List** methods
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

### **Table** methods
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

### **Future** methods
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

### **Actor** methods
```js
var actor = do SomeClass();
actor.method();  // Returns future
```

| Operation | Description |
|-----------|-------------|
| `actor.method(args)` | Send message, returns future |
| `actor == null` | Check if dead |
| `if (actor)` | Check if alive (truthy) |

---

## Building

```bash
make          # Build release version
make debug    # Build debug version with tracing
make test     # Run test suite
make clean    # Clean build artifacts
```

### Requirements

- GCC or Clang with C11 support
- POSIX threads (pthread)
- Math library (libm)

---

## License

MIT License - see LICENSE file for details.
