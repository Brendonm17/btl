# BTL Language Reference

## Program Structure

A BTL program is a sequence of declarations and statements, executed top to bottom.

```ebnf
program        -> declaration* EOF

declaration    -> classDecl
               | funcDecl
               | varDecl
               | importDecl
               | statement
```

---

## Data Types

| Type | Description | Examples |
|------|-------------|---------|
| `null` | Absence of a value | `null` |
| Boolean | `true` or `false` | `true`, `false` |
| Number | IEEE 754 double-precision float | `42`, `3.14`, `-1` |
| String | Immutable text | `"hello"`, `""` |
| List | Ordered, mutable collection | `[1, 2, 3]` |
| Table | Key-value dictionary | `["name": "Alice"]` |
| Function | First-class callable | `func(x) { return x; }` |
| Class | Blueprint for instances | `class Foo { }` |
| Instance | Object created from a class | `Foo()` |
| Actor | Concurrent class instance | `do Foo()` |
| Future | Pending async result | `actor.method()` |

---

## Variables

Declare variables with `var`. Uninitialized variables default to `null`.

```js
var x = 10;
var name = "BTL";
var unset;           // null
```

Multiple variables can be declared in a single statement:

```js
var a, b, c = 1, 2, 3;
```

### Scoping

- Variables are lexically scoped
- Inner scopes can shadow outer variables
- Closures capture variables by reference

```ebnf
varDecl -> "var" IDENTIFIER ( "=" expression )?
           ( "," IDENTIFIER ( "=" expression )? )* ";"
```

---

## Functions

### Named Functions

```js
func greet(name) {
    system.println("Hello, " + name + "!");
}

func factorial(n) {
    if (n <= 1) return 1;
    return n * factorial(n - 1);
}
```

### Anonymous Functions

Functions are first-class values. Assign them to variables, pass them as arguments, or return them from other functions.

```js
var double = func(x) { return x * 2; };
```

### Closures

Functions capture their surrounding scope by reference:

```js
func makeCounter() {
    var count = 0;
    return func() {
        count++;
        return count;
    };
}

var counter = makeCounter();
counter();  // 1
counter();  // 2
```

```ebnf
funcDecl       -> "func" IDENTIFIER "(" parameters? ")" block
parameters     -> IDENTIFIER ( "," IDENTIFIER )*
anonymousFunc  -> "func" "(" parameters? ")" block
```

---

## Classes

Classes support single inheritance, fields, methods, and constructors.

```js
class Animal {
    var name;

    func init(name) {
        this.name = name;
    }

    func speak() {
        system.println(this.name + " makes a sound");
    }
}
```

### Inheritance

Use `<` to inherit from a parent class. Call `super.method()` to access parent methods.

```js
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

### Constructors

The `init` method is the constructor. If fields have initializers and no `init` is defined, one is auto-generated.

```ebnf
classDecl  -> "class" IDENTIFIER ( "<" IDENTIFIER )? "{" classBody* "}"
classBody  -> fieldDecl | methodDecl
fieldDecl  -> "var" IDENTIFIER ( "=" expression )?
              ( "," IDENTIFIER ( "=" expression )? )* ";"
methodDecl -> "func" IDENTIFIER "(" parameters? ")" block
```

---

## Control Flow

### If / Else

```js
if (x > 0) {
    system.println("positive");
} else if (x < 0) {
    system.println("negative");
} else {
    system.println("zero");
}
```

### While Loop

```js
var i = 0;
while (i < 10) {
    system.println(i);
    i++;
}
```

### For Loop

```js
for (var i = 0; i < 10; i++) {
    system.println(i);
}
```

### Break and Continue

- `break` exits the innermost loop or switch
- `continue` skips to the next iteration of the innermost loop

```js
for (var i = 0; i < 10; i++) {
    if (i == 5) continue;  // skip 5
    if (i == 8) break;     // stop at 8
    system.println(i);
}
```

```ebnf
ifStmt       -> "if" "(" expression ")" statement ( "else" statement )?
whileStmt    -> "while" "(" expression ")" statement
forStmt      -> "for" "(" forInit? ";" expression? ";" expression? ")" statement
forInit      -> varDecl | expression
returnStmt   -> "return" expression? ";"
breakStmt    -> "break" expression? ";"
continueStmt -> "continue" ";"
block        -> "{" declaration* "}"
```

---

## Switch Statements

Cases fall through by default (like C). Use `break` to exit a case.

```js
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
```

### Multiple Values per Case

Use `,` or `or` for any-match logic, `and` for all-match:

```js
switch (day) {
    case 1, 2, 3, 4, 5:
        system.println("weekday");
        break;
    case 6, 7:
        system.println("weekend");
        break;
}
```

### Comparison Operators in Cases

```js
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

### Switch Expressions

Use `break value` to return a value from a switch:

```js
var grade = switch (score) {
    case >= 90: break "A";
    case >= 80: break "B";
    case >= 70: break "C";
    default: break "F";
};
```

```ebnf
switchStmt    -> "switch" "(" expression ")" "{" caseClause* defaultClause? caseClause* "}"
caseClause    -> "case" caseExpr ( ( "," | "or" | "and" ) caseExpr )* ":" statement*
caseExpr      -> comparisonOp expression | expression
comparisonOp  -> "<" | "<=" | ">" | ">=" | "==" | "!="
defaultClause -> "default" ":" statement*
```

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

### Increment / Decrement

| Operator | Description |
|----------|-------------|
| `++` | Increment (prefix or postfix) |
| `--` | Decrement (prefix or postfix) |

Postfix returns the old value, prefix returns the new value:

```js
var x = 5;
system.println(x++);  // 5 (returns old, then increments)
system.println(++x);  // 7 (increments, then returns new)
```

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

### Other Symbols

| Symbol | Description |
|--------|-------------|
| `.` | Property/method access |
| `[]` | Index access / List literal |
| `()` | Call / Grouping |
| `{}` | Block |
| `,` | Separator |
| `;` | Statement terminator |
| `:` | Key-value separator / Case label |

### Operator Precedence

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

## Imports and Modules

Import native or user modules with the `import` statement:

```js
import "system";
import "math";
import "random";
```

User modules are BTL files:

```js
import "path/to/module.btl" as mymod;
```

```ebnf
importDecl -> "import" STRING ( "as" IDENTIFIER )? ";"
```

See [STDLIB.md](STDLIB.md) for the full reference of built-in modules.

---

## Lists and Tables

### Lists

Ordered, mutable, zero-indexed collections:

```js
var list = [1, 2, 3, 4, 5];
list.push(6);
system.println(list[0]);        // 1
system.println(list.length());  // 6
list[0] = 10;                   // modify element
```

### Tables

Key-value dictionaries:

```js
var person = ["name": "Alice", "age": 30];
system.println(person["name"]);  // Alice
system.println(person.keys());   // ["name", "age"]
person["city"] = "NYC";          // add new key

// Empty table
var empty = [:];
```

```ebnf
listLiteral  -> "[" ( expression ( "," expression )* )? "]"
tableLiteral -> "[" ":" "]"
             | "[" expression ":" expression ( "," expression ":" expression )* "]"
```

See [STDLIB.md](STDLIB.md) for all native methods on lists and tables.

---

## Actors and Futures

BTL has built-in support for concurrent programming using actors and futures. This enables parallel execution without manual thread management or callbacks.

### Creating Actors

Use the `do` keyword before a class instantiation to create an actor. The actor runs on its own thread with isolated state.

```js
class Counter {
    var count = 0;

    func increment() {
        this.count = this.count + 1;
        return this.count;
    }

    func getCount() {
        return this.count;
    }
}

var counter = do Counter();
```

### Futures

Method calls on actors return futures immediately:

```js
var future = counter.increment();  // returns immediately
var result = future();             // blocks until result is ready
system.println(result);            // 1
```

### Future States

| Check | Meaning |
|-------|---------|
| `future == null` | Pending (not yet complete) |
| `if (future)` | Ready (has a value) |
| `if (!future)` | Error occurred |
| `future()` | Block and get the result |

### Fire and Forget

Ignore the returned future if you don't need the result:

```js
counter.increment();  // fire and forget
counter.increment();
var count = counter.getCount()();  // get final state
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

var result = future();  // block and get result
```

### Actor State Isolation

Each actor has its own isolated state. Data passed to actors is deep-copied.

```ebnf
doExpr -> "do" IDENTIFIER "(" arguments? ")"
        | "do" IDENTIFIER "." IDENTIFIER "(" arguments? ")"
        | "do" "func" "(" parameters? ")" block
```

| Syntax | Description |
|--------|-------------|
| `do Class(args)` | Create an actor (class instance on its own thread) |
| `do func() { }` | Run anonymous function asynchronously |
| `actor.method()` | Send message to actor, returns future |
| `future()` | Block and get result (or throw on error) |

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

### Comments

Single-line comments start with `//`:

```js
// This is a comment
var x = 10;  // inline comment
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

## Full Grammar

### Expressions

```ebnf
expression     -> assignment

assignment     -> ( call "." )? IDENTIFIER assignOp expression
               | logic_or

assignOp       -> "=" | "+=" | "-=" | "*=" | "/=" | "%="

logic_or       -> logic_and ( "or" logic_and )*
logic_and      -> equality ( "and" equality )*
equality       -> comparison ( ( "==" | "!=" ) comparison )*
comparison     -> term ( ( "<" | "<=" | ">" | ">=" ) term )*
term           -> factor ( ( "+" | "-" ) factor )*
factor         -> unary ( ( "*" | "/" | "%" ) unary )*

unary          -> ( "!" | "-" ) unary
               | prefixIncDec
               | postfix

prefixIncDec   -> ( "++" | "--" ) ( IDENTIFIER | IDENTIFIER "." IDENTIFIER )
postfix        -> call ( "++" | "--" )?

call           -> primary ( "(" arguments? ")" | "." IDENTIFIER | "[" expression "]" )*
arguments      -> expression ( "," expression )*

primary        -> "true" | "false" | "null"
               | NUMBER | STRING | IDENTIFIER
               | "this"
               | "super" "." IDENTIFIER ( "(" arguments? ")" )?
               | "(" expression ")"
               | listLiteral | tableLiteral
               | anonymousFunc | switchExpr | doExpr
```

### Lexical Grammar

```ebnf
NUMBER     -> DIGIT+ ( "." DIGIT+ )?
STRING     -> '"' ( <any char except '"' and newline> )* '"'
IDENTIFIER -> ALPHA ( ALPHA | DIGIT )*
ALPHA      -> "a"..."z" | "A"..."Z" | "_"
DIGIT      -> "0"..."9"
COMMENT    -> "//" <any char except newline>* newline
```

### Grammar Notation

```
|       alternation
( )     grouping
*       zero or more
+       one or more
?       optional
"x"     terminal/keyword/symbol
CAPS    token type
```
