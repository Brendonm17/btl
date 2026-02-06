# BTL Examples

A collection of code examples demonstrating BTL's features.

---

## Hello World

```js
import "system";

system.println("Hello, World!");
```

---

## Variables

```js
var x = 10;
var a, b, c = 1, 2, 3;
var name = "BTL";
```

---

## Functions

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

---

## Closures

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

---

## Classes

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

---

## Control Flow

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

---

## Switch Statement

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

---

## Switch Expression

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

---

## Lists and Tables

```js
import "system";

// Lists
var list = [1, 2, 3, 4, 5];
list.push(6);
system.println(list[0]);        // 1
system.println(list.length());  // 6
list[0] = 10;                   // Modify element

// Tables (dictionaries)
var person = ["name": "Alice", "age": 30];
system.println(person["name"]);  // Alice
system.println(person.keys());   // ["name", "age"]
person["city"] = "NYC";          // Add new key

// Empty table
var empty = [:];
```

---

## Imports and Modules

```js
import "math";
import "random";
import "system";

system.println(math.sqrt(16));       // 4
system.println(math.PI);             // 3.14159...

random.seed(42);
system.println(random.int(1, 100));  // Random integer

system.println(system.platform());   // "linux", "macos", or "windows"
```

---

## Native Methods

```js
import "system";

// String methods
var s = "  Hello, World!  ";
system.println(s.trim());            // "Hello, World!"
system.println(s.upper());           // "  HELLO, WORLD!  "
system.println(s.split(", "));       // ["  Hello", "World!  "]

// Number methods
var n = 3.14159;
system.println(n.floor());           // 3
system.println(n.toFixed(2));        // "3.14"
system.println((65).chr());          // "A"

// List methods
var list = [3, 1, 4, 1, 5];
list.push(9).reverse();              // Method chaining
system.println(list);                // [9, 5, 1, 4, 1, 3]
system.println(list.join("-"));      // "9-5-1-4-1-3"

// Table methods
var t = ["a": 1, "b": 2];
system.println(t.keys());            // ["a", "b"]
system.println(t.values());          // [1, 2]
system.println(t.has("a"));          // true
```

---

## Increment / Decrement and Compound Assignment

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

---

## Type Checking and Conversion

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

---

## Error Handling

```js
import "system";

func divide(a, b) {
    system.assert(b != 0, "Division by zero!");
    return a / b;
}

system.println(divide(10, 2));  // 5
system.println(divide(10, 0));  // Error: Division by zero!
```

---

## Command Line Arguments

```js
import "system";

system.println("Argument count: " + system.argc());
system.println("Program name: " + system.argv(0));
system.println("All arguments: " + system.args());
```

---

## Actors and Concurrency

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

---

## Parallel Processing

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
