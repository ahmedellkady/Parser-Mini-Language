# Parser Mini-Language

This project implements a simple parser mini-language in both **functional** and **imperative** styles using F#. The interpreter supports basic arithmetic operations, variable assignments, conditional statements (`if`), and loops (`while`).

## Features

- **Functional Implementation**:
  - Uses immutable data structures and recursive functions.
  - Symbol table is implemented using `Map<string, int>`.
  - Emphasizes pure functions and declarative programming.

- **Imperative Implementation**:
  - Uses mutable data structures (`Dictionary<string, int>`) and loops.
  - Symbol table is implemented using a mutable `Dictionary`.
  - Emphasizes imperative programming with side effects.

## Language Syntax

The mini-language supports the following constructs:

1. **Variable Assignment**:
   ```plaintext
   x = 5 + 3
