# Parser Mini-Language

This project implements a simple parser mini-language in both **functional** and **imperative** paradigms using F#. The interpreter supports basic arithmetic operations, variable assignments, conditional statements (`if`), and loops (`while`).

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
2. **Arithmetic Operations**:
   ```plaintext
    x = (5 + 3) * 2
3. **Conditional Statements**:
   ```plaintext
   if x > 0 then x = x - 1
4. **Loops**:
   ```plaintext
   while x < 5 then x = x + 1
5. **Multiple Statements**:
   ```plaintext
   x = 5 + 3; y = x * 2
   
## Example Output

Here is the output of the program for the input `x = 10; y = 20; z = 0; if y > x then z = 100`:

```plaintext
------------------------------------------------------------------------------------------------

Input: x = 10; y = 20; z = 0; if y > x then z = 100 else z = 0
Tokens: [VariableToken "x"; AssignToken; NumberToken 10; SemicolonToken;
 VariableToken "y"; AssignToken; NumberToken 20; SemicolonToken;
 VariableToken "z"; AssignToken; NumberToken 0; SemicolonToken; IfToken;
 VariableToken "y"; ComparisonToken ">"; VariableToken "x"; ThenToken;
 VariableToken "z"; AssignToken; NumberToken 100; ElseToken; VariableToken "z";
 AssignToken; NumberToken 0]

--- (AST) ---
Assignment(x)
  Number(10)
Assignment(y)
  Number(20)
Assignment(z)
  Number(0)
IfStatement(
  Condition:
    Comparison(>)
      Variable(y)
      Variable(x)
  If Block:
    Assignment(z)
      Number(100)
  Else Block:
    Assignment(z)
      Number(0)
)

--- Program Execution ---
Final symbol table: map [("x", 10); ("y", 20); ("z", 100)]
```

Here is the output of the program for the input `x = 0; while x < 5 then x = x + 1`:

```plaintext
------------------------------------------------------------------------------------------------

Input: x = 0; while x < 5 then x = x + 1
Tokens: [VariableToken "x"; AssignToken; NumberToken 0; SemicolonToken; WhileToken;
 VariableToken "x"; ComparisonToken "<"; NumberToken 5; ThenToken;
 VariableToken "x"; AssignToken; VariableToken "x"; OperatorToken "+";
 NumberToken 1]

--- (AST) ---
Assignment(x)
  Number(0)
WhileLoop(
  Condition:
    Comparison(<)
      Variable(x)
      Number(5)
  Block:
    Assignment(x)
      Add(
        Variable(x)
        Number(1)
      )
)

--- Program Execution ---
Final symbol table: map [("x", 5)]
```

Here is the output of the program for the input `x = 5 + 3 y = x * 2;`:

```plaintext
------------------------------------------------------------------------------------------------

Input: x = 5 + 3 y = x * 2;
Tokens: [VariableToken "x"; AssignToken; NumberToken 5; OperatorToken "+"; NumberToken 3;
 VariableToken "y"; AssignToken; VariableToken "x"; OperatorToken "*";
 NumberToken 2; SemicolonToken]

--- (AST) ---
Error: Expected semicolon after statement
```

Here is the output of the program for the input `x = z + 2`:

```plaintext
------------------------------------------------------------------------------------------------

Input: x = z + 2
Tokens: [VariableToken "x"; AssignToken; VariableToken "z"; OperatorToken "+";
 NumberToken 2]

--- (AST) ---
Assignment(x)
  Add(
    Variable(z)
    Number(2)
  )

--- Program Execution ---
Error: Undefined variable: z
```

Here is the output of the program for the input `x = 10 / 0`:

```plaintext
------------------------------------------------------------------------------------------------

Input: x = 10 / 0
Tokens: [VariableToken "x"; AssignToken; NumberToken 10; OperatorToken "/";
 NumberToken 0]

--- (AST) ---
Assignment(x)
  Divide(
    Number(10)
    Number(0)
  )

--- Program Execution ---
Error: Attempted to divide by zero.
```
