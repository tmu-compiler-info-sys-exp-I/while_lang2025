# while_lang2025

A compiler from the WHILE language to WebAssembly Text Format (.wat). Educational material for the compiler construction course at Tokyo Metropolitan University.

[日本語版はこちら](README.ja.md)

## Overview

This project compiles source code written in the simple imperative **WHILE** language into **WebAssembly Text Format (WAT)**. The compilation pipeline consists of four stages: lexing (ocamllex), parsing (ocamlyacc), conversion to a virtual stack machine intermediate representation, and WebAssembly code generation.

## Prerequisites

- **OCaml 4.11+** — used to build the compiler
  - macOS: `brew install ocaml`
  - Ubuntu: `sudo apt install ocaml ocaml-findlib`
- **(Optional) WebAssembly runtime** — to execute the output `.wat` files
  - [wasmtime](https://wasmtime.dev/), [wasmer](https://wasmer.io/), or a browser

## Build

```bash
# Build the compiler
make

# Build and run day2 exercise tests
make day2

# Compile test .while files
make test
```

On success, the `while_lang` binary is produced.

### Windows

A Windows distribution of OCaml 4.11.1 is bundled as a git submodule in `win64ocaml/`.

```batch
bin\setup.bat    # Fetch the submodule
bin\build.bat    # Build the compiler
bin\run.bat      # Run the compiler
```

## Usage

```bash
# Compile a .while source file
./while_lang test/assign.while

# Output: test/assign.wat

# Run without arguments to see usage
./while_lang
# [usage] ./while_lang filename.while
```

## WHILE Language Syntax

### Arithmetic Expressions
```
<arith> ::= <number> | <variable> | <arith> + <arith>
```

- `Var` — variable reference (`i`, `j`, `x`)
- `Num` — integer literal (`0`, `42`, `-1`)
- `Add` — addition

### Predicates (Boolean Expressions)
```
<pred> ::= true | false
         | not <pred>
         | <pred> and <pred> | <pred> or <pred>
         | <arith> < <arith>
         | GT, GE, LE, EQ are exercises
```

### Statements
```
<stmt> ::= skip
         | <id> := <arith>
         | print <arith>
         | <stmt> ; <stmt>
         | begin <stmt> end
         | while <pred> do <stmt>
```

### Sample Programs

**test/assign.while** — assignment and printing:
```
i := 1;
j := 2;
print i + j;
```

**test/loop.while** — nested while loops:
```
i := 0;

while i < 10 do
  begin
    i := i + 1;
    j := 0;
    while j < 10 do
    begin
      j := j + 1;
      print j;
    end;
  end;

print i;
print j;
```

## Project Structure

```
while_lang2025/
├── main.ml               # Entry point (orchestrates compilation)
├── syntax.ml             # AST data type definitions (arith, predicate, stmt)
├── lexer.mll             # ocamllex lexer specification
├── parser.mly            # ocamlyacc parser specification
├── virtual_stack.ml      # Compilation to virtual stack machine IR
├── emit_wasm.ml          # Virtual stack IR → WebAssembly text format
├── error.ml              # Parse error visualization
├── visualizer.ml         # Pretty printer for AST and stack code
├── test_day2.ml          # Exercise test harness
├── Makefile              # Build configuration
├── OCamlMakefile         # OCaml Makefile framework
├── bin/                  # Windows batch scripts
├── test/                 # Sample WHILE programs
└── win64ocaml/           # Windows OCaml distribution (submodule)
```

## Compiler Pipeline

```
Source code (.while)
  │
  ▼ lexer.mll (ocamllex)
Token stream
  │
  ▼ parser.mly (ocamlyacc)
AST (syntax.ml)
  │
  ▼ virtual_stack.ml
Virtual stack instructions
  │
  ▼ emit_wasm.ml
WebAssembly Text Format (.wat)
```

### Virtual Stack Instructions

| Instruction | Description |
|-------------|-------------|
| `Push n` | Push integer constant n |
| `RValue id` | Push value of variable id |
| `LPush id` | Pop stack top and assign to variable id |
| `PLUS`, `MINUS`, `TIMES`, `DIV` | Binary arithmetic operations |
| `EQ`, `LT`, `LE`, `GT`, `GE` | Comparison operations |
| `NOT`, `AND`, `OR` | Logical operations |
| `LabelTest`, `LabelOut`, `GoTo`, `GoFalse` | Control flow (for while loops) |
| `PRINT` | Output |

### WASM Code Generation

The generated `.wat` file defines WHILE program variables as mutable globals and exports the compiled program as a `$main` function. The `print` function is imported from the host environment.

## Exercises

This project contains TODO items for students to implement.

1. **Exercise 1** (`syntax.ml`, `lexer.mll`, `parser.mly`, `virtual_stack.ml`, `emit_wasm.ml`): Add `Sub`, `Mul`, `Div` operators
2. **Exercise 2** (`syntax.ml` etc.): Add `GT`, `GE`, `LE`, `EQ` comparison operators
3. **Exercise 3** (`syntax.ml` etc.): Add `Block`, `Seq`, `While` constructs

Each exercise requires consistent changes across the AST type definitions, lexer, parser, virtual stack code generation, and WebAssembly emission.

## Clean

```bash
make clean        # Remove build artifacts
make clean_test   # Remove test output (.pyc, .res)
```

## References

- [WHILE Language Resources (CMU)](https://www.cs.cmu.edu/~aldrich/courses/15-819O-13sp/resources/)
- [WebAssembly Specification](https://webassembly.github.io/spec/)
- [OCamlMakefile](https://github.com/mmottl/ocaml-makefile)
