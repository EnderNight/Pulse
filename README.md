# Pulse

A small, mutlipurpose programming language.

For now, only capable of doing simple addition, substractions, multiplications and divisions.

The main goal of the Pulse syntax is readability and not having a lot of reserved words.

# Todo

- [x] Implement a custom Lexer
- [x] Implement a custom Parser
- [x] Implement a custom Executor
- [ ] Implement operators:
    - [x] addition (+, binary)
    - [x] substraction (-, binary)
    - [x] multiplication (*, binary)
    - [x] division (/, binary)
    - [ ] power (^, binary)
    - [x] modulo (%, binary)
- [ ] Implement types:
    - [ ] Integers
    - [ ] Floats
    - [ ] Characters (find usage)
- [ ] Implement variables
- [ ] Implement control blocks
- [ ] Implement loops
- [ ] Implement functions
- [ ] Implement a custom compiler
- [ ] Write a small Usage/Features description in README/Wiki

# Installation

1. Clone this repo
2. Once in the root directory, type ```make```.

This will install *pulse* in your local PATH.

# Usage

You have two possibilities when using Pulse:

1. Write your program in a file

Write your program inside a file with '.pul' as its extension.

Then, execute:
```command
pulse <path_of_your_program>
```
2. Use the cli

```command
pulse <your_program>
```
where <your_program> is a string containing some Pulse code: ```pulse "34+35"```.

# Language Reference

Since the language is still a work in progress, **EVERYTHING** is subject to change.

## Literals

Only integers are supported. If you try to use something else, the interpreter will show you an error.

## Arithmetic

- Plus ('+'): [a: int] '+' [b: int] -> [c: int] adds two numbers
- Minus ('-'): [a: int] '-' [b: int] -> [c: int] substracts two numbers
- Multiply ('\*'): [a: int] '\*' [b: int] -> [c: int] mutliplies two numbers
- Divide ('/'): [a: int] '/' [b: int] -> [c: int] divides two numbers
- Modulo ('%'): [a: int] '%' [b: int] -> [c: int] applies modulo on two numbers

# How to write in Pulse?

For now, you can write in Pulse like you write on a calculator.
