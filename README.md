# Pulse

A small, mutlipurpose programming language.

For now, only capable of doing simple addition and substractions.

The main goal of the Pulse syntax is readability and not having a lot of reserved words.

# Todo

- [x] Implement a custom Lexer
- [x] Implement a custom Parser
- [x] Implement a custom Executor
- [ ] Implement operators:
    - [x] addition (+, binary)
    - [x] substraction (-, binary)
    - [ ] multiplication (*, binary)
    - [ ] division (/, binary)
    - [ ] power (^, binary)
    - [ ] modulo (%, binary)
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
2. Once in the root directory, type ```make``` or ```make install```.

This will install both *pulse* and *pulcli* in your local PATH.

# Usage

You have two possibilities when using Pulse:

1. Write your program in a file
Write your program inside a file with '.pul' as its extension.

Then, execute:
```command
src/pulse.py <path_of_your_program>
```
2. Use the cli
For now, it may be better for you to use the cli instead of the interpreter.

To use the cli, type:
```command
pulcli <your_program>
```
<your_program> can either be wrapped in quotes or just written as plain text:
```command
pulcli "34 + 35"
```
is equivalent to
```command
pulcli 34 + 35
```

# Language Reference

Since the language is still a work in progress, **EVERYTHING** is subject to change.

## Literals

Only integers are supported. If you try to use something else, the interpreter will show you an error.

## Arithmetic

- Plus ('+'): [a: int] '+' [b: int] -> [c: int] adds two numbers
- Minus ('-'): [a: int] '-' [b: int] -> [c: int] substracts two numbers

# How to write in Pulse?

For now, the interpreter will only understand literals and operators when they aren't next to each other (this "34+35" will fail but this "34 + 35" is ok).
