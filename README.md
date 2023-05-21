# Pulse

A small, mutlipurpose programming language.

For now, only capable of doing simple addition and substractions.

# Todo

- [x] Implement a custom Lexer
- [x] Implement a custom Parser
- [x] Implement a custom Executor
- [ ] Implement operators:
    - [x] addition (+, binary)
    - [x] substraction (-, binary)
    - [ ] multiplication (*, binary)
    - [ ] division (/, binary)
    - [ ] power (**, binary)
- [ ] Implement types:
    - [ ] Integers
    - [ ] Floats
    - [ ] Characters (find usage)
- [ ] Implement functions
- [ ] Implement a custom compiler
- [ ] Write a small Usage/Features description in README/Wiki

# Usage

Firstly, write your program inside a file with '.pul' as its extension.

Then, execute:
```command
python src/pulse.py <path_of_your_program>
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
