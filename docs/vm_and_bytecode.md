# VM and Bytecode

## VM

The vm is called 'PulseVM' and has its instruction set is defined here: [instructions.md](./instructions.md).

The vm consists of two main parts:

- A stack: containing only 64 bit unsigned integers.
- A variable pool: an array containing only 64 bit unsigned integers.

## Bytecode

The bytecode is called 'PulseByc' and has this format:

```
{
  header : bytecode_header,
  instructions : bytecode_instruction list
}
```

### Header

The bytecode_header has this format:

```
{
  major : u2,
  minor : u2,
  patch : u2,
  variable_pool_count : u2
}
```

Where:

- major, minor and patch represents the compiler version that compiled this bytecode.
- variable_pool_count represents the number of variables to allocate.

### Bytecode instructions

The bytecode_instruction has this format:

```
{
  instruction_code : u1,
  ...
}
```
