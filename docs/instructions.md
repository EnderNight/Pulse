# Instructions

## `HALT`

### Args

None.

### Description

Stop the VM.

## `PUSH n`

### Args

- n: A 64 bit unsigned integer

### Description

Push the number 'n' on top of the stack.

## `ADD`

### Args

None.

### Description

Add the first two numbers on top of the stack and push the result.

## `SUB`

### Args

None.

### Description

Sub the first two numbers on top of the stack and push the result.

## `MULT`

### Args

None.

### Description

Multiply the first two numbers on top of the stack and push the result.

## `DIV`

### Args

None.

### Description

Divide the first two numbers on top of the stack and push the result.

## `LOAD n`

### Args

- n: A 16 bit unsigned integer

### Description

Push the variable value at index `n` in the variable pool on top of the stack.

## `STORE n`

### Args

- n: A 16 bit unsigned integer

### Description

Set the variable value at index `n` in the variable pool to the value on top of the stack.
Pop the stack.
