# Instructions

## `HALT`

Stop the VM.

## `PUSH n`

Push the number `n` on top of the stack.

### Args

- n: A 64 bit unsigned integer

## `ADD`

Add the first two numbers on top of the stack and push the result.

```
a = pop()
b = pop()
push(b + a)
```

## `SUB`

Sub the first two numbers on top of the stack and push the result.

```
a = pop()
b = pop()
push(b - a)
```

## `MULT`

Multiply the first two numbers on top of the stack and push the result.

```
a = pop()
b = pop()
push(b * a)
```

## `DIV`

Divide the first two numbers on top of the stack and push the quotient.

```
a = pop()
b = pop()
push(b / a)
```

## `MOD`

Divide the first two numbers on top of the stack and push the remainder.

```
a = pop()
b = pop()
push(b % a)
```

## `LOAD n`

Push the variable value at index `n` in the variable pool on top of the stack.

```
push(vp[n])
```

### Args

- n: A 16 bit unsigned integer

## `STORE n`

Set the variable value at index `n` in the variable pool to the value on top of the stack.

```
a = pop()
vp[n] = a
```

### Args

- n: A 16 bit unsigned integer

## `PRINT`

Print on stdout the corresponding ASCII representation of the value on top of the stack.

```
a = pop()
print(a)
```

## `JMP a`

Jump to the specified address `a`, unconditionally.

### Args

- a: A 64 bit address

## `JNZ a`

Jump to the specified address `a` if the top stack value is not equal to 0.
Pop the stack.

### Args

- a: A 64 bit address

## `CEQ`

Pop the top value from the stack, `a`, and the second one, `b`, and push 1 if `b` == `a`, 0 otherwise.

```
a = pop()
b = pop()
push(b == a ? 1 : 0)
```

## `CNE`

Pop the top value from the stack, `a`, and the second one, `b`, and push 1 if `b` != `a`, 0 otherwise.

```
a = pop()
b = pop()
push(b != a ? 1 : 0)
```

## `CLT`

Pop the top value from the stack, `a`, and the second one, `b`, and push 1 if `b` < `a`, 0 otherwise.

```
a = pop()
b = pop()
push(b < a ? 1 : 0)
```

## `CLE`

Pop the top value from the stack, `a`, and the second one, `b`, and push 1 if `b` <= `a`, 0 otherwise.

```
a = pop()
b = pop()
push(b <= a ? 1 : 0)
```

## `CGT`

Pop the top value from the stack, `a`, and the second one, `b`, and push 1 if `b` > `a`, 0 otherwise.

```
a = pop()
b = pop()
push(b > a ? 1 : 0)
```

## `CGE`

Pop the top value from the stack, `a`, and the second one, `b`, and push 1 if `b` >= `a`, 0 otherwise.

```
a = pop()
b = pop()
push(b >= a ? 1 : 0)
```
