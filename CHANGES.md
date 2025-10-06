# Changelog

All notable changes to this project is documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- `|`, `&`, `<<` and `>>` operations for `or`, `and`, `shift left` and `shift right`.
- Arrays:
    - Arrays contain only 64bit integer.
    - Initialized with `let arr = [<length>]`.
    - New `array`, second primitive type.
    - Accessed and written to with `arr[<index>]`, with runtime bounds checking.
    - New runtime api:
        - `Array` type.
        - `array_alloc()`, `array_get()` and `array_set()` which performs direct operations on arrays.
- Standalone expression:
    - Expressions can now be statements (preparing for function calls).
- Turing completness !!
    - With `rule110.pulse` in the examples.

### Changed

- Ast terminologies:
    - `Parsetree` becomes `Astree`, because it actualy is an AST and not a CST.
    - The `--dump-parsetree` flag becomes `--dump-astree`.

## [v0.2.0] - 2025-10-03

### Added

- Ast dump via the dot format:
    - Parsetree with the `--dump-parsetree` flag.
    - Typetree with the `--dump-typetree` flag.
- Internal compilation pipeline.
- Basic type system:
    - The language is strongly and staticaly typed.
    - Types are nominal.
    - First primitive type: `int`, which represents 64bit two's complement integers.

### Changed

- Runtime uses C and the glibc.
- Produced binaries are dynamicaly linked and depends on glibc and libpulsert.

## [v0.1.0] - 2025-10-01

### Added

- Numbers:
    - 64 bit signed integers, represented using two's complement.
    - Only positive numbers can be declared in a program, but negative numbers can still be used.
- Variables:
    - Defined using `let <varname> = <expr>'`.
    - Can only contain numbers.
- Basic arithmetic operations: `+`, `-`, `*`, `/` and `%`, which perform the related math operation.
- Comparisons: `<`, `<=`, `>` and `>=`, which perform the related comparison operations and return `1` in case of success, `0` otherwise.
- Control flow:
    - `if`/`else` branches, which test if the value passed differs from `0` or not, in which case it evaluates the `if` branch or the `else` branch otherwise, if any.
    - `while` loops, which test if the value passed differs from `0` or not, in which case it evaluates its body, or skips it otherwise.
- `print` and `print_int` intrinsincs:
    - `print` prints the ascii representation of the last 8 least significant bits of the passed number.
    - `print_int` prints its passed number in a human readable way. Note that negative numbers are printed as their two's complement positive version.
- Examples, inside the `examples/` directory.
