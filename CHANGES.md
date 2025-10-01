# Changelog

All notable changes to this project is documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.0]

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
