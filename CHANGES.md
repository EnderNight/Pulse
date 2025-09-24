# Changelog

All notable changes to this project is documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- New `%` operand:
    - Get the remainder of an euclidian division.
    - New `MOD` instruction.
- New `==`, `!=`, `<`, `<=`, `>`, `>=` operands:
    - Compare two numbers.
    - New `Cxx` comparison instruction family.
- Support for `if - else` conditions:
    - New `JMP` and `JNZ` instructions for (un)conditional jumps.
- Support for `while` loops.
- New assignation `=` expression:
    - Assign new values to variables.

### Changed

- docs/instructions.md instruction description for easier reading.

## [0.2.0] - 2025-06-30

### Added

- PulseVM runtime checks for division by zero.
- Support for variables:
    - Defined with 'let id = expr;' with proper error handling.
    - New Bindtree with a binder that maps variable usage to their definition.
    - New `LOAD` and `STORE` vm instructions as well as a 'variable pool' that contains variable values.
    - New bytecode format, with a header containing the compiler version.
- 'print' command:
    - New 'print expr' command that prints the ASCII representation of 'expr'.
    - New `PRINT` vm instruction.

### Changed

- Add 'linux' to release archive name.
- Error handling and reporting, with proper exit code in case of an error.

## [0.1.0] - 2025-06-28

### Added

- Initial release of Pulse
- Support for:
    - Arithmetic expressions ('+', '-', '*' and '/') with their proper precedence.
    - Priority with parenthesis.
    - Stack-based virtual machine, called 'PulseVM', with 6 instructions.
    - Cli with 4 commands: 'compile', 'run', 'exec' and 'disasm'.
    - Custom bytecode format, called 'PulseByc', with serialization/deserialization.
    - File-only compilation.
    - PulseByc-only target.
