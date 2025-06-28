# Changelog

All notable changes to this project is documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.0] - 2025-06-28

### Added

- Initial release of Pulse
- Support for:
    - Arithmetic expressions ('+', '-', '*' and '/') with their proper precedence.
    - Stack-based virtual machine, called 'PulseVM', with 6 instructions.
    - Cli with 4 commands: 'compile', 'run', 'exec' and 'disasm'.
    - Custom bytecode format, called 'PulseByc', with serialization/deserialization.
    - File-only compilation.
    - PulseByc-only target.
