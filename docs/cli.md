# cli

`pulse [OPTIONS] COMMAND`

## Common options

### `--help`

Show the cli help on the standard output.

## Commands

### `compile [-o OUTPUT_FILE] ARG`

Compile a pulse program named ARG and write the bytecode in OUTPUT_FILE ("a.pulsebyc" if not given).

### `run ARG`

Run a pulse bytecode program named ARG.
ARG must be the result of the `compile` command.

### `exec ARG`

Compile a pulse program named ARG and run it right after.
Does not create any temporary files

### `disasm ARG`

Disassemble a pulse bytecode program named ARG and show the instructions on the standard output.
See [instructions.md](instructions.md) for a description of each VM instructions
