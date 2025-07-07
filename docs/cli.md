# cli

`pulse [OPTIONS] COMMAND`

## Common options

### `--help`

Show the cli help on the standard output.

## Commands

### `compile [-o OUTPUT_FILE] ARG`

Compile a pulse program named ARG and write the bytecode in OUTPUT_FILE ("a.pulsebyc" if not given).

## Return code

- `0` in case of success.
- `1` in case of an error, with a proper error message.
