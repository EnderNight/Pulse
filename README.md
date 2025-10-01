# Pulse

Simple programming language.

OCaml frontend, [QBE](https://c9x.me/compile/) backend.

Only for x86_64-linux.

## Usage

```sh
pulse INPUT_FILE.pulse OUTPUT_EXE
```

where:
- `INPUT_FILE.pulse` is the source file name.
- `OUTPUT_EXE` is the name of the final executable.

The `PULSE_RUNTIMEDIR` env variable, which points to the directory containing the Pulse runtime (defaults to `/usr/lib`), can be overwritten.
