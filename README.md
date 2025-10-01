# Pulse

Simple programming language.

OCaml frontend, [QBE](https://c9x.me/compile/) backend.

Only for x86_64-linux.

## Usage

```sh
pulse [OPTIONS] INFILE OUTFILE
```

Options:
- `--dump-parstree`: shows the dot representation of the current parsetree and stops the pipeline.

Arguments:
- `INFILE`: the source file name.
- `OUTFILE`: the name of the final executable.

Env variables:
- `PULSE_RUNTIMEDIR`: points to the directory containing the Pulse runtime (defaults to `/usr/lib`).
