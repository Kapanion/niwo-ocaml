# niwo-ocaml

NIWO2 implementation in OCaml.

## Description

This project implements the NIWO2 tool in OCaml, starting with basic functionality to read and process transition system files.

## Setup

1. Ensure you have OCaml and Dune installed.
2. Clone or navigate to the project directory.
3. Run `dune build` to build the project.
4. Run `dune test` to run the tests.

## Usage

To run the tool:

```
dune exec bin/main.exe <transition_system_file>
```

The tool will output the number of lines in the specified file.

## Development

- Library code is in `src/`
- Executable in `bin/`
- Tests in `test/`