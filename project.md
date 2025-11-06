# Project Guidelines

This document outlines the core principles and coding standards that must be respected when making changes to this project, especially when working with LLM assistance.

## Code Formatting

### OCamlformat (CRITICAL)
**All code must be formatted with OCamlformat using the Jane Street profile.**

- Use `ocamlformat` with the `--profile=janestreet` option
- Maximum line length: **90 characters**
- Formatting is enforced automatically and must be applied before committing code
- Create a `.ocamlformat` file in the project root with:
  ```
  profile = janestreet
  ```

### Jane Street Style Guide Compliance
This project follows the [Jane Street Style Guide](https://opensource.janestreet.com/standards/) conventions:

**Key Formatting Rules:**
- Indentation follows `ocp-indent` rules with the JaneStreet ruleset
- 90-character maximum line length
- Use underscores for identifiers (e.g., `num_apples`, not `numApples`)
- Boolean-returning functions should have predicate names (e.g., `is_valid` not `check_validity`)

**Documentation:**
- Use OCamldoc style comments (`(**`) in `.mli` files
- Enclose small OCaml values in square brackets `[value]`
- Enclose larger code blocks in `{[ code ]}`
- Comments should add useful information beyond what the type signature provides

**Exception Handling:**
- Functions that raise exceptions should be suffixed with `_exn`
- Prefer returning explicit `option` or `Result.t` types over raising exceptions

## Core Principles

### 1. Open Module Files (CRITICAL)
**All files must be open!** This means:
- Use `open Core` at the top of every module
- Leverage Core's extended standard library throughout the codebase
- Maintain consistency with Core's conventions and types

### 2. Type Naming Convention
**The primary type of each module must be named `t`.**

Example:
```ocaml
(* In file formula.ml *)
type t = 
  | And of t * t
  | Or of t * t
  | Not of t
  | Atom of string
```

This follows OCaml best practices and allows for clean type references like `Formula.t`, `Parser.t`, etc.

### 3. Function Parameter Ordering
**All functions must follow this strict parameter order:**

```ocaml
let function_name ?optional1 ?optional2 t ~labeled1 ~labeled2 positional1 positional2 =
  (* implementation *)
```

**Parameter Order Rules:**
1. **Optional parameters** (`?optional1`, `?optional2`) come first
2. **The module's primary type `t`** comes next (unlabeled)
3. **Labeled parameters** (`~labeled1`, `~labeled2`) follow
4. **Positional parameters** come last

**Example:**
```ocaml
let process_formula ?debug ?verbose formula ~mode ~output_format input_string limit =
  (* formula has type t *)
  (* implementation *)
```

## Why These Rules Matter

- **Open Core**: Ensures consistent use of Core's enhanced data structures and functions
- **Type `t`**: Standard OCaml convention for primary module types, enables clean APIs
- **Parameter Ordering**: Provides consistency across the codebase, making it easier to:
  - Understand function signatures at a glance
  - Apply partial application correctly
  - Maintain and refactor code

## Enforcement

When reviewing code or making changes:
1. ✅ Run `ocamlformat --profile=janestreet` on all modified files
2. ✅ Check that `open Core` is present at the top of each module
3. ✅ Verify the primary type is named `t`
4. ✅ Ensure function parameters follow the ordering: `?optional* t ~labeled* positional*`
5. ✅ Verify maximum line length is 90 characters
6. ✅ Check that exception-raising functions are suffixed with `_exn`

**Before committing:**
```bash
# Format all OCaml files
find . -name "*.ml" -o -name "*.mli" | xargs ocamlformat --inplace --profile=janestreet
```

These guidelines are non-negotiable and must be followed in all new code and refactoring efforts.

## Additional Resources

- [Jane Street Style Guide](https://opensource.janestreet.com/standards/)
- [OCamlformat Documentation](https://github.com/ocaml-ppx/ocamlformat)
- [Jane Street Open Source Libraries](https://github.com/janestreet)
