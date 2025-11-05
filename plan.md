# Plan for Implementing FOLTL Formula Data Structure in OCaml

## Overview
This plan outlines the implementation of the FOLTL (First-Order Linear Temporal Logic) formula data structure in OCaml, based on the Scala implementation in `FOLTL.scala`. The implementation will use OCaml's algebraic data types (variants) to represent the formula hierarchy. It also includes a parser implementation based on `FormulaParser.scala`.

## Proposed File Structure

```
niwo-ocaml/
├── bin/
│   ├── dune
│   └── main.ml                    # Main executable
├── src/
│   ├── dune                       # Library configuration
│   ├── niwo.ml                    # Top-level module (file utilities + re-exports)
│   ├── niwo.mli                   # Top-level interface
│   ├── foltl.ml                   # Formula data structure
│   ├── foltl.mli                  # Formula interface
│   ├── formula_parser.ml          # Parser implementation
│   └── formula_parser.mli         # Parser interface
├── test/
│   ├── dune                       # Test configuration
│   ├── test_niwo.ml               # Basic tests
│   ├── test_foltl.ml              # Formula tests
│   └── test_parser.ml             # Parser tests
├── .gitignore
├── dune-project
├── niwo-ocaml.opam
├── plan.md
├── README.md
└── test.txt                       # Test formula file (manually editable)
```

## Core Formula Type Structure

### 1. Create `src/foltl.ml` and `src/foltl.mli`

The formula type should be implemented as a recursive variant type with the following structure:

```ocaml
type var = {
  name : string;
  typ : string;  (* default: "T" *)
}

type formula =
  (* Base cases *)
  | True
  | False
  | Var of var
  | Fun of string * string option * var list
    (* name, optional index, parameters *)
  
  (* Propositional logic *)
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Implies of formula * formula
  | Equiv of formula * formula
  | Equal of formula * formula
  
  (* Quantifiers *)
  | Exists of var list * formula
  | Forall of var list * formula
  | ForallOtherThan of var list * var list * formula
    (* vars, otherthan, formula *)
  
  (* Temporal operators *)
  | Next of formula
  | Globally of formula
  | Finally of formula
  | Until of formula * formula
  | WUntil of formula * formula  (* Weak Until *)
```

### 2. Implement Core Constructor Functions

Create smart constructors in `src/foltl.ml`:

```ocaml
(* Variable constructor with default type *)
let mk_var ?(typ="T") name = { name; typ }

(* Function constructor with simplified signature *)
let mk_fun name params = Fun (name, None, params)
let mk_fun_indexed name index params = Fun (name, Some index, params)

(* Binary operators that handle lists *)
let mk_and_list formulas =
  (* Filter out True, return False if any False found *)
  (* Flatten nested Ands, remove duplicates *)
  match List.filter ((<>) True) formulas with
  | [] -> True
  | [f] -> f
  | fs when List.mem False fs -> False
  | fs -> List.fold_left (fun acc f -> And (acc, f)) (List.hd fs) (List.tl fs)

let mk_or_list formulas =
  (* Filter out False, return True if any True found *)
  (* Flatten nested Ors, remove duplicates *)
  match List.filter ((<>) False) formulas with
  | [] -> False
  | [f] -> f
  | fs when List.mem True fs -> True
  | fs -> List.fold_left (fun acc f -> Or (acc, f)) (List.hd fs) (List.tl fs)

(* Similar for Equiv, Equal, Implies *)
let mk_implies_list formulas = (* ... *)
let mk_equiv_list formulas = (* ... *)
let mk_equal_list formulas = (* ... *)

(* Quantifier constructors that handle empty lists *)
let mk_exists vars formula =
  if vars = [] then formula else Exists (vars, formula)

let mk_forall vars formula =
  if vars = [] then formula else Forall (vars, formula)

let mk_forall_other_than vars otherthan formula =
  if otherthan = [] || vars = [] then mk_forall vars formula
  else ForallOtherThan (vars, otherthan, formula)
```

### 3. Implement String Conversion Functions

```ocaml
(* Convert var to string with type annotation *)
let var_to_string_typed var = var.name ^ ":" ^ var.typ

(* Convert var to string without type *)
let var_to_string var = var.name

(* Convert formula to string with proper bracketing *)
let rec to_string formula = (* ... *)

(* Convert formula to string with type annotations *)
let rec to_typed_string formula = (* ... *)

(* Pretty-print with indentation and line breaks *)
let pretty_print formula = (* ... *)
```

### 4. Implement Helper Functions for Fun Type

```ocaml
(* Check predicates on function names *)
let is_oracle name = String.starts_with ~prefix:"O" name
let is_const_input name = String.starts_with ~prefix:"I" name
let is_aux name = String.starts_with ~prefix:"choice" name
let is_b name = String.starts_with ~prefix:"B" name

(* Encode Fun to Var *)
let encode_fun_to_var (Fun (name, ind, params)) =
  let ind_str = match ind with
    | None -> ""
    | Some i -> "#" ^ i
  in
  let param_str = 
    params 
    |> List.map (fun v -> "_" ^ v.name ^ "#" ^ v.typ)
    |> String.concat ""
  in
  mk_var (name ^ ind_str ^ param_str)

(* Encode function name with index *)
let encode_fun_name (Fun (name, ind, _)) =
  match ind with
  | None -> name
  | Some i -> name ^ "#" ^ i
```

### 5. Implement Pattern Matching and Decoding

```ocaml
(* Decode Var to Fun based on naming pattern *)
let decode_var_to_fun var_string =
  (* Pattern: name(#index)?(_param1#type1_param2#type2...)? *)
  (* Use regex or string parsing *)
  (* Return Fun option *)
  (* ... *)

(* Decode just the function name and index *)
let decode_fun_name var_string =
  (* Pattern: name(#index)? *)
  (* Return (string * string option) option *)
  (* ... *)
```

### 6. Implement Basic Formula Analysis Functions

```ocaml
(* Compute free variables *)
let rec free_vars formula = (* ... returns var list *)

(* Compute bound variables *)
let rec bound_vars formula = (* ... returns var list *)

(* Compute operator size (number of operators) *)
let rec opsize formula = (* ... returns int *)

(* Check if formula is quantifier-free *)
let rec is_qfree formula = (* ... returns bool *)

(* Check if formula is universal (only ∀ quantifiers) *)
let rec is_universal formula = (* ... returns bool *)

(* Check if formula is in Bernays-Schönfinkel class *)
let is_bs formula = (* ... returns bool *)
```

### 7. Implement Formula Transformation Functions

```ocaml
(* Apply transformation everywhere in the formula tree *)
let rec everywhere f formula = (* ... *)

(* Collect results from partial function application *)
let rec collect f formula = (* ... *)

(* Check if any subformula satisfies a predicate *)
let rec has_subformula pred formula = (* ... *)

(* Parallel variable renaming *)
let rec parallel_rename formula old_vars new_vars = (* ... *)

(* Remove ForallOtherThan quantifiers *)
let rec remove_ot_quantifiers formula = (* ... *)
```

### 8. Create Test File `test/test_foltl.ml`

Implement expect tests for:

```ocaml
(* Test basic construction *)
let%expect_test "create_basic_formulas" = (* ... *)

(* Test smart constructors *)
let%expect_test "and_list_construction" = (* ... *)
let%expect_test "or_list_construction" = (* ... *)

(* Test string conversion *)
let%expect_test "formula_to_string" = (* ... *)
let%expect_test "formula_to_typed_string" = (* ... *)

(* Test variable operations *)
let%expect_test "free_vars" = (* ... *)
let%expect_test "bound_vars" = (* ... *)

(* Test formula properties *)
let%expect_test "is_qfree" = (* ... *)
let%expect_test "opsize" = (* ... *)

(* Test Fun encoding/decoding *)
let%expect_test "encode_fun_to_var" = (* ... *)
let%expect_test "decode_var_to_fun" = (* ... *)

(* Test transformations *)
let%expect_test "parallel_rename" = (* ... *)
let%expect_test "everywhere_transformation" = (* ... *)
```

### 9. Update Interface File `src/foltl.mli`

Export all public types and functions:

```ocaml
type var = {
  name : string;
  typ : string;
}

type formula = (* full type definition *)

(* Constructors *)
val mk_var : ?typ:string -> string -> var
val mk_fun : string -> var list -> formula
val mk_fun_indexed : string -> string -> var list -> formula
val mk_and_list : formula list -> formula
val mk_or_list : formula list -> formula
(* ... all other constructors ... *)

(* String conversion *)
val var_to_string : var -> string
val var_to_string_typed : var -> string
val to_string : formula -> string
val to_typed_string : formula -> string
val pretty_print : formula -> string

(* Analysis functions *)
val free_vars : formula -> var list
val bound_vars : formula -> var list
val opsize : formula -> int
val is_qfree : formula -> bool
val is_universal : formula -> bool
val is_bs : formula -> bool

(* Transformation functions *)
val everywhere : (formula -> formula) -> formula -> formula
val collect : (formula -> 'a list) -> formula -> 'a list
val has_subformula : (formula -> bool) -> formula -> bool
val parallel_rename : formula -> var list -> var list -> formula
val remove_ot_quantifiers : formula -> formula

(* Fun-specific utilities *)
val is_oracle : string -> bool
val is_const_input : string -> bool
val is_aux : string -> bool
val is_b : string -> bool
val encode_fun_to_var : formula -> var
val encode_fun_name : formula -> string
val decode_var_to_fun : string -> formula option
val decode_fun_name : string -> (string * string option) option
```

### 10. Modify `src/niwo.ml` and `src/niwo.mli`

Update the file processing logic to read formula files and parse them:

#### `src/niwo.ml`

```ocaml
(* Re-export modules for convenience *)
module Foltl = Foltl
module Formula_parser = Formula_parser

(* Read entire file contents into a string *)
let read_file filename =
  let ic = open_in filename in
  let rec aux acc =
    try
      let line = input_line ic in
      aux (line :: acc)
    with End_of_file -> 
      close_in ic;
      List.rev acc
  in
  aux []

(* Count lines in a file *)
let count_lines filename =
  let ic = open_in filename in
  let rec aux acc =
    try
      let _ = input_line ic in
      aux (acc + 1)
    with End_of_file -> acc
  in
  let result = aux 0 in
  close_in ic;
  result

(* Process a formula file: read, parse, and display formula information *)
let process_formula_file filename =
  try
    (* Read all lines from file *)
    let lines = read_file filename in
    let content = String.concat "\n" lines in
    
    (* Parse the formula *)
    match Formula_parser.parse_formula content with
    | Ok formula ->
        Printf.printf "File: %s\n" filename;
        Printf.printf "Lines: %d\n" (List.length lines);
        Printf.printf "\n--- Parsed Formula ---\n";
        Printf.printf "%s\n" (Foltl.to_string formula);
        Printf.printf "\n--- With Types ---\n";
        Printf.printf "%s\n" (Foltl.to_typed_string formula);
        Printf.printf "\n--- Pretty Print ---\n";
        Printf.printf "%s\n" (Foltl.pretty_print formula);
        Printf.printf "\n--- Analysis ---\n";
        Printf.printf "Free variables: %s\n"
          (String.concat ", " (List.map Foltl.var_to_string_typed (Foltl.free_vars formula)));
        Printf.printf "Bound variables: %s\n"
          (String.concat ", " (List.map Foltl.var_to_string_typed (Foltl.bound_vars formula)));
        Printf.printf "Operator size: %d\n" (Foltl.opsize formula);
        Printf.printf "Quantifier-free: %b\n" (Foltl.is_qfree formula);
        Printf.printf "Universal: %b\n" (Foltl.is_universal formula);
        Printf.printf "Bernays-Schönfinkel: %b\n" (Foltl.is_bs formula)
    | Error msg ->
        Printf.eprintf "Parse error in file '%s': %s\n" filename msg;
        exit 1
  with
  | Sys_error msg ->
      Printf.eprintf "Error reading file: %s\n" msg;
      exit 1

(* Legacy function for backward compatibility *)
let process_file filename =
  try
    let line_count = count_lines filename in
    Printf.printf "The file '%s' has %d lines.\n" filename line_count
  with Sys_error msg ->
    Printf.eprintf "Error: %s\n" msg;
    exit 1
```

#### `src/niwo.mli`

```ocaml
(* Re-export formula modules *)
module Foltl = Foltl
module Formula_parser = Formula_parser

(* Read all lines from a file *)
val read_file : string -> string list

(* Count lines in a file *)
val count_lines : string -> int

(* Process a formula file: read, parse, and display analysis *)
val process_formula_file : string -> unit

(* Legacy function: just print line count *)
val process_file : string -> unit
```

### 11. Integration Steps

1. Update `src/dune` to include all modules:
   ```
   (library
     (name niwo)
     (modules niwo foltl formula_parser)
     (libraries re angstrom))
   ```

2. The `src/niwo.mli` interface (shown above) re-exports `Foltl` and `Formula_parser` modules

3. Create example usage in `bin/main.ml` (see section 13)

## Implementation Order

1. **Phase 1**: Core type definition and basic constructors
   - Define `var` and `formula` types
   - Implement `mk_var`, `mk_fun`, basic binary operators
   - Implement `to_string` function
   - Write basic tests

2. **Phase 2**: Smart constructors for lists
   - Implement `mk_and_list`, `mk_or_list`, etc.
   - Handle edge cases (empty lists, True/False)
   - Write tests for list constructors

3. **Phase 3**: String conversion and encoding
   - Implement `to_typed_string`, `pretty_print`
   - Implement Fun encoding/decoding functions
   - Write comprehensive string conversion tests

4. **Phase 4**: Analysis functions
   - Implement `free_vars`, `bound_vars`, `opsize`
   - Implement predicate functions (`is_qfree`, `is_universal`, `is_bs`)
   - Write tests for all analysis functions

5. **Phase 5**: Transformation functions
   - Implement `everywhere`, `collect`, `has_subformula`
   - Implement `parallel_rename`, `remove_ot_quantifiers`
   - Write tests for transformations

6. **Phase 6**: Documentation and refinement
   - Add comprehensive documentation comments
   - Refine error handling
   - Add more edge case tests
   - Update README with FOLTL module usage

## Notes for Implementation

- Use OCaml's pattern matching extensively for clean code
- Consider using `Seq` or `List` module functions for efficient list operations
- For regex operations in decoding, use the `Re` library (add to dependencies)
- Use `Format` module for pretty-printing with proper indentation
- Consider adding a `pp` function for use with `%a` in printf-style formatting
- Keep functions pure and avoid mutable state
- Use tail-recursive functions for list operations when possible
- Consider using `@@` and `|>` operators for cleaner function composition

## Additional Dependencies

Update `niwo-ocaml.opam` to include:
```
depends: [
  "ocaml" {>= "4.14.0"}
  "dune" {>= "3.7"}
  "ppx_expect" {with-test}
  "re" {>= "1.10.0"}      (* for regex in decoding *)
  "angstrom" {>= "0.15.0"} (* for parser combinators *)
]
```

## Parser Implementation

### 11. Create `src/formula_parser.ml` and `src/formula_parser.mli`

The parser will use the Angstrom library for parser combinators, which provides a similar abstraction to Scala's parser combinator library.

#### Parser Structure

```ocaml
open Angstrom
open Foltl

(* Whitespace handling *)
let whitespace = skip_while (function ' ' | '\t' | '\n' | '\r' -> true | _ -> false)
let token p = p <* whitespace
let parens p = token (char '(') *> p <* token (char ')')
let symbol s = token (string s)

(* Identifiers *)
let is_alpha = function 'a'..'z' | 'A'..'Z' -> true | _ -> false
let is_alphanum = function 'a'..'z' | 'A'..'Z' | '0'..'9' -> true | _ -> false

let ident : string t =
  lift2 (fun c s -> String.make 1 c ^ s)
    (satisfy is_alpha)
    (take_while is_alphanum)
  >>= fun s -> whitespace *> return s

(* Variables *)
let var : var t =
  ident >>= fun name -> return (mk_var name)

let typed_var : var t =
  ident >>= fun name ->
  option None (char ':' *> ident >>| fun t -> Some t) >>= fun typ ->
  match typ with
  | None -> return (mk_var name)
  | Some t -> return (mk_var ~typ:t name)

(* Parameters tuple *)
let tuple : var list t =
  parens (sep_by (token (char ',')) typed_var)

(* Function application *)
let fun_term : formula t =
  var >>= fun v ->
  option None (char '#' *> var >>| fun i -> Some i.name) >>= fun index ->
  tuple >>= fun params ->
  match index with
  | None -> return (mk_fun v.name params)
  | Some idx -> return (mk_fun_indexed v.name idx params)

(* Forward declaration for recursive parser *)
let term : formula t ref = ref (fail "term not initialized")

(* Boolean constants *)
let true_lit = symbol "True" *> return True
let false_lit = symbol "False" *> return False

(* Equality *)
let equal : formula t =
  var >>= fun v1 ->
  token (char '=') *>
  var >>= fun v2 ->
  return (Equal (Var v1, Var v2))

let not_equal : formula t =
  var >>= fun v1 ->
  (token (string "≠") <|> token (string "!=")) *>
  var >>= fun v2 ->
  return (Neg (Equal (Var v1, Var v2)))

let eq_term = equal <|> not_equal

(* Simple terms (non-recursive) *)
let simple_term : formula t =
  true_lit <|> false_lit <|> attempt fun_term <|> attempt eq_term <|> (var >>| fun v -> Var v)

(* Logical operators *)
let neg : formula t =
  (token (string "¬") <|> token (string "!")) *>
  !term

let and_term : formula t =
  parens (
    sep_by1 (token (string "∧") <|> token (string "&&")) !term
  ) >>| mk_and_list

let or_term : formula t =
  parens (
    sep_by1 (token (string "∨") <|> token (string "||")) !term
  ) >>| mk_or_list

let implies : formula t =
  parens (
    !term >>= fun t1 ->
    (token (string "⟹") <|> token (string "=>") <|> token (string "->")) *>
    !term >>= fun t2 ->
    return (Implies (t1, t2))
  )

let equiv : formula t =
  parens (
    !term >>= fun t1 ->
    token (string "<->") *>
    !term >>= fun t2 ->
    return (Equiv (t1, t2))
  )

(* Temporal operators *)
let next : formula t =
  (token (string "X") <|> token (string "◯")) *>
  !term >>| fun t -> Next t

let globally : formula t =
  (token (string "G") <|> token (string "☐")) *>
  !term >>| fun t -> Globally t

let finally : formula t =
  (token (string "F") <|> token (string "♢")) *>
  !term >>| fun t -> Finally t

let wuntil : formula t =
  parens (
    !term >>= fun t1 ->
    token (string "W") *>
    !term >>= fun t2 ->
    return (WUntil (t1, t2))
  )

let until : formula t =
  parens (
    !term >>= fun t1 ->
    token (string "U") *>
    !term >>= fun t2 ->
    return (Until (t1, t2))
  )

(* Quantifiers *)
let exists : formula t =
  token (string "∃") *>
  sep_by1 (token (char ',')) typed_var >>= fun vars ->
  option () (token (char '.')) *>
  !term >>= fun t ->
  return (mk_exists vars t)

let forall : formula t =
  token (string "∀") *>
  sep_by1 (token (char ',')) typed_var >>= fun vars ->
  option () (token (char '.')) *>
  !term >>= fun t ->
  return (mk_forall vars t)

let quantifier = exists <|> forall

(* Complete term parser *)
let () =
  term := fix (fun _ ->
    whitespace *>
    choice [
      and_term;
      or_term;
      implies;
      equiv;
      attempt neg;
      next;
      wuntil;
      until;
      quantifier;
      simple_term;
    ]
  )

(* Main parsing function *)
let parse_formula (s : string) : (formula, string) result =
  match parse_string ~consume:All !term s with
  | Ok formula -> Ok formula
  | Error msg -> Error msg

(* Convenience function that raises exception on error *)
let parse_formula_exn (s : string) : formula =
  match parse_formula s with
  | Ok f -> f
  | Error msg -> failwith ("Parse error: " ^ msg)
```

#### Parser Interface (`src/formula_parser.mli`)

```ocaml
(* Parse a formula from a string *)
val parse_formula : string -> (Foltl.formula, string) result

(* Parse a formula from a string, raising an exception on error *)
val parse_formula_exn : string -> Foltl.formula

(* Individual parser components (exposed for testing and composition) *)
val ident : string Angstrom.t
val var : Foltl.var Angstrom.t
val typed_var : Foltl.var Angstrom.t
val simple_term : Foltl.formula Angstrom.t
```

### 12. Parser Tests (`test/test_parser.ml`)

Implement comprehensive expect tests:

```ocaml
open Formula_parser

(* Test basic parsing *)
let%expect_test "parse_true" =
  match parse_formula "True" with
  | Ok f -> print_endline (Foltl.to_string f)
  | Error e -> Printf.printf "Error: %s\n" e;
  [%expect {| True |}]

let%expect_test "parse_false" =
  match parse_formula "False" with
  | Ok f -> print_endline (Foltl.to_string f)
  | Error e -> Printf.printf "Error: %s\n" e;
  [%expect {| False |}]

(* Test variables *)
let%expect_test "parse_var" =
  match parse_formula "x" with
  | Ok f -> print_endline (Foltl.to_string f)
  | Error e -> Printf.printf "Error: %s\n" e;
  [%expect {| x |}]

let%expect_test "parse_typed_var" =
  let s = "x:Int" in
  (* Note: typed vars only appear in quantifiers/functions in full formulas *)
  Printf.printf "Input: %s\n" s;
  [%expect {| Input: x:Int |}]

(* Test functions *)
let%expect_test "parse_function" =
  match parse_formula "f(x:T,y:T)" with
  | Ok f -> print_endline (Foltl.to_string f)
  | Error e -> Printf.printf "Error: %s\n" e;
  [%expect {| f(x,y) |}]

let%expect_test "parse_function_with_index" =
  match parse_formula "P#1(x:T)" with
  | Ok f -> print_endline (Foltl.to_string f)
  | Error e -> Printf.printf "Error: %s\n" e;
  [%expect {| P(1)(x) |}]

(* Test equality *)
let%expect_test "parse_equal" =
  match parse_formula "x = y" with
  | Ok f -> print_endline (Foltl.to_string f)
  | Error e -> Printf.printf "Error: %s\n" e;
  [%expect {| (x = y) |}]

let%expect_test "parse_not_equal" =
  match parse_formula "x ≠ y" with
  | Ok f -> print_endline (Foltl.to_string f)
  | Error e -> Printf.printf "Error: %s\n" e;
  [%expect {| ¬(x = y) |}]

(* Test logical operators *)
let%expect_test "parse_neg" =
  match parse_formula "¬ True" with
  | Ok f -> print_endline (Foltl.to_string f)
  | Error e -> Printf.printf "Error: %s\n" e;
  [%expect {| ¬ True |}]

let%expect_test "parse_and" =
  match parse_formula "(True ∧ False)" with
  | Ok f -> print_endline (Foltl.to_string f)
  | Error e -> Printf.printf "Error: %s\n" e;
  [%expect {| (True ∧ False) |}]

let%expect_test "parse_and_multiple" =
  match parse_formula "(x ∧ y ∧ z)" with
  | Ok f -> print_endline (Foltl.to_string f)
  | Error e -> Printf.printf "Error: %s\n" e;
  [%expect {| (x ∧ (y ∧ z)) |}]

let%expect_test "parse_or" =
  match parse_formula "(True ∨ False)" with
  | Ok f -> print_endline (Foltl.to_string f)
  | Error e -> Printf.printf "Error: %s\n" e;
  [%expect {| (True ∨ False) |}]

let%expect_test "parse_implies" =
  match parse_formula "(True ⟹ False)" with
  | Ok f -> print_endline (Foltl.to_string f)
  | Error e -> Printf.printf "Error: %s\n" e;
  [%expect {| (True → False) |}]

let%expect_test "parse_equiv" =
  match parse_formula "(True <-> False)" with
  | Ok f -> print_endline (Foltl.to_string f)
  | Error e -> Printf.printf "Error: %s\n" e;
  [%expect {| (True ↔ False) |}]

(* Test temporal operators *)
let%expect_test "parse_next" =
  match parse_formula "X True" with
  | Ok f -> print_endline (Foltl.to_string f)
  | Error e -> Printf.printf "Error: %s\n" e;
  [%expect {| X True |}]

let%expect_test "parse_wuntil" =
  match parse_formula "(p W q)" with
  | Ok f -> print_endline (Foltl.to_string f)
  | Error e -> Printf.printf "Error: %s\n" e;
  [%expect {| (p W q) |}]

(* Test quantifiers *)
let%expect_test "parse_exists" =
  match parse_formula "∃ x:T. P(x:T)" with
  | Ok f -> print_endline (Foltl.to_typed_string f)
  | Error e -> Printf.printf "Error: %s\n" e;
  [%expect {| ∃ x:T. P(x) |}]

let%expect_test "parse_forall" =
  match parse_formula "∀ x:T,y:T. (P(x:T) ⟹ Q(y:T))" with
  | Ok f -> print_endline (Foltl.to_typed_string f)
  | Error e -> Printf.printf "Error: %s\n" e;
  [%expect {| ∀ x:T,y:T. (P(x) → Q(y)) |}]

(* Test complex formulas *)
let%expect_test "parse_complex" =
  match parse_formula "∀ x:T. (P(x:T) ⟹ X Q(x:T))" with
  | Ok f -> print_endline (Foltl.to_typed_string f)
  | Error e -> Printf.printf "Error: %s\n" e;
  [%expect {| ∀ x:T. (P(x) → X Q(x)) |}]

let%expect_test "parse_nested" =
  match parse_formula "∀ x:T. ∃ y:T. (P(x:T,y:T) ∧ Q(y:T))" with
  | Ok f -> print_endline (Foltl.to_typed_string f)
  | Error e -> Printf.printf "Error: %s\n" e;
  [%expect {| ∀ x:T. ∃ y:T. (P(x,y) ∧ Q(y)) |}]

(* Test error cases *)
let%expect_test "parse_error_unclosed_paren" =
  match parse_formula "(True ∧ False" with
  | Ok f -> print_endline (Foltl.to_string f)
  | Error e -> Printf.printf "Error: %s\n" e;
  [%expect {| Error: ... |}]

let%expect_test "parse_error_invalid_operator" =
  match parse_formula "True & False" with
  | Ok f -> print_endline (Foltl.to_string f)
  | Error e -> Printf.printf "Error: %s\n" e;
  [%expect {| Error: ... |}]
```

### 13. Update `bin/main.ml`

Update the main executable to support both file parsing and direct formula strings:

```ocaml
let usage () =
  Printf.eprintf "Usage:\n";
  Printf.eprintf "  %s <formula_file>           Parse formula from file\n" Sys.argv.(0);
  Printf.eprintf "  %s --string <formula>       Parse formula from command line\n" Sys.argv.(0);
  Printf.eprintf "  %s --count <file>           Count lines in file (legacy)\n" Sys.argv.(0);
  Printf.eprintf "\nExamples:\n";
  Printf.eprintf "  %s test.txt\n" Sys.argv.(0);
  Printf.eprintf "  %s --string \"∀ x:T. P(x:T)\"\n" Sys.argv.(0);
  Printf.eprintf "  %s --string '(p ∧ q)'\n" Sys.argv.(0);
  exit 1

let () =
  if Array.length Sys.argv < 2 then usage ();
  
  match Sys.argv.(1) with
  | "--string" ->
      if Array.length Sys.argv < 3 then begin
        Printf.eprintf "Error: --string requires a formula argument\n";
        usage ()
      end;
      let formula_str = Sys.argv.(2) in
      (match Niwo.Formula_parser.parse_formula formula_str with
       | Ok formula ->
           Printf.printf "--- Parsed Formula ---\n";
           Printf.printf "%s\n" (Niwo.Foltl.to_string formula);
           Printf.printf "\n--- With Types ---\n";
           Printf.printf "%s\n" (Niwo.Foltl.to_typed_string formula);
           Printf.printf "\n--- Pretty Print ---\n";
           Printf.printf "%s\n" (Niwo.Foltl.pretty_print formula);
           Printf.printf "\n--- Analysis ---\n";
           Printf.printf "Free vars: %s\n"
             (String.concat ", " (List.map Niwo.Foltl.var_to_string_typed (Niwo.Foltl.free_vars formula)));
           Printf.printf "Bound vars: %s\n"
             (String.concat ", " (List.map Niwo.Foltl.var_to_string_typed (Niwo.Foltl.bound_vars formula)));
           Printf.printf "Op size: %d\n" (Niwo.Foltl.opsize formula);
           Printf.printf "Quantifier-free: %b\n" (Niwo.Foltl.is_qfree formula);
           Printf.printf "Universal: %b\n" (Niwo.Foltl.is_universal formula);
           Printf.printf "Bernays-Schönfinkel: %b\n" (Niwo.Foltl.is_bs formula)
       | Error msg ->
           Printf.eprintf "Parse error: %s\n" msg;
           exit 1)
  
  | "--count" ->
      if Array.length Sys.argv < 3 then begin
        Printf.eprintf "Error: --count requires a filename\n";
        usage ()
      end;
      let filename = Sys.argv.(2) in
      Niwo.process_file filename
  
  | filename ->
      (* Default: treat argument as a formula file *)
      Niwo.process_formula_file filename
```

### 14. Update `.gitignore`

Update the `.gitignore` file to exclude build artifacts:

```
_build/
*.install
*.merlin
.ocamlformat
*.swp
*~
```

Note: `test.txt` is kept in the repository as a manually editable test file for formulas.

## Updated Implementation Order

1. **Phase 1**: Core type definition and basic constructors ✅ COMPLETED
   - Define `var` and `formula` types in `foltl.ml` and `foltl.mli`
   - Implement `mk_var`, `mk_fun`, basic binary operators
   - Implement `to_string` function
   - Write basic tests in `test/test_foltl.ml`

2. **Phase 2**: Smart constructors for lists ✅ COMPLETED
   - Implement `mk_and_list`, `mk_or_list`, etc.
   - Handle edge cases (empty lists, True/False)
   - Write tests for list constructors

3. **Phase 3**: String conversion and encoding ✅ COMPLETED
   - Implement `to_typed_string`, `pretty_print`
   - Implement Fun encoding/decoding functions
   - Write comprehensive string conversion tests

4. **Phase 4**: Analysis functions 🔄 IN PROGRESS
   - Implement `free_vars`, `bound_vars`, `opsize`
   - Implement predicate functions (`is_qfree`, `is_universal`, `is_bs`)
   - Write tests for all analysis functions

2. **Phase 2**: Smart constructors for lists
   - Implement `mk_and_list`, `mk_or_list`, etc.
   - Handle edge cases (empty lists, True/False)
   - Write tests for list constructors

3. **Phase 3**: String conversion and encoding
   - Implement `to_typed_string`, `pretty_print`
   - Implement Fun encoding/decoding functions
   - Write comprehensive string conversion tests

4. **Phase 4**: Analysis functions
   - Implement `free_vars`, `bound_vars`, `opsize`
   - Implement predicate functions (`is_qfree`, `is_universal`, `is_bs`)
   - Write tests for all analysis functions

5. **Phase 5**: Transformation functions
   - Implement `everywhere`, `collect`, `has_subformula`
   - Implement `parallel_rename`, `remove_ot_quantifiers`
   - Write tests for transformations

6. **Phase 6**: Parser implementation
   - Set up Angstrom parser combinators in `formula_parser.ml`
   - Implement basic parsers (ident, var, typed_var)
   - Implement term parsers (simple_term, operators)
   - Implement recursive term parser
   - Write comprehensive parser tests in `test/test_parser.ml`
   - Test round-trip: parse -> to_string -> parse

7. **Phase 7**: File processing integration
   - Update `niwo.ml` to add `read_file` and `process_formula_file` functions
   - Update `niwo.mli` to expose new functions and re-export modules
   - Update `bin/main.ml` to support file parsing and command-line formulas
   - Update `.gitignore` as needed
   - Write tests for file processing in `test/test_niwo.ml`
   - Test with `test.txt` (manually edit as needed for different test cases)

8. **Phase 8**: Documentation and refinement
   - Add comprehensive documentation comments to all modules
   - Refine error handling and error messages
   - Add more edge case tests
   - Update README with complete usage guide:
     - FOLTL module usage
     - Parser usage
     - File format specification
     - Command-line interface examples
   - Add examples of common formulas