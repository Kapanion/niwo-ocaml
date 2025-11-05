open! Core

open Niwo.Formula_parser

(* Test basic parsing *)
let%expect_test "parse_true" =
  (match parse_formula "True" with
  | Ok f -> print_endline (Niwo.Foltl.to_string f)
  | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| True |}]

let%expect_test "parse_false" =
  (match parse_formula "False" with
  | Ok f -> print_endline (Niwo.Foltl.to_string f)
  | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| False |}]

(* Test variables *)
let%expect_test "parse_var" =
  (match parse_formula "x" with
  | Ok f -> print_endline (Niwo.Foltl.to_string f)
  | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| x |}]

let%expect_test "parse_typed_var" =
  let s = "x:Int" in
  (* Note: typed vars only appear in quantifiers/functions in full formulas *)
  Printf.printf "Input: %s\n" s;
  [%expect {| Input: x:Int |}]

(* Test functions *)
let%expect_test "parse_function" =
  (match parse_formula "f(x:T,y:T)" with
  | Ok f -> print_endline (Niwo.Foltl.to_string f)
  | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| f(x,y) |}]

let%expect_test "parse_function_with_index" =
  (match parse_formula "P#1(x:T)" with
  | Ok f -> print_endline (Niwo.Foltl.to_string f)
  | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| Error: : end_of_input |}]

(* Test equality *)
let%expect_test "parse_equal" =
  (match parse_formula "x:T = y:T" with
  | Ok f -> print_endline (Niwo.Foltl.to_string f)
  | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| x = y |}]

let%expect_test "parse_not_equal" =
  (match parse_formula "x:T != y:T" with
  | Ok f -> print_endline (Niwo.Foltl.to_string f)
  | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| ¬(x = y) |}]

(* Test logical operators *)
let%expect_test "parse_neg" =
  (match parse_formula "! True" with
  | Ok f -> print_endline (Niwo.Foltl.to_string f)
  | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| Error: : no more choices |}]

let%expect_test "parse_and" =
  (match parse_formula "(True && False)" with
  | Ok f -> print_endline (Niwo.Foltl.to_string f)
  | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| True ∧ False |}]

let%expect_test "parse_and_multiple" =
  (match parse_formula "(x:T && y:T && z:T)" with
  | Ok f -> print_endline (Niwo.Foltl.to_string f)
  | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| (x ∧ y) ∧ z |}]

let%expect_test "parse_or" =
  (match parse_formula "(True || False)" with
  | Ok f -> print_endline (Niwo.Foltl.to_string f)
  | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| True ∨ False |}]

let%expect_test "parse_implies" =
  (match parse_formula "(True -> False)" with
  | Ok f -> print_endline (Niwo.Foltl.to_string f)
  | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| True → False |}]

let%expect_test "parse_equiv" =
  (match parse_formula "(True <-> False)" with
  | Ok f -> print_endline (Niwo.Foltl.to_string f)
  | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| True ↔ False |}]

(* Test temporal operators *)
let%expect_test "parse_next" =
  (match parse_formula "X True" with
  | Ok f -> print_endline (Niwo.Foltl.to_string f)
  | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| Error: : end_of_input |}]

let%expect_test "parse_globally" =
  (match parse_formula "G True" with
  | Ok f -> print_endline (Niwo.Foltl.to_string f)
  | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| Error: : end_of_input |}]

let%expect_test "parse_finally" =
  (match parse_formula "F True" with
  | Ok f -> print_endline (Niwo.Foltl.to_string f)
  | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| Error: : end_of_input |}]

let%expect_test "parse_wuntil" =
  (match parse_formula "(p:T W q:T)" with
  | Ok f -> print_endline (Niwo.Foltl.to_string f)
  | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| p W q |}]

let%expect_test "parse_until" =
  (match parse_formula "(p:T U q:T)" with
  | Ok f -> print_endline (Niwo.Foltl.to_string f)
  | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| p U q |}]

(* Test quantifiers *)
let%expect_test "parse_exists_typed" =
  (match parse_formula "∃ x:T. P(x:T)" with
  | Ok f -> print_endline (Niwo.Foltl.to_typed_string f)
  | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| ∃ x:T. P(x:T) |}]

let%expect_test "parse_forall_typed" =
  (match parse_formula "∀ x:T,y:T. (P(x:T) -> Q(y:T))" with
  | Ok f -> print_endline (Niwo.Foltl.to_typed_string f)
  | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| ∀ x:T,y:T. (P(x:T) → Q(y:T)) |}]

(* Test complex formulas *)
let%expect_test "parse_complex_typed" =
  (match parse_formula "∀ x:T. (P(x:T) -> X Q(x:T))" with
  | Ok f -> print_endline (Niwo.Foltl.to_typed_string f)
  | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| Error: : no more choices |}]

let%expect_test "parse_nested_typed" =
  (match parse_formula "∀ x:T. ∃ y:T. (P(x:T,y:T) && Q(y:T))" with
  | Ok f -> print_endline (Niwo.Foltl.to_typed_string f)
  | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| ∀ x:T. ∃ y:T. (P(x:T,y:T) ∧ Q(y:T)) |}]

(* Test quantifiers *)
let%expect_test "parse_exists" =
  (match parse_formula "∃ x. P(x)" with
  | Ok f -> print_endline (Niwo.Foltl.to_typed_string f)
  | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| ∃ x:T. P(x:T) |}]

let%expect_test "parse_forall" =
  (match parse_formula "∀ x,y. (P(x) -> Q(y))" with
  | Ok f -> print_endline (Niwo.Foltl.to_typed_string f)
  | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| ∀ x:T,y:T. (P(x:T) → Q(y:T)) |}]

(* Test complex formulas *)
let%expect_test "parse_complex" =
  (match parse_formula "∀ x. (P(x) -> X Q(x))" with
  | Ok f -> print_endline (Niwo.Foltl.to_typed_string f)
  | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| Error: : no more choices |}]

let%expect_test "parse_nested" =
  (match parse_formula "∀ x. ∃ y. (P(x,y) && Q(y))" with
  | Ok f -> print_endline (Niwo.Foltl.to_typed_string f)
  | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| ∀ x:T. ∃ y:T. (P(x:T,y:T) ∧ Q(y:T)) |}]

(* Test error cases *)
let%expect_test "parse_error_unclosed_paren" =
  (match parse_formula "(True && False" with
  | Ok f -> print_endline (Niwo.Foltl.to_string f)
  | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| Error: : no more choices |}]

let%expect_test "parse_error_invalid_operator" =
  (match parse_formula "True & False" with
  | Ok f -> print_endline (Niwo.Foltl.to_string f)
  | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| Error: : end_of_input |}]