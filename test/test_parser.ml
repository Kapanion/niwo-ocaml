open! Core
open Niwo.Formula_parser

let print_sexp f = print_endline (Sexp.to_string_hum (Niwo.Formula.sexp_of_t f))

(* Test basic parsing *)
let%expect_test "parse_true" =
  (match parse_formula "True" with
   | Ok f -> print_sexp f
   | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| True |}]
;;

let%expect_test "parse_false" =
  (match parse_formula "False" with
   | Ok f -> print_sexp f
   | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| False |}]
;;

(* Test variables *)
let%expect_test "parse_var" =
  (match parse_formula "x" with
   | Ok f -> print_sexp f
   | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| (Var ((name x) (typ T))) |}]
;;

let%expect_test "parse_typed_var" =
  let s = "x:Int" in
  (* Note: typed vars only appear in quantifiers/functions in full formulas *)
  Printf.printf "Input: %s\n" s;
  [%expect {| Input: x:Int |}]
;;

(* Test functions *)
let%expect_test "parse_function" =
  (match parse_formula "f(x:T,y:T)" with
   | Ok f -> print_sexp f
   | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| (Fun f () (((name x) (typ T)) ((name y) (typ T)))) |}]
;;

let%expect_test "parse_function_with_index" =
  (match parse_formula "P#1(x:T)" with
   | Ok f -> print_sexp f
   | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| Error: : end_of_input |}]
;;

(* Test equality *)
let%expect_test "parse_equal" =
  (match parse_formula "x:T = y:T" with
   | Ok f -> print_sexp f
   | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| (Equal (Var ((name x) (typ T))) (Var ((name y) (typ T)))) |}]
;;

let%expect_test "parse_not_equal" =
  (match parse_formula "x:T != y:T" with
   | Ok f -> print_sexp f
   | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| (Neg (Equal (Var ((name x) (typ T))) (Var ((name y) (typ T))))) |}]
;;

(* Test logical operators *)
let%expect_test "parse_neg" =
  (match parse_formula "! True" with
   | Ok f -> print_sexp f
   | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| (Neg True) |}]
;;

let%expect_test "parse_and" =
  (match parse_formula "(True && False)" with
   | Ok f -> print_sexp f
   | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| (And True False) |}]
;;

let%expect_test "parse_and_multiple" =
  (match parse_formula "(x:T && y:T && z:T)" with
   | Ok f -> print_sexp f
   | Error e -> Printf.printf "Error: %s\n" e);
  [%expect
    {|
    (And (And (Var ((name x) (typ T))) (Var ((name y) (typ T))))
     (Var ((name z) (typ T))))
    |}]
;;

let%expect_test "parse_or" =
  (match parse_formula "(True || False)" with
   | Ok f -> print_sexp f
   | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| (Or True False) |}]
;;

let%expect_test "parse_implies" =
  (match parse_formula "(True -> False)" with
   | Ok f -> print_sexp f
   | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| (Implies True False) |}]
;;

let%expect_test "parse_equiv" =
  (match parse_formula "(True <-> False)" with
   | Ok f -> print_sexp f
   | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| (Equiv True False) |}]
;;

(* Test temporal operators *)
let%expect_test "parse_next" =
  (match parse_formula "X True" with
   | Ok f -> print_sexp f
   | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| (Next True) |}]
;;

let%expect_test "parse_globally" =
  (match parse_formula "G True" with
   | Ok f -> print_sexp f
   | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| (Globally True) |}]
;;

let%expect_test "parse_finally" =
  (match parse_formula "F True" with
   | Ok f -> print_sexp f
   | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| (Finally True) |}]
;;

let%expect_test "parse_wuntil" =
  (match parse_formula "(p:T W q:T)" with
   | Ok f -> print_sexp f
   | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| (WUntil (Var ((name p) (typ T))) (Var ((name q) (typ T)))) |}]
;;

let%expect_test "parse_until" =
  (match parse_formula "(p:T U q:T)" with
   | Ok f -> print_sexp f
   | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| (Until (Var ((name p) (typ T))) (Var ((name q) (typ T)))) |}]
;;

(* Test quantifiers *)
let%expect_test "parse_exists_typed" =
  (match parse_formula "∃ x:T. P(x:T)" with
   | Ok f -> print_sexp f
   | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| (Exists (((name x) (typ T))) (Fun P () (((name x) (typ T))))) |}]
;;

let%expect_test "parse_forall_typed" =
  (match parse_formula "∀ x:T,y:T. (P(x:T) -> Q(y:T))" with
   | Ok f -> print_sexp f
   | Error e -> Printf.printf "Error: %s\n" e);
  [%expect
    {|
    (Forall (((name x) (typ T)) ((name y) (typ T)))
     (Implies (Fun P () (((name x) (typ T)))) (Fun Q () (((name y) (typ T))))))
    |}]
;;

(* Test complex formulas *)
let%expect_test "parse_complex_typed" =
  (match parse_formula "∀ x:T. (P(x:T) -> X Q(x:T))" with
   | Ok f -> print_sexp f
   | Error e -> Printf.printf "Error: %s\n" e);
  [%expect
    {|
    (Forall (((name x) (typ T)))
     (Implies (Fun P () (((name x) (typ T))))
      (Next (Fun Q () (((name x) (typ T)))))))
    |}]
;;

let%expect_test "parse_nested_typed" =
  (match parse_formula "∀ x:T. ∃ y:T. (P(x:T,y:T) && Q(y:T))" with
   | Ok f -> print_sexp f
   | Error e -> Printf.printf "Error: %s\n" e);
  [%expect
    {|
    (Forall (((name x) (typ T)))
     (Exists (((name y) (typ T)))
      (And (Fun P () (((name x) (typ T)) ((name y) (typ T))))
       (Fun Q () (((name y) (typ T)))))))
    |}]
;;

(* Test quantifiers *)
let%expect_test "parse_exists" =
  (match parse_formula "∃ x. P(x)" with
   | Ok f -> print_sexp f
   | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| (Exists (((name x) (typ T))) (Fun P () (((name x) (typ T))))) |}]
;;

let%expect_test "parse_forall" =
  (match parse_formula "∀ x,y. (P(x) -> Q(y))" with
   | Ok f -> print_sexp f
   | Error e -> Printf.printf "Error: %s\n" e);
  [%expect
    {|
    (Forall (((name x) (typ T)) ((name y) (typ T)))
     (Implies (Fun P () (((name x) (typ T)))) (Fun Q () (((name y) (typ T))))))
    |}]
;;

(* Test complex formulas *)
let%expect_test "parse_complex" =
  (match parse_formula "∀ x. (P(x) -> X Q(x))" with
   | Ok f -> print_sexp f
   | Error e -> Printf.printf "Error: %s\n" e);
  [%expect
    {|
    (Forall (((name x) (typ T)))
     (Implies (Fun P () (((name x) (typ T))))
      (Next (Fun Q () (((name x) (typ T)))))))
    |}]
;;

let%expect_test "parse_nested" =
  (match parse_formula "∀ x. ∃ y. (P(x,y) && Q(y))" with
   | Ok f -> print_sexp f
   | Error e -> Printf.printf "Error: %s\n" e);
  [%expect
    {|
    (Forall (((name x) (typ T)))
     (Exists (((name y) (typ T)))
      (And (Fun P () (((name x) (typ T)) ((name y) (typ T))))
       (Fun Q () (((name y) (typ T)))))))
    |}]
;;

(* Test error cases *)
let%expect_test "parse_error_unclosed_paren" =
  (match parse_formula "(True && False" with
   | Ok f -> print_sexp f
   | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| Error: : no more choices |}]
;;

let%expect_test "parse_error_invalid_operator" =
  (match parse_formula "True & False" with
   | Ok f -> print_sexp f
   | Error e -> Printf.printf "Error: %s\n" e);
  [%expect {| Error: : end_of_input |}]
;;
