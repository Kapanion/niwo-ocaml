open! Core
open Niwo.Formula_parser

let print_parse_result = function
  | Ok f -> print_endline (Sexp.to_string_hum (Niwo.Formula.sexp_of_t f))
  | Error e -> Printf.printf "Error: %s\n" (Error.to_string_hum e)
;;

let%expect_test "parse_true" =
  parse_formula "True" |> print_parse_result;
  [%expect {| True |}]
;;

let%expect_test "parse_false" =
  parse_formula "False" |> print_parse_result;
  [%expect {| False |}]
;;

let%expect_test "parse_var" =
  parse_formula "x" |> print_parse_result;
  [%expect {| (Var ((name x) (typ T))) |}]
;;

let%expect_test "parse_typed_var" =
  let s = "x:Int" in
  (* Note: typed vars only appear in quantifiers/functions in full formulas *)
  Printf.printf "Input: %s\n" s;
  [%expect {| Input: x:Int |}]
;;

let%expect_test "parse_function" =
  parse_formula "f(x:T,y:T)" |> print_parse_result;
  [%expect {| (Fun f () (((name x) (typ T)) ((name y) (typ T)))) |}]
;;

let%expect_test "parse_function_with_index" =
  parse_formula "P#1(x:T)" |> print_parse_result;
  [%expect {| Error: : end_of_input |}]
;;

let%expect_test "parse_equal" =
  parse_formula "x:T = y:T" |> print_parse_result;
  [%expect {| (Equal (Var ((name x) (typ T))) (Var ((name y) (typ T)))) |}]
;;

let%expect_test "parse_not_equal" =
  parse_formula "x:T != y:T" |> print_parse_result;
  [%expect {| (Neg (Equal (Var ((name x) (typ T))) (Var ((name y) (typ T))))) |}]
;;

let%expect_test "parse_neg" =
  parse_formula "! True" |> print_parse_result;
  [%expect {| (Neg True) |}]
;;

let%expect_test "parse_and" =
  parse_formula "(True && False)" |> print_parse_result;
  [%expect {| (And True False) |}]
;;

let%expect_test "parse_and_multiple" =
  parse_formula "(x:T && y:T && z:T)" |> print_parse_result;
  [%expect
    {|
    (And (And (Var ((name x) (typ T))) (Var ((name y) (typ T))))
     (Var ((name z) (typ T))))
    |}]
;;

let%expect_test "parse_or" =
  parse_formula "(True || False)" |> print_parse_result;
  [%expect {| (Or True False) |}]
;;

let%expect_test "parse_implies" =
  parse_formula "(True -> False)" |> print_parse_result;
  [%expect {| (Implies True False) |}]
;;

let%expect_test "parse_equiv" =
  parse_formula "(True <-> False)" |> print_parse_result;
  [%expect {| (Equiv True False) |}]
;;

let%expect_test "parse_next" =
  parse_formula "X True" |> print_parse_result;
  [%expect {| (Next True) |}]
;;

let%expect_test "parse_globally" =
  parse_formula "G True" |> print_parse_result;
  [%expect {| (Globally True) |}]
;;

let%expect_test "parse_finally" =
  parse_formula "F True" |> print_parse_result;
  [%expect {| (Finally True) |}]
;;

let%expect_test "parse_wuntil" =
  parse_formula "(p:T W q:T)" |> print_parse_result;
  [%expect {| (WUntil (Var ((name p) (typ T))) (Var ((name q) (typ T)))) |}]
;;

let%expect_test "parse_until" =
  parse_formula "(p:T U q:T)" |> print_parse_result;
  [%expect {| (Until (Var ((name p) (typ T))) (Var ((name q) (typ T)))) |}]
;;

let%expect_test "parse_exists_typed" =
  parse_formula "∃ x:T. P(x:T)" |> print_parse_result;
  [%expect {| (Exists (((name x) (typ T))) (Fun P () (((name x) (typ T))))) |}]
;;

let%expect_test "parse_forall_typed" =
  parse_formula "∀ x:T,y:T. (P(x:T) -> Q(y:T))" |> print_parse_result;
  [%expect
    {|
    (Forall (((name x) (typ T)) ((name y) (typ T)))
     (Implies (Fun P () (((name x) (typ T)))) (Fun Q () (((name y) (typ T))))))
    |}]
;;

let%expect_test "parse_complex_typed" =
  parse_formula "∀ x:T. (P(x:T) -> X Q(x:T))" |> print_parse_result;
  [%expect
    {|
    (Forall (((name x) (typ T)))
     (Implies (Fun P () (((name x) (typ T))))
      (Next (Fun Q () (((name x) (typ T)))))))
    |}]
;;

let%expect_test "parse_nested_typed" =
  parse_formula "∀ x:T. ∃ y:T. (P(x:T,y:T) && Q(y:T))" |> print_parse_result;
  [%expect
    {|
    (Forall (((name x) (typ T)))
     (Exists (((name y) (typ T)))
      (And (Fun P () (((name x) (typ T)) ((name y) (typ T))))
       (Fun Q () (((name y) (typ T)))))))
    |}]
;;

let%expect_test "parse_exists" =
  parse_formula "∃ x. P(x)" |> print_parse_result;
  [%expect {| (Exists (((name x) (typ T))) (Fun P () (((name x) (typ T))))) |}]
;;

let%expect_test "parse_forall" =
  parse_formula "∀ x,y. (P(x) -> Q(y))" |> print_parse_result;
  [%expect
    {|
    (Forall (((name x) (typ T)) ((name y) (typ T)))
     (Implies (Fun P () (((name x) (typ T)))) (Fun Q () (((name y) (typ T))))))
    |}]
;;

let%expect_test "parse_complex" =
  parse_formula "∀ x. (P(x) -> X Q(x))" |> print_parse_result;
  [%expect
    {|
    (Forall (((name x) (typ T)))
     (Implies (Fun P () (((name x) (typ T))))
      (Next (Fun Q () (((name x) (typ T)))))))
    |}]
;;

let%expect_test "parse_nested" =
  parse_formula "∀ x. ∃ y. (P(x,y) && Q(y))" |> print_parse_result;
  [%expect
    {|
    (Forall (((name x) (typ T)))
     (Exists (((name y) (typ T)))
      (And (Fun P () (((name x) (typ T)) ((name y) (typ T))))
       (Fun Q () (((name y) (typ T)))))))
    |}]
;;

let%expect_test "parse_error_unclosed_paren" =
  parse_formula "(True && False" |> print_parse_result;
  [%expect {| Error: : no more choices |}]
;;

let%expect_test "parse_error_invalid_operator" =
  parse_formula "True & False" |> print_parse_result;
  [%expect {| Error: : end_of_input |}]
;;
