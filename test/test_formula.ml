open! Core
open Niwo.Formula

let%expect_test "create_basic_formulas" =
  let t = True in
  print_endline (to_string t);
  [%expect {| True |}]
;;

let%expect_test "create_false" =
  let f = False in
  print_endline (to_string f);
  [%expect {| False |}]
;;

let%expect_test "create_variable" =
  let v = mk_var "x" in
  let var_formula = Var v in
  print_endline (to_string var_formula);
  [%expect {| x |}]
;;

let%expect_test "create_typed_variable" =
  let v = mk_var ~typ:"Int" "x" in
  let var_formula = Var v in
  print_endline (to_string var_formula);
  [%expect {| x |}]
;;

let%expect_test "create_function" =
  let x = mk_var "x" in
  let y = mk_var "y" in
  let f = mk_fun "P" [ x; y ] in
  print_endline (to_string f);
  [%expect {| P(x,y) |}]
;;

let%expect_test "create_function_with_index" =
  let x = mk_var "x" in
  let f = mk_fun_indexed "P" "1" [ x ] in
  print_endline (to_string f);
  [%expect {| P(1)(x) |}]
;;

let%expect_test "create_negation" =
  let t = True in
  let neg_t = mk_neg t in
  print_endline (to_string neg_t);
  [%expect {| ¬True |}]
;;

let%expect_test "create_conjunction" =
  let t = True in
  let f = False in
  let conj = mk_and t f in
  print_endline (to_string conj);
  [%expect {| True ∧ False |}]
;;

let%expect_test "create_disjunction" =
  let t = True in
  let f = False in
  let disj = mk_or t f in
  print_endline (to_string disj);
  [%expect {| True ∨ False |}]
;;

let%expect_test "create_implication" =
  let t = True in
  let f = False in
  let impl = mk_implies t f in
  print_endline (to_string impl);
  [%expect {| True → False |}]
;;

let%expect_test "create_equivalence" =
  let t = True in
  let f = False in
  let equiv = mk_equiv t f in
  print_endline (to_string equiv);
  [%expect {| True ↔ False |}]
;;

let%expect_test "create_equality" =
  let x = mk_var "x" in
  let y = mk_var "y" in
  let eq = mk_equal (Var x) (Var y) in
  print_endline (to_string eq);
  [%expect {| x = y |}]
;;

let%expect_test "create_next" =
  let t = True in
  let next_t = mk_next t in
  print_endline (to_string next_t);
  [%expect {| X True |}]
;;

let%expect_test "create_globally" =
  let t = True in
  let glob_t = mk_globally t in
  print_endline (to_string glob_t);
  [%expect {| G True |}]
;;

let%expect_test "create_finally" =
  let t = True in
  let fin_t = mk_finally t in
  print_endline (to_string fin_t);
  [%expect {| F True |}]
;;

let%expect_test "create_until" =
  let p = Var (mk_var "p") in
  let q = Var (mk_var "q") in
  let until = mk_until p q in
  print_endline (to_string until);
  [%expect {| p U q |}]
;;

let%expect_test "create_weak_until" =
  let p = Var (mk_var "p") in
  let q = Var (mk_var "q") in
  let wuntil = mk_wuntil p q in
  print_endline (to_string wuntil);
  [%expect {| p W q |}]
;;

let%expect_test "create_exists" =
  let x = mk_var "x" in
  let p = mk_fun "P" [ x ] in
  let exists = mk_exists [ x ] p in
  print_endline (to_string exists);
  [%expect {| ∃ x:T. P(x) |}]
;;

let%expect_test "create_forall" =
  let x = mk_var "x" in
  let p = mk_fun "P" [ x ] in
  let forall = mk_forall [ x ] p in
  print_endline (to_string forall);
  [%expect {| ∀ x:T. P(x) |}]
;;

let%expect_test "create_forall_other_than" =
  let x = mk_var "x" in
  let y = mk_var "y" in
  let p = mk_fun "P" [ x ] in
  let forall_ot = mk_forall_other_than [ x ] [ y ] p in
  print_endline (to_string forall_ot);
  [%expect {| ∀ x:T ∉ {y:T}. P(x) |}]
;;

let%expect_test "empty_quantifier_lists" =
  let p = mk_fun "P" [] in
  let exists_empty = mk_exists [] p in
  let forall_empty = mk_forall [] p in
  print_endline (to_string exists_empty);
  print_endline (to_string forall_empty);
  [%expect
    {|
    P()
    P() |}]
;;

let%expect_test "forall_other_than_empty_lists" =
  let x = mk_var "x" in
  let p = mk_fun "P" [ x ] in
  let forall_ot_empty_vars = mk_forall_other_than [] [] p in
  let forall_ot_empty_other = mk_forall_other_than [ x ] [] p in
  print_endline (to_string forall_ot_empty_vars);
  print_endline (to_string forall_ot_empty_other);
  [%expect
    {|
    P(x)
    ∀ x:T. P(x) |}]
;;

let%expect_test "mk_and_list_empty" =
  let result = mk_and_list [] in
  print_endline (to_string result);
  [%expect {| True |}]
;;

let%expect_test "mk_and_list_single" =
  let p = Var (mk_var "p") in
  let result = mk_and_list [ p ] in
  print_endline (to_string result);
  [%expect {| p |}]
;;

let%expect_test "mk_and_list_with_true" =
  let p = Var (mk_var "p") in
  let result = mk_and_list [ True; p; True ] in
  print_endline (to_string result);
  [%expect {| p |}]
;;

let%expect_test "mk_and_list_with_false" =
  let p = Var (mk_var "p") in
  let result = mk_and_list [ p; False; True ] in
  print_endline (to_string result);
  [%expect {| False |}]
;;

let%expect_test "mk_and_list_multiple" =
  let p = Var (mk_var "p") in
  let q = Var (mk_var "q") in
  let r = Var (mk_var "r") in
  let result = mk_and_list [ p; q; r ] in
  print_endline (to_string result);
  [%expect {| (p ∧ q) ∧ r |}]
;;

let%expect_test "mk_or_list_empty" =
  let result = mk_or_list [] in
  print_endline (to_string result);
  [%expect {| False |}]
;;

let%expect_test "mk_or_list_single" =
  let p = Var (mk_var "p") in
  let result = mk_or_list [ p ] in
  print_endline (to_string result);
  [%expect {| p |}]
;;

let%expect_test "mk_or_list_with_false" =
  let p = Var (mk_var "p") in
  let result = mk_or_list [ False; p; False ] in
  print_endline (to_string result);
  [%expect {| p |}]
;;

let%expect_test "mk_or_list_with_true" =
  let p = Var (mk_var "p") in
  let result = mk_or_list [ p; True; False ] in
  print_endline (to_string result);
  [%expect {| True |}]
;;

let%expect_test "mk_or_list_multiple" =
  let p = Var (mk_var "p") in
  let q = Var (mk_var "q") in
  let r = Var (mk_var "r") in
  let result = mk_or_list [ p; q; r ] in
  print_endline (to_string result);
  [%expect {| (p ∨ q) ∨ r |}]
;;

let%expect_test "mk_implies_list_empty" =
  let result = mk_implies_list [] in
  print_endline (to_string result);
  [%expect {| True |}]
;;

let%expect_test "mk_implies_list_single" =
  let p = Var (mk_var "p") in
  let result = mk_implies_list [ p ] in
  print_endline (to_string result);
  [%expect {| p |}]
;;

let%expect_test "mk_implies_list_two" =
  let p = Var (mk_var "p") in
  let q = Var (mk_var "q") in
  let result = mk_implies_list [ p; q ] in
  print_endline (to_string result);
  [%expect {| p → q |}]
;;

let%expect_test "mk_implies_list_three" =
  let p = Var (mk_var "p") in
  let q = Var (mk_var "q") in
  let r = Var (mk_var "r") in
  let result = mk_implies_list [ p; q; r ] in
  print_endline (to_string result);
  [%expect {| (p → q) → r |}]
;;

let%expect_test "mk_equiv_list_empty" =
  let result = mk_equiv_list [] in
  print_endline (to_string result);
  [%expect {| True |}]
;;

let%expect_test "mk_equiv_list_single" =
  let p = Var (mk_var "p") in
  let result = mk_equiv_list [ p ] in
  print_endline (to_string result);
  [%expect {| p |}]
;;

let%expect_test "mk_equiv_list_two" =
  let p = Var (mk_var "p") in
  let q = Var (mk_var "q") in
  let result = mk_equiv_list [ p; q ] in
  print_endline (to_string result);
  [%expect {| p ↔ q |}]
;;

let%expect_test "mk_equiv_list_three" =
  let p = Var (mk_var "p") in
  let q = Var (mk_var "q") in
  let r = Var (mk_var "r") in
  let result = mk_equiv_list [ p; q; r ] in
  print_endline (to_string result);
  [%expect {| (p ↔ q) ↔ r |}]
;;

let%expect_test "mk_equal_list_empty" =
  let result = mk_equal_list [] in
  print_endline (to_string result);
  [%expect {| True |}]
;;

let%expect_test "mk_equal_list_single" =
  let p = Var (mk_var "p") in
  let result = mk_equal_list [ p ] in
  print_endline (to_string result);
  [%expect {| p |}]
;;

let%expect_test "mk_equal_list_two" =
  let p = Var (mk_var "p") in
  let q = Var (mk_var "q") in
  let result = mk_equal_list [ p; q ] in
  print_endline (to_string result);
  [%expect {| p = q |}]
;;

let%expect_test "to_typed_string_basic" =
  let x = mk_var "x" in
  let p = mk_fun "P" [ x ] in
  print_endline (to_string ~annotate:true p);
  [%expect {| P(x:T) |}]
;;

let%expect_test "to_typed_string_with_types" =
  let x = mk_var ~typ:"Int" "x" in
  let y = mk_var ~typ:"Bool" "y" in
  let p = mk_fun "P" [ x; y ] in
  print_endline (to_string ~annotate:true p);
  [%expect {| P(x:Int,y:Bool) |}]
;;

let%expect_test "to_typed_string_quantifier" =
  let x = mk_var "x" in
  let p = mk_fun "P" [ x ] in
  let exists = mk_exists [ x ] p in
  print_endline (to_string ~annotate:true exists);
  [%expect {| ∃ x:T. P(x:T) |}]
;;

let%expect_test "to_typed_string_forall_other_than" =
  let x = mk_var "x" in
  let y = mk_var "y" in
  let p = mk_fun "P" [ x ] in
  let forall_ot = mk_forall_other_than [ x ] [ y ] p in
  print_endline (to_string ~annotate:true forall_ot);
  [%expect {| ∀ x:T ∉ {y:T}. P(x:T) |}]
;;

let%expect_test "pretty_print_simple" =
  let t = True in
  print_endline (pretty_print t);
  [%expect {| True |}]
;;

let%expect_test "pretty_print_conjunction" =
  let p = Var (mk_var "p") in
  let q = Var (mk_var "q") in
  let conj = mk_and p q in
  print_endline (pretty_print conj);
  [%expect
    {|
  (
    p
   ∧
    q
  ) |}]
;;

let%expect_test "pretty_print_quantifier" =
  let x = mk_var "x" in
  let p = mk_fun "P" [ x ] in
  let forall = mk_forall [ x ] p in
  print_endline (pretty_print forall);
  [%expect
    {|
  ∀ x:T.
  P(x) |}]
;;

let%expect_test "encode_fun_to_var_simple" =
  let x = mk_var "x" in
  let f = mk_fun "P" [ x ] in
  let encoded = encode_fun_to_var f in
  print_endline (var_to_string encoded);
  [%expect {| P_x#T |}]
;;

let%expect_test "encode_fun_to_var_with_index" =
  let x = mk_var "x" in
  let f = mk_fun_indexed "P" "1" [ x ] in
  let encoded = encode_fun_to_var f in
  print_endline (var_to_string encoded);
  [%expect {| P#1_x#T |}]
;;

let%expect_test "encode_fun_to_var_multiple_params" =
  let x = mk_var ~typ:"Int" "x" in
  let y = mk_var ~typ:"Bool" "y" in
  let f = mk_fun "P" [ x; y ] in
  let encoded = encode_fun_to_var f in
  print_endline (var_to_string encoded);
  [%expect {| P_x#Int_y#Bool |}]
;;

let%expect_test "encode_fun_name_simple" =
  let x = mk_var "x" in
  let f = mk_fun "P" [ x ] in
  print_endline (encode_fun_name f);
  [%expect {| P |}]
;;

let%expect_test "encode_fun_name_with_index" =
  let x = mk_var "x" in
  let f = mk_fun_indexed "P" "1" [ x ] in
  print_endline (encode_fun_name f);
  [%expect {| P#1 |}]
;;

(* We ignore the below test cases for now. *)
(* 
let%expect_test "decode_var_to_fun_simple" =
  let decoded = decode_var_to_fun "P_x#T" in
  match decoded with
  | Some f -> print_endline (to_string f)
  | None -> print_endline "None";
  [%expect {| P(x) |}]

let%expect_test "decode_var_to_fun_with_index" =
  let decoded = decode_var_to_fun "P#1_x#T" in
  match decoded with
  | Some f -> print_endline (to_string f)
  | None -> print_endline "None";
  [%expect {| P(1)(x) |}]

let%expect_test "decode_var_to_fun_multiple_params" =
  let decoded = decode_var_to_fun "P_x#Int_y#Bool" in
  match decoded with
  | Some f -> print_endline (to_string ~annotate:true f)
  | None -> print_endline "None";
  [%expect {| P(x:Int,y:Bool) |}]

let%expect_test "decode_var_to_fun_invalid" =
  let decoded = decode_var_to_fun "invalid" in
  match decoded with
  | Some f -> print_endline (to_string f)
  | None -> print_endline "None";
  [%expect {| None |}]

let%expect_test "decode_fun_name_simple" =
  let decoded = decode_fun_name "P_x#T" in
  match decoded with
  | Some (name, idx) -> 
      Printf.printf "name: %s, index: %s\n" name (match idx with None -> "None" | Some i -> i)
  | None -> print_endline "None";
  [%expect {| name: P, index: None |}]

let%expect_test "decode_fun_name_with_index" =
  let decoded = decode_fun_name "P#1_x#T" in
  match decoded with
  | Some (name, idx) -> 
      Printf.printf "name: %s, index: %s\n" name (match idx with None -> "None" | Some i -> i)
  | None -> print_endline "None";
  [%expect {| name: P, index: 1 |}] *)

let%expect_test "is_oracle" =
  print_endline (string_of_bool (is_oracle "O1"));
  print_endline (string_of_bool (is_oracle "P1"));
  [%expect
    {|
    true
    false |}]
;;

let%expect_test "is_const_input" =
  print_endline (string_of_bool (is_const_input "I1"));
  print_endline (string_of_bool (is_const_input "P1"));
  [%expect
    {|
    true
    false |}]
;;

let%expect_test "is_aux" =
  print_endline (string_of_bool (is_aux "choice1"));
  print_endline (string_of_bool (is_aux "P1"));
  [%expect
    {|
    true
    false |}]
;;

let%expect_test "is_b" =
  print_endline (string_of_bool (is_b "B1"));
  print_endline (string_of_bool (is_b "P1"));
  [%expect
    {|
    true
    false |}]
;;

let%expect_test "free_vars_simple" =
  let x = mk_var "x" in
  let p = Var x in
  let vars = free_vars p in
  print_endline (String.concat ~sep:", " (List.map ~f:var_to_string vars));
  [%expect {| x |}]
;;

let%expect_test "free_vars_function" =
  let x = mk_var "x" in
  let y = mk_var "y" in
  let f = mk_fun "P" [ x; y ] in
  let vars = free_vars f in
  print_endline (String.concat ~sep:", " (List.map ~f:var_to_string vars));
  [%expect {| x, y |}]
;;

let%expect_test "free_vars_quantified" =
  let x = mk_var "x" in
  let y = mk_var "y" in
  let p = mk_fun "P" [ x; y ] in
  let exists_x = mk_exists [ x ] p in
  let vars = free_vars exists_x in
  print_endline (String.concat ~sep:", " (List.map ~f:var_to_string vars));
  [%expect {| y |}]
;;

let%expect_test "free_vars_forall_other_than" =
  let x = mk_var "x" in
  let y = mk_var "y" in
  let z = mk_var "z" in
  let p = mk_fun "P" [ x; y; z ] in
  let forall_ot = mk_forall_other_than [ x ] [ y ] p in
  let vars = free_vars forall_ot in
  print_endline (String.concat ~sep:", " (List.map ~f:var_to_string vars));
  [%expect {| z |}]
;;

let%expect_test "bound_vars_exists" =
  let x = mk_var "x" in
  let y = mk_var "y" in
  let p = mk_fun "P" [ x; y ] in
  let exists_x = mk_exists [ x ] p in
  let vars = bound_vars exists_x in
  print_endline (String.concat ~sep:", " (List.map ~f:var_to_string vars));
  [%expect {| x |}]
;;

let%expect_test "bound_vars_nested" =
  let x = mk_var "x" in
  let y = mk_var "y" in
  let p = mk_fun "P" [ x; y ] in
  let inner = mk_forall [ y ] p in
  let outer = mk_exists [ x ] inner in
  let vars = bound_vars outer in
  print_endline (String.concat ~sep:", " (List.map ~f:var_to_string vars));
  [%expect {| x, y |}]
;;

let%expect_test "opsize_constants" =
  let t = True in
  let f = False in
  print_endline (string_of_int (opsize t));
  print_endline (string_of_int (opsize f));
  [%expect
    {|
    0
    0 |}]
;;

let%expect_test "opsize_function" =
  let x = mk_var "x" in
  let f = mk_fun "P" [ x ] in
  print_endline (string_of_int (opsize f));
  [%expect {| 1 |}]
;;

let%expect_test "opsize_negation" =
  let t = True in
  let neg_t = mk_neg t in
  print_endline (string_of_int (opsize neg_t));
  [%expect {| 1 |}]
;;

let%expect_test "opsize_conjunction" =
  let p = Var (mk_var "p") in
  let q = Var (mk_var "q") in
  let conj = mk_and p q in
  print_endline (string_of_int (opsize conj));
  [%expect {| 1 |}]
;;

let%expect_test "opsize_quantifier" =
  let x = mk_var "x" in
  let p = mk_fun "P" [ x ] in
  let forall = mk_forall [ x ] p in
  print_endline (string_of_int (opsize forall));
  [%expect {| 2 |}]
;;

let%expect_test "opsize_complex" =
  let x = mk_var "x" in
  let p = mk_fun "P" [ x ] in
  let q = mk_fun "Q" [ x ] in
  let conj = mk_and p q in
  let neg_conj = mk_neg conj in
  let forall = mk_forall [ x ] neg_conj in
  print_endline (string_of_int (opsize forall));
  [%expect {| 5 |}]
;;

let%expect_test "is_qfree_true" =
  let p = Var (mk_var "p") in
  let q = Var (mk_var "q") in
  let conj = mk_and p q in
  print_endline (string_of_bool (is_qfree conj));
  [%expect {| true |}]
;;

let%expect_test "is_qfree_false" =
  let x = mk_var "x" in
  let p = mk_fun "P" [ x ] in
  let exists = mk_exists [ x ] p in
  print_endline (string_of_bool (is_qfree exists));
  [%expect {| false |}]
;;

let%expect_test "is_universal_true" =
  let x = mk_var "x" in
  let p = mk_fun "P" [ x ] in
  let forall = mk_forall [ x ] p in
  print_endline (string_of_bool (is_universal forall));
  [%expect {| true |}]
;;

let%expect_test "is_universal_false_exists" =
  let x = mk_var "x" in
  let p = mk_fun "P" [ x ] in
  let exists = mk_exists [ x ] p in
  print_endline (string_of_bool (is_universal exists));
  [%expect {| false |}]
;;

let%expect_test "is_universal_false_ot" =
  let x = mk_var "x" in
  let y = mk_var "y" in
  let p = mk_fun "P" [ x ] in
  let forall_ot = mk_forall_other_than [ x ] [ y ] p in
  print_endline (string_of_bool (is_universal forall_ot));
  [%expect {| false |}]
;;

let%expect_test "is_bs_universal_qfree" =
  let x = mk_var "x" in
  let p = mk_fun "P" [ x ] in
  let q = mk_fun "Q" [ x ] in
  let conj = mk_and p q in
  let forall = mk_forall [ x ] conj in
  print_endline (string_of_bool (is_bs forall));
  [%expect {| true |}]
;;

let%expect_test "is_bs_with_existential" =
  let x = mk_var "x" in
  let y = mk_var "y" in
  let p = mk_fun "P" [ x; y ] in
  let inner = mk_forall [ y ] p in
  let exists = mk_exists [ x ] inner in
  print_endline (string_of_bool (is_bs exists));
  [%expect {| false |}]
;;

let%expect_test "is_bs_qfree_only" =
  let p = Var (mk_var "p") in
  let q = Var (mk_var "q") in
  let conj = mk_and p q in
  print_endline (string_of_bool (is_bs conj));
  [%expect {| true |}]
;;
