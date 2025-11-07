open! Core

type var =
  { name : string
  ; typ : string
  }
[@@deriving sexp_of]

type t =
  (* Base cases *)
  | True
  | False
  | Var of var
  | Fun of string * string option * var list
  (* Propositional logic *)
  | Neg of t
  | And of t * t
  | Or of t * t
  | Implies of t * t
  | Equiv of t * t
  | Equal of t * t
  (* Quantifiers *)
  | Exists of var list * t
  | Forall of var list * t
  | ForallOtherThan of var list * var list * t
  (* Temporal operators *)
  | Next of t
  | Globally of t
  | Finally of t
  | Until of t * t
  | WUntil of t * t
[@@deriving sexp_of]

let equal = Poly.equal

(* Variable constructor with default type *)
let mk_var ?(typ = "T") name = { name; typ }

(* Function constructor with simplified signature *)
let mk_fun name params = Fun (name, None, params)
let mk_fun_indexed name index params = Fun (name, Some index, params)

(* Basic binary operators *)
let mk_and t1 t2 = And (t1, t2)
let mk_or t1 t2 = Or (t1, t2)
let mk_implies t1 t2 = Implies (t1, t2)
let mk_equiv t1 t2 = Equiv (t1, t2)
let mk_equal t1 t2 = Equal (t1, t2)

(* Unary operators *)
let mk_neg t = Neg t
let mk_next t = Next t
let mk_globally t = Globally t
let mk_finally t = Finally t
let mk_until t1 t2 = Until (t1, t2)
let mk_wuntil t1 t2 = WUntil (t1, t2)

(* Quantifier constructors that handle empty lists *)
let mk_exists (vars : var list) formula =
  if List.is_empty vars then formula else Exists (vars, formula)
;;

let mk_forall (vars : var list) formula =
  if List.is_empty vars then formula else Forall (vars, formula)
;;

let mk_forall_other_than (vars : var list) (otherthan : var list) formula =
  if List.is_empty otherthan || List.is_empty vars
  then mk_forall vars formula
  else ForallOtherThan (vars, otherthan, formula)
;;

(* Name-only vs typed variable string *)
let var_to_string ?(annotate = false) v =
  if annotate then v.name ^ ":" ^ v.typ else v.name
;;

(* Always-typed list of variables (used in binders) *)
let join_typed_vars vars =
  String.concat ~sep:"," (List.map ~f:(fun v -> v.name ^ ":" ^ v.typ) vars)
;;

let to_string ?(annotate = false) t =
  let rec go t =
    match t with
    | True -> "True"
    | False -> "False"
    | Var v -> var_to_string ~annotate v
    | Fun (name, None, params) ->
      name
      ^ "("
      ^ String.concat ~sep:"," (List.map ~f:(var_to_string ~annotate) params)
      ^ ")"
    | Fun (name, Some idx, params) ->
      name
      ^ "("
      ^ idx
      ^ ")("
      ^ String.concat ~sep:"," (List.map ~f:(var_to_string ~annotate) params)
      ^ ")"
    | Neg t' -> "¬" ^ bracketed t'
    | And (t1, t2) -> bracketed t1 ^ " ∧ " ^ bracketed t2
    | Or (t1, t2) -> bracketed t1 ^ " ∨ " ^ bracketed t2
    | Implies (t1, t2) -> bracketed t1 ^ " → " ^ bracketed t2
    | Equiv (t1, t2) -> bracketed t1 ^ " ↔ " ^ bracketed t2
    | Equal (t1, t2) -> bracketed t1 ^ " = " ^ bracketed t2
    | Next t' -> "X " ^ bracketed t'
    | Globally t' -> "G " ^ bracketed t'
    | Finally t' -> "F " ^ bracketed t'
    | Until (t1, t2) -> bracketed t1 ^ " U " ^ bracketed t2
    | WUntil (t1, t2) -> bracketed t1 ^ " W " ^ bracketed t2
    | Exists (vars, t') -> "∃ " ^ join_typed_vars vars ^ ". " ^ bracketed t'
    | Forall (vars, t') -> "∀ " ^ join_typed_vars vars ^ ". " ^ bracketed t'
    | ForallOtherThan (vars, otherthan, t') ->
      "∀ "
      ^ join_typed_vars vars
      ^ " ∉ {"
      ^ join_typed_vars otherthan
      ^ "}. "
      ^ bracketed t'
  and bracketed t' =
    match t' with
    | True | False | Var _ | Fun _ -> go t'
    | _ -> "(" ^ go t' ^ ")"
  in
  go t
;;

(* Smart constructors for lists *)
let mk_and_list formulas =
  match List.filter ~f:(fun x -> not (phys_equal x True)) formulas with
  | [] -> True
  | [ f ] -> f
  | fs when List.mem fs False ~equal:phys_equal -> False
  | fs ->
    List.fold_left ~init:(List.hd_exn fs) ~f:(fun acc f -> And (acc, f)) (List.tl_exn fs)
;;

let mk_or_list formulas =
  match List.filter ~f:(fun x -> not (phys_equal x False)) formulas with
  | [] -> False
  | [ f ] -> f
  | fs when List.mem fs True ~equal:phys_equal -> True
  | fs ->
    List.fold_left ~init:(List.hd_exn fs) ~f:(fun acc f -> Or (acc, f)) (List.tl_exn fs)
;;

let mk_implies_list formulas =
  match formulas with
  | [] -> True
  | [ f ] -> f
  | _ ->
    let rec aux acc = function
      | [] -> acc
      | [ last ] -> Implies (acc, last)
      | hd :: tl -> aux (Implies (acc, hd)) tl
    in
    aux (List.hd_exn formulas) (List.tl_exn formulas)
;;

let mk_equiv_list formulas =
  match formulas with
  | [] -> True
  | [ f ] -> f
  | [ f1; f2 ] -> Equiv (f1, f2)
  | _ ->
    let rec aux acc = function
      | [] -> acc
      | [ last ] -> Equiv (acc, last)
      | hd :: tl -> aux (Equiv (acc, hd)) tl
    in
    aux (List.hd_exn formulas) (List.tl_exn formulas)
;;

let mk_equal_list formulas =
  match formulas with
  | [] -> True
  | [ f ] -> f
  | [ f1; f2 ] -> Equal (f1, f2)
  | _ -> failwith "mk_equal_list: can only handle 0, 1, or 2 formulas"
;;

(* Fun-specific utilities *)
let is_oracle name = String.is_prefix ~prefix:"O" name
let is_const_input name = String.is_prefix ~prefix:"I" name
let is_aux name = String.is_prefix ~prefix:"choice" name
let is_b name = String.is_prefix ~prefix:"B" name

(* Analysis functions *)

(* Compute free variables (variables not bound by quantifiers) *)
let rec free_vars t =
  match t with
  | True | False -> []
  | Var v -> [ v ]
  | Fun (_, _, params) -> params
  | Neg f -> free_vars f
  | And (f1, f2)
  | Or (f1, f2)
  | Implies (f1, f2)
  | Equiv (f1, f2)
  | Equal (f1, f2)
  | Until (f1, f2)
  | WUntil (f1, f2) -> free_vars f1 @ free_vars f2
  | Next f | Globally f | Finally f -> free_vars f
  | Exists (vars, f) | Forall (vars, f) ->
    List.filter ~f:(fun v -> not (List.mem vars v ~equal:phys_equal)) (free_vars f)
  | ForallOtherThan (vars, otherthan, f) ->
    let bound = vars @ otherthan in
    List.filter ~f:(fun v -> not (List.mem bound v ~equal:phys_equal)) (free_vars f)
;;

(* Compute bound variables (variables bound by quantifiers) *)
let rec bound_vars t =
  match t with
  | True | False | Var _ | Fun _ -> []
  | Neg f -> bound_vars f
  | And (f1, f2)
  | Or (f1, f2)
  | Implies (f1, f2)
  | Equiv (f1, f2)
  | Equal (f1, f2)
  | Until (f1, f2)
  | WUntil (f1, f2) -> bound_vars f1 @ bound_vars f2
  | Next f | Globally f | Finally f -> bound_vars f
  | Exists (vars, f) | Forall (vars, f) -> vars @ bound_vars f
  | ForallOtherThan (vars, otherthan, f) -> vars @ otherthan @ bound_vars f
;;

(* Compute operator size (number of operators in the formula tree) *)
let rec opsize t =
  match t with
  | True | False | Var _ -> 0
  | Fun _ -> 1 (* Function application counts as an operator *)
  | Neg f -> 1 + opsize f
  | And (f1, f2)
  | Or (f1, f2)
  | Implies (f1, f2)
  | Equiv (f1, f2)
  | Equal (f1, f2)
  | Until (f1, f2)
  | WUntil (f1, f2) -> 1 + opsize f1 + opsize f2
  | Next f | Globally f | Finally f -> 1 + opsize f
  | Exists (_, f) | Forall (_, f) | ForallOtherThan (_, _, f) -> 1 + opsize f
;;

(* Check if formula is quantifier-free *)
let rec is_qfree t =
  match t with
  | True | False | Var _ -> true
  | Fun _ -> true
  | Neg f -> is_qfree f
  | And (f1, f2)
  | Or (f1, f2)
  | Implies (f1, f2)
  | Equiv (f1, f2)
  | Equal (f1, f2)
  | Until (f1, f2)
  | WUntil (f1, f2) -> is_qfree f1 && is_qfree f2
  | Next f | Globally f | Finally f -> is_qfree f
  | Exists _ | Forall _ | ForallOtherThan _ -> false
;;

(* Check if formula is universal (only ∀ quantifiers, no ∃) *)
let rec is_universal t =
  match t with
  | True | False | Var _ -> true
  | Fun _ -> true
  | Neg f -> is_universal f
  | And (f1, f2)
  | Or (f1, f2)
  | Implies (f1, f2)
  | Equiv (f1, f2)
  | Equal (f1, f2)
  | Until (f1, f2)
  | WUntil (f1, f2) -> is_universal f1 && is_universal f2
  | Next f | Globally f | Finally f -> is_universal f
  | Exists _ | ForallOtherThan _ -> false
  | Forall (_, f) -> is_universal f
;;

(* Check if formula is in Bernays-Schönfinkel class *)
(* BS class: universal prefix followed by quantifier-free matrix *)
let is_bs t =
  let rec has_existential_prefix f =
    match f with
    | Exists _ -> true
    | Forall (_, f') -> has_existential_prefix f'
    | ForallOtherThan (_, _, f') -> has_existential_prefix f'
    | _ -> false
  in
  let rec strip_universal_prefix f =
    match f with
    | Forall (_, f') -> strip_universal_prefix f'
    | ForallOtherThan (_, _, f') -> strip_universal_prefix f'
    | _ -> f
  in
  (not (has_existential_prefix t)) && is_qfree (strip_universal_prefix t)
;;
