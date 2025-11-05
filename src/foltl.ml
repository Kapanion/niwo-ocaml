open! Core

type var = {
  name : string;
  typ : string;
}

type formula =
  (* Base cases *)
  | True
  | False
  | Var of var
  | Fun of string * string option * var list
  
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
  
  (* Temporal operators *)
  | Next of formula
  | Globally of formula
  | Finally of formula
  | Until of formula * formula
  | WUntil of formula * formula
[@@deriving equal, sexp_of]

(* Variable constructor with default type *)
let mk_var ?(typ="T") name = { name; typ }

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

let mk_forall (vars : var list) formula =
  if List.is_empty vars then formula else Forall (vars, formula)

let mk_forall_other_than (vars : var list) (otherthan : var list) formula =
  if List.is_empty otherthan || List.is_empty vars then mk_forall vars formula
  else ForallOtherThan (vars, otherthan, formula)

(* Convert var to string without type *)
let var_to_string var = var.name

(* Convert var to string with type annotation *)
let var_to_string_typed var = var.name ^ ":" ^ var.typ

(* String conversion with type annotations *)
let rec to_typed_string formula =
  match formula with
  | True -> "True"
  | False -> "False"
  | Var v -> var_to_string_typed v
  | Fun (name, None, params) ->
      name ^ "(" ^ (String.concat ~sep:"," (List.map ~f:var_to_string_typed params)) ^ ")"
  | Fun (name, Some idx, params) ->
      name ^ "(" ^ idx ^ ")(" ^ (String.concat ~sep:"," (List.map ~f:var_to_string_typed params)) ^ ")"
  | Neg f -> "¬" ^ to_typed_string f
  | And (f1, f2) -> "(" ^ to_typed_string f1 ^ " ∧ " ^ to_typed_string f2 ^ ")"
  | Or (f1, f2) -> "(" ^ to_typed_string f1 ^ " ∨ " ^ to_typed_string f2 ^ ")"
  | Implies (f1, f2) -> "(" ^ to_typed_string f1 ^ " → " ^ to_typed_string f2 ^ ")"
  | Equiv (f1, f2) -> "(" ^ to_typed_string f1 ^ " ↔ " ^ to_typed_string f2 ^ ")"
  | Equal (f1, f2) -> "(" ^ to_typed_string f1 ^ " = " ^ to_typed_string f2 ^ ")"
  | Next f -> "X " ^ to_typed_string f
  | Globally f -> "G " ^ to_typed_string f
  | Finally f -> "F " ^ to_typed_string f
  | Until (f1, f2) -> "(" ^ to_typed_string f1 ^ " U " ^ to_typed_string f2 ^ ")"
  | WUntil (f1, f2) -> "(" ^ to_typed_string f1 ^ " W " ^ to_typed_string f2 ^ ")"
  | Exists (vars, f) ->
      "∃ " ^ (String.concat ~sep:"," (List.map ~f:var_to_string_typed vars)) ^ ". " ^ to_typed_string f
  | Forall (vars, f) ->
      "∀ " ^ (String.concat ~sep:"," (List.map ~f:var_to_string_typed vars)) ^ ". " ^ to_typed_string f
  | ForallOtherThan (vars, otherthan, f) ->
      "∀ " ^ (String.concat ~sep:"," (List.map ~f:var_to_string_typed vars)) ^
      " ∉ {" ^ String.concat ~sep:"," (List.map ~f:var_to_string_typed otherthan) ^ "}. " ^
      to_typed_string f

(* Basic to_string function *)
let rec to_string formula =
  match formula with
  | True -> "True"
  | False -> "False"
  | Var v -> var_to_string v
  | Fun (name, None, params) ->
      name ^ "(" ^ String.concat ~sep:"," (List.map ~f:var_to_string params) ^ ")"
  | Fun (name, Some idx, params) ->
      name ^ "(" ^ idx ^ ")(" ^ String.concat ~sep:"," (List.map ~f:var_to_string params) ^ ")"
  | Neg t -> "¬" ^ bracketed t
  | And (t1, t2) -> bracketed t1 ^ " ∧ " ^ bracketed t2
  | Or (t1, t2) -> bracketed t1 ^ " ∨ " ^ bracketed t2
  | Implies (t1, t2) -> bracketed t1 ^ " → " ^ bracketed t2
  | Equiv (t1, t2) -> bracketed t1 ^ " ↔ " ^ bracketed t2
  | Equal (t1, t2) -> bracketed t1 ^ " = " ^ bracketed t2
  | Exists (vars, t) ->
      "∃ " ^ String.concat ~sep:"," (List.map ~f:(fun v -> v.name ^ ":" ^ v.typ) vars) ^ ". " ^ bracketed t
  | Forall (vars, t) ->
      "∀ " ^ String.concat ~sep:"," (List.map ~f:(fun v -> v.name ^ ":" ^ v.typ) vars) ^ ". " ^ bracketed t
  | ForallOtherThan (vars, otherthan, t) ->
      "∀ " ^ String.concat ~sep:"," (List.map ~f:(fun v -> v.name ^ ":" ^ v.typ) vars) ^
      " ∉ {" ^ String.concat ~sep:"," (List.map ~f:(fun v -> v.name ^ ":" ^ v.typ) otherthan) ^
      "}. " ^ bracketed t
  | Next t -> "X " ^ bracketed t
  | Globally t -> "G " ^ bracketed t
  | Finally t -> "F " ^ bracketed t
  | Until (t1, t2) -> bracketed t1 ^ " U " ^ bracketed t2
  | WUntil (t1, t2) -> bracketed t1 ^ " W " ^ bracketed t2

and bracketed formula =
  match formula with
  | And _ | Or _ | Implies _ | Equiv _ | Equal _ -> "(" ^ to_string formula ^ ")"
  | _ -> to_string formula

(* Smart constructors for lists *)

let mk_and_list formulas =
  match List.filter ~f:(fun x -> not (phys_equal x True)) formulas with
  | [] -> True
  | [f] -> f
  | fs when List.mem fs False ~equal:phys_equal -> False
  | fs -> List.fold_left ~init:(List.hd_exn fs) ~f:(fun acc f -> And (acc, f)) (List.tl_exn fs)

let mk_or_list formulas =
  match List.filter ~f:(fun x -> not (phys_equal x False)) formulas with
  | [] -> False
  | [f] -> f
  | fs when List.mem fs True ~equal:phys_equal -> True
  | fs -> List.fold_left ~init:(List.hd_exn fs) ~f:(fun acc f -> Or (acc, f)) (List.tl_exn fs)

let mk_implies_list formulas =
  match formulas with
  | [] -> True
  | [f] -> f
  | _ -> 
      let rec aux acc = function
        | [] -> acc
        | [last] -> Implies (acc, last)
        | hd :: tl -> aux (Implies (acc, hd)) tl
      in
      aux (List.hd_exn formulas) (List.tl_exn formulas)

let mk_equiv_list formulas =
  match formulas with
  | [] -> True
  | [f] -> f
  | [f1; f2] -> Equiv (f1, f2)
  | _ -> 
      let rec aux acc = function
        | [] -> acc
        | [last] -> Equiv (acc, last)
        | hd :: tl -> aux (Equiv (acc, hd)) tl
      in
      aux (List.hd_exn formulas) (List.tl_exn formulas)

let mk_equal_list formulas =
  match formulas with
  | [] -> True
  | [f] -> f
  | [f1; f2] -> Equal (f1, f2)
  | _ -> failwith "mk_equal_list: can only handle 0, 1, or 2 formulas"

(* Pretty-print with indentation *)
let rec pretty_print ?(indent=0) formula =
  let indent_str = String.make indent ' ' in
  match formula with
  | True -> indent_str ^ "True"
  | False -> indent_str ^ "False"
  | Var v -> indent_str ^ var_to_string v
  | Fun (name, None, params) ->
      indent_str ^ name ^ "(" ^ String.concat ~sep:"," (List.map ~f:var_to_string params) ^ ")"
  | Fun (name, Some idx, params) ->
      indent_str ^ name ^ "(" ^ idx ^ ")(" ^ String.concat ~sep:"," (List.map ~f:var_to_string params) ^ ")"
  | Neg f -> indent_str ^ "¬" ^ pretty_print f
  | And (f1, f2) ->
      indent_str ^ "(\n" ^
      pretty_print ~indent:(indent+2) f1 ^ "\n" ^
      indent_str ^ " ∧\n" ^
      pretty_print ~indent:(indent+2) f2 ^ "\n" ^
      indent_str ^ ")"
  | Or (f1, f2) ->
      indent_str ^ "(\n" ^
      pretty_print ~indent:(indent+2) f1 ^ "\n" ^
      indent_str ^ " ∨\n" ^
      pretty_print ~indent:(indent+2) f2 ^ "\n" ^
      indent_str ^ ")"
  | Implies (f1, f2) ->
      indent_str ^ "(\n" ^
      pretty_print ~indent:(indent+2) f1 ^ "\n" ^
      indent_str ^ " →\n" ^
      pretty_print ~indent:(indent+2) f2 ^ "\n" ^
      indent_str ^ ")"
  | Equiv (f1, f2) ->
      indent_str ^ "(\n" ^
      pretty_print ~indent:(indent+2) f1 ^ "\n" ^
      indent_str ^ " ↔\n" ^
      pretty_print ~indent:(indent+2) f2 ^ "\n" ^
      indent_str ^ ")"
  | Equal (f1, f2) ->
      indent_str ^ "(\n" ^
      pretty_print ~indent:(indent+2) f1 ^ "\n" ^
      indent_str ^ " =\n" ^
      pretty_print ~indent:(indent+2) f2 ^ "\n" ^
      indent_str ^ ")"
  | Next f -> indent_str ^ "X " ^ pretty_print f
  | Globally f -> indent_str ^ "G " ^ pretty_print f
  | Finally f -> indent_str ^ "F " ^ pretty_print f
  | Until (f1, f2) ->
      indent_str ^ "(\n" ^
      pretty_print ~indent:(indent+2) f1 ^ "\n" ^
      indent_str ^ " U\n" ^
      pretty_print ~indent:(indent+2) f2 ^ "\n" ^
      indent_str ^ ")"
  | WUntil (f1, f2) ->
      indent_str ^ "(\n" ^
      pretty_print ~indent:(indent+2) f1 ^ "\n" ^
      indent_str ^ " W\n" ^
      pretty_print ~indent:(indent+2) f2 ^ "\n" ^
      indent_str ^ ")"
  | Exists (vars, f) ->
      indent_str ^ "∃ " ^ String.concat ~sep:"," (List.map ~f:var_to_string_typed vars) ^ ".\n" ^
      pretty_print ~indent f
  | Forall (vars, f) ->
      indent_str ^ "∀ " ^ String.concat ~sep:"," (List.map ~f:var_to_string_typed vars) ^ ".\n" ^
      pretty_print ~indent f
  | ForallOtherThan (vars, otherthan, f) ->
      indent_str ^ "∀ " ^ String.concat ~sep:"," (List.map ~f:var_to_string_typed vars) ^
      " ∉ {" ^ String.concat ~sep:"," (List.map ~f:var_to_string_typed otherthan) ^ "}.\n" ^
      pretty_print ~indent f

(* Fun-specific utilities *)
let is_oracle name = String.is_prefix ~prefix:"O" name
let is_const_input name = String.is_prefix ~prefix:"I" name
let is_aux name = String.is_prefix ~prefix:"choice" name
let is_b name = String.is_prefix ~prefix:"B" name

(* Encode Fun to Var *)
let encode_fun_to_var = function
  | Fun (name, None, params) ->
      let param_str = 
        params 
        |> List.map ~f:(fun v -> "_" ^ v.name ^ "#" ^ v.typ)
        |> String.concat ~sep:""
      in
      mk_var (name ^ param_str)
  | Fun (name, Some idx, params) ->
      let param_str = 
        params 
        |> List.map ~f:(fun v -> "_" ^ v.name ^ "#" ^ v.typ)
        |> String.concat ~sep:""
      in
      mk_var (name ^ "#" ^ idx ^ param_str)
  | _ -> failwith "encode_fun_to_var: expected Fun formula"

(* Encode function name with index *)
let encode_fun_name = function
  | Fun (name, None, _) -> name
  | Fun (name, Some idx, _) -> name ^ "#" ^ idx
  | _ -> failwith "encode_fun_name: expected Fun formula"

(* Decode Var to Fun based on naming pattern *)
let decode_var_to_fun var_string =
  try
    (* Split on '_' to separate name/index from parameters *)
    match String.split ~on:'_' var_string with
    | [] | [_] -> None  (* Need at least name and one parameter *)
    | name_part :: param_parts ->
        (* Parse name and index from name_part *)
        let name, idx =
          match String.split ~on:'#' name_part with
          | [name] -> (name, None)
          | [name; idx] -> (name, Some idx)
          | _ -> failwith "invalid name format"
        in
        
        (* Parse parameters *)
        let params =
          List.map ~f:(fun p ->
            match String.split ~on:'#' p with
            | [param_name; param_type] -> mk_var ~typ:param_type param_name
            | _ -> failwith "invalid parameter format"
          ) param_parts
        in
        
        Some (Fun (name, idx, params))
  with _ -> None

(* Decode just the function name and index *)
let decode_fun_name var_string =
  try
    (* Split on '_' and take first part *)
    match String.split ~on:'_' var_string with
    | [] -> None
    | name_part :: _ ->
        match String.split ~on:'#' name_part with
        | [name] -> Some (name, None)
        | [name; idx] -> Some (name, Some idx)
        | _ -> None
  with _ -> None

(* Analysis functions *)

(* Compute free variables (variables not bound by quantifiers) *)
let rec free_vars formula =
  match formula with
  | True | False -> []
  | Var v -> [v]
  | Fun (_, _, params) -> params
  | Neg f -> free_vars f
  | And (f1, f2) | Or (f1, f2) | Implies (f1, f2) | Equiv (f1, f2) | Equal (f1, f2) 
  | Until (f1, f2) | WUntil (f1, f2) ->
      free_vars f1 @ free_vars f2
  | Next f | Globally f | Finally f -> free_vars f
  | Exists (vars, f) | Forall (vars, f) ->
      List.filter ~f:(fun v -> not (List.mem vars v ~equal:phys_equal)) (free_vars f)
  | ForallOtherThan (vars, otherthan, f) ->
      let bound = vars @ otherthan in
      List.filter ~f:(fun v -> not (List.mem bound v ~equal:phys_equal)) (free_vars f)

(* Compute bound variables (variables bound by quantifiers) *)
let rec bound_vars formula =
  match formula with
  | True | False | Var _ | Fun _ -> []
  | Neg f -> bound_vars f
  | And (f1, f2) | Or (f1, f2) | Implies (f1, f2) | Equiv (f1, f2) | Equal (f1, f2)
  | Until (f1, f2) | WUntil (f1, f2) ->
      bound_vars f1 @ bound_vars f2
  | Next f | Globally f | Finally f -> bound_vars f
  | Exists (vars, f) | Forall (vars, f) -> vars @ bound_vars f
  | ForallOtherThan (vars, otherthan, f) -> vars @ otherthan @ bound_vars f

(* Compute operator size (number of operators in the formula tree) *)
let rec opsize formula =
  match formula with
  | True | False | Var _ -> 0
  | Fun _ -> 1  (* Function application counts as an operator *)
  | Neg f -> 1 + opsize f
  | And (f1, f2) | Or (f1, f2) | Implies (f1, f2) | Equiv (f1, f2) | Equal (f1, f2)
  | Until (f1, f2) | WUntil (f1, f2) ->
      1 + opsize f1 + opsize f2
  | Next f | Globally f | Finally f -> 1 + opsize f
  | Exists (_, f) | Forall (_, f) | ForallOtherThan (_, _, f) -> 1 + opsize f

(* Check if formula is quantifier-free *)
let rec is_qfree formula =
  match formula with
  | True | False | Var _ -> true
  | Fun _ -> true
  | Neg f -> is_qfree f
  | And (f1, f2) | Or (f1, f2) | Implies (f1, f2) | Equiv (f1, f2) | Equal (f1, f2)
  | Until (f1, f2) | WUntil (f1, f2) ->
      is_qfree f1 && is_qfree f2
  | Next f | Globally f | Finally f -> is_qfree f
  | Exists _ | Forall _ | ForallOtherThan _ -> false

(* Check if formula is universal (only ∀ quantifiers, no ∃) *)
let rec is_universal formula =
  match formula with
  | True | False | Var _ -> true
  | Fun _ -> true
  | Neg f -> is_universal f
  | And (f1, f2) | Or (f1, f2) | Implies (f1, f2) | Equiv (f1, f2) | Equal (f1, f2)
  | Until (f1, f2) | WUntil (f1, f2) ->
      is_universal f1 && is_universal f2
  | Next f | Globally f | Finally f -> is_universal f
  | Exists _ | ForallOtherThan _ -> false
  | Forall (_, f) -> is_universal f

(* Check if formula is in Bernays-Schönfinkel class *)
(* BS class: universal prefix followed by quantifier-free matrix *)
let is_bs formula =
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
  not (has_existential_prefix formula) && is_qfree (strip_universal_prefix formula)