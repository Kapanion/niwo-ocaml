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
(* Constructors *)
val mk_var : ?typ:string -> string -> var
val mk_fun : string -> var list -> formula
val mk_fun_indexed : string -> string -> var list -> formula
val mk_and : formula -> formula -> formula
val mk_or : formula -> formula -> formula
val mk_implies : formula -> formula -> formula
val mk_equiv : formula -> formula -> formula
val mk_equal : formula -> formula -> formula
val mk_neg : formula -> formula
val mk_next : formula -> formula
val mk_globally : formula -> formula
val mk_finally : formula -> formula
val mk_until : formula -> formula -> formula
val mk_wuntil : formula -> formula -> formula
val mk_exists : var list -> formula -> formula
val mk_forall : var list -> formula -> formula
val mk_forall_other_than : var list -> var list -> formula -> formula

(* Smart constructors for lists *)
val mk_and_list : formula list -> formula
val mk_or_list : formula list -> formula
val mk_implies_list : formula list -> formula
val mk_equiv_list : formula list -> formula
val mk_equal_list : formula list -> formula

(* String conversion *)
val var_to_string : var -> string
val var_to_string_typed : var -> string
val to_string : formula -> string
val to_typed_string : formula -> string
val pretty_print : ?indent:int -> formula -> string

(* Fun-specific utilities *)
val is_oracle : string -> bool
val is_const_input : string -> bool
val is_aux : string -> bool
val is_b : string -> bool
val encode_fun_to_var : formula -> var
val encode_fun_name : formula -> string
val decode_var_to_fun : string -> formula option
val decode_fun_name : string -> (string * string option) option

(* Analysis functions *)
val free_vars : formula -> var list
val bound_vars : formula -> var list
val opsize : formula -> int
val is_qfree : formula -> bool
val is_universal : formula -> bool
val is_bs : formula -> bool