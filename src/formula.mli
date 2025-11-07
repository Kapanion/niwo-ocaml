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

val equal : t -> t -> bool

(* Constructors *)
val mk_var : ?typ:string -> string -> var
val mk_fun : string -> var list -> t
val mk_fun_indexed : string -> string -> var list -> t
val mk_and : t -> t -> t
val mk_or : t -> t -> t
val mk_implies : t -> t -> t
val mk_equiv : t -> t -> t
val mk_equal : t -> t -> t
val mk_neg : t -> t
val mk_next : t -> t
val mk_globally : t -> t
val mk_finally : t -> t
val mk_until : t -> t -> t
val mk_wuntil : t -> t -> t
val mk_exists : var list -> t -> t
val mk_forall : var list -> t -> t
val mk_forall_other_than : var list -> var list -> t -> t

(* Smart constructors for lists *)
val mk_and_list : t list -> t
val mk_or_list : t list -> t
val mk_implies_list : t list -> t
val mk_equiv_list : t list -> t
val mk_equal_list : t list -> t

(* String conversion *)
val var_to_string : ?annotate:bool -> var -> string
val to_string : ?annotate:bool -> t -> string

(* Fun-specific utilities *)
val is_oracle : string -> bool
val is_const_input : string -> bool
val is_aux : string -> bool
val is_b : string -> bool

(* Analysis functions *)
val free_vars : t -> var list
val bound_vars : t -> var list
val opsize : t -> int
val is_qfree : t -> bool
val is_universal : t -> bool
val is_bs : t -> bool
