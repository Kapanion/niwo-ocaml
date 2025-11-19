open! Core

(**
    Public interface for the Formula module. This module exposes the AST for
    first-order linear temporal logic formulas (FOLTL) and a number of helper
    constructors and analysis functions used by other parts of the codebase.
*)

(** A variable with a name and a type annotation.*)
type var =
  { name : string
  ; typ : string
  }
[@@deriving sexp_of]

(** The formula AST. *)
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

(** Structural equality for formulas. *)
val equal : t -> t -> bool

(** {1 Constructors} *)

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

(** {2 Smart constructors for lists} *)

val mk_and_list : t list -> t
val mk_or_list : t list -> t
val mk_implies_list : t list -> t
val mk_equiv_list : t list -> t
val mk_equal_list : t list -> t

(** {1 String conversion helpers} *)

val var_to_string : ?annotate:bool -> var -> string
val to_string : ?annotate:bool -> t -> string

(** {1 Function-name predicates used across the system} *)

val is_oracle : string -> bool
val is_const_input : string -> bool
val is_aux : string -> bool
val is_b : string -> bool

(** {1 Analysis functions} *)

(** Compute free variables (variables not bound by quantifiers) *)
val free_vars : t -> var list

(** Compute bound variables (variables bound by quantifiers) *)
val bound_vars : t -> var list

(** Compute operator size (number of operators in the formula tree) *)
val opsize : t -> int

(** Check if formula is quantifier-free *)
val is_qfree : t -> bool

(** Check if formula is universal (only ∀ quantifiers, no ∃) *)
val is_universal : t -> bool

(** Check if formula is in Bernays-Schönfinkel class *)
val is_bs : t -> bool
(** BS class: universal prefix followed by quantifier-free matrix *)
