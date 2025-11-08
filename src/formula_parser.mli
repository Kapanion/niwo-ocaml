open! Core

(** Parser interface for FOLTL formulas *)

val parse_formula : string -> Formula.t Or_error.t
val parse_formula_exn : string -> Formula.t
