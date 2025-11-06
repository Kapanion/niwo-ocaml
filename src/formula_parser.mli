open! Core

(* Parser interface for FOLTL formulas *)

(* Parse a formula from a string *)
val parse_formula : string -> (Formula.t, string) result

(* Parse a formula from a string, raising an exception on error *)
val parse_formula_exn : string -> Formula.t

(* Individual parser components (exposed for testing and composition) *)
val ident : string Angstrom.t
val var : Formula.var Angstrom.t
val typed_var : Formula.var Angstrom.t
val simple_term : Formula.t Angstrom.t
