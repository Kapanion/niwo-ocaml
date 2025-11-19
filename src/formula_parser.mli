open! Core

(** Parser interface for FOLTL formulas *)

(** Parse a formula from a string, returning an [Or_error.t]. The parser
	accepts the FOLTL concrete syntax used across the project. *)
val parse_formula : string -> Formula.t Or_error.t

(** Same as [parse_formula] but raises on error. Useful for tests and REPL
	usage. *)
val parse_formula_exn : string -> Formula.t
