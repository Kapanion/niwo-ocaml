(* Parser interface for FOLTL formulas *)

(* Parse a formula from a string *)
val parse_formula : string -> (Foltl.formula, string) result

(* Parse a formula from a string, raising an exception on error *)
val parse_formula_exn : string -> Foltl.formula

(* Individual parser components (exposed for testing and composition) *)
val ident : string Angstrom.t
val var : Foltl.var Angstrom.t
val typed_var : Foltl.var Angstrom.t
val simple_term : Foltl.formula Angstrom.t