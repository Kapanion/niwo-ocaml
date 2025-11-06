open! Core

(* Re-export formula modules *)
module Formula = Formula
module Formula_parser = Formula_parser

val count_lines : string -> int
val process_file : string -> unit
