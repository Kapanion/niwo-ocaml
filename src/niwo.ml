open! Core

(* Re-export modules for convenience *)
module Formula = Formula
module Formula_parser = Formula_parser

let count_lines filename =
  let ic = In_channel.create filename in
  let rec aux acc =
    try
      let _ = In_channel.input_line ic in
      aux (acc + 1)
    with
    | End_of_file -> acc
  in
  let result = aux 0 in
  In_channel.close ic;
  result
;;

let process_file filename =
  try
    let line_count = count_lines filename in
    Printf.printf "The file '%s' has %d lines.\n" filename line_count
  with
  | Sys_error msg ->
    Printf.eprintf "Error: %s\n" msg;
    exit 1
;;
