open! Core

let usage () =
  let argv = Sys.get_argv () in
  Printf.eprintf "Usage:\n";
  Printf.eprintf "  %s <formula_file>           Parse formula from file\n" argv.(0);
  Printf.eprintf "  %s --string <formula>       Parse formula from command line\n" argv.(0);
  Printf.eprintf "  %s --count <file>           Count lines in file (legacy)\n" argv.(0);
  exit 1

let () =
  let argv = Sys.get_argv () in
  if Array.length argv < 2 then usage ();

  match argv.(1) with
  | "--string" ->
      if Array.length argv < 3 then begin
        Printf.eprintf "Error: --string requires a formula argument\n";
        usage ()
      end;
      let formula_str = argv.(2) in
      (match Niwo.Formula_parser.parse_formula formula_str with
       | Ok formula ->
           Printf.printf "--- Parsed Formula ---\n";
           Printf.printf "%s\n" (Niwo.Foltl.to_string formula);
           Printf.printf "\n--- With Types ---\n";
           Printf.printf "%s\n" (Niwo.Foltl.to_typed_string formula);
           Printf.printf "\n--- Pretty Print ---\n";
           Printf.printf "%s\n" (Niwo.Foltl.pretty_print formula);
           Printf.printf "\n--- Analysis ---\n";
           Printf.printf "Free vars: %s\n"
             (String.concat ~sep:", " (List.map ~f:Niwo.Foltl.var_to_string_typed (Niwo.Foltl.free_vars formula)));
           Printf.printf "Bound vars: %s\n"
             (String.concat ~sep:", " (List.map ~f:Niwo.Foltl.var_to_string_typed (Niwo.Foltl.bound_vars formula)));
           Printf.printf "Op size: %d\n" (Niwo.Foltl.opsize formula);
           Printf.printf "Quantifier-free: %b\n" (Niwo.Foltl.is_qfree formula);
           Printf.printf "Universal: %b\n" (Niwo.Foltl.is_universal formula);
           Printf.printf "Bernays-Schönfinkel: %b\n" (Niwo.Foltl.is_bs formula)
       | Error msg ->
           Printf.eprintf "Parse error: %s\n" msg;
           exit 1)

  | "--count" ->
      if Array.length argv < 3 then begin
        Printf.eprintf "Error: --count requires a filename\n";
        usage ()
      end;
      let filename = argv.(2) in
      Niwo.process_file filename

  | filename ->
      (* Default: treat argument as a formula file *)
      (* For now, just count lines - we'll implement formula file processing later *)
      Niwo.process_file filename