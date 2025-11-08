open! Core

let usage () =
  let argv = Sys.get_argv () in
  Printf.eprintf "Usage:\n";
  Printf.eprintf
    "  %s --string <formula>       Parse formula from command line\n"
    argv.(0);
  exit 1
;;

let () =
  let argv = Sys.get_argv () in
  if Array.length argv < 2 then usage ();
  match argv.(1) with
  | "--string" ->
    if Array.length argv < 3
    then (
      Printf.eprintf "Error: --string requires a formula argument\n";
      usage ());
    let formula_str = argv.(2) in
    (match Niwo.Formula_parser.parse_formula formula_str with
     | Ok formula ->
       Printf.printf "--- Parsed Formula ---\n";
       Printf.printf "%s\n" (Niwo.Formula.to_string formula);
       Printf.printf "\n--- With Types ---\n";
       Printf.printf "%s\n" (Niwo.Formula.to_string ~annotate:true formula);
       Printf.printf "\n--- Analysis ---\n";
       Printf.printf
         "Free vars: %s\n"
         (String.concat
            ~sep:", "
            (List.map
               ~f:(Niwo.Formula.var_to_string ~annotate:true)
               (Niwo.Formula.free_vars formula)));
       Printf.printf
         "Bound vars: %s\n"
         (String.concat
            ~sep:", "
            (List.map
               ~f:(Niwo.Formula.var_to_string ~annotate:true)
               (Niwo.Formula.bound_vars formula)));
       Printf.printf "Op size: %d\n" (Niwo.Formula.opsize formula);
       Printf.printf "Quantifier-free: %b\n" (Niwo.Formula.is_qfree formula);
       Printf.printf "Universal: %b\n" (Niwo.Formula.is_universal formula);
       Printf.printf "Bernays-Schönfinkel: %b\n" (Niwo.Formula.is_bs formula)
     | Error msg ->
       Printf.eprintf "Parse error: %s\n" (Error.to_string_hum msg);
       exit 1)
  | _ -> usage ()
;;
