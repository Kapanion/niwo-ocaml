let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Usage: %s <transition_system_file>\n" Sys.argv.(0);
    exit 1
  end;
  let filename = Sys.argv.(1) in
  Niwo.process_file filename