let%expect_test "count_lines" =
  let temp_file = Filename.temp_file "test" ".txt" in
  let oc = open_out temp_file in
  output_string oc "line1\nline2\nline3\n";
  close_out oc;
  let count = Niwo.count_lines temp_file in
  Printf.printf "%d" count;
  [%expect {| 3 |}];
  Sys.remove temp_file