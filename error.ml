(* ANSI color codes for better visualization *)
let color_reset = "\027[0m"
let color_cyan = "\027[36m"
let color_red = "\027[31m"
let color_bold = "\027[1m"

(* Global reference to store the source code for error reporting *)
let current_source = ref ""

(* Set the current source being parsed *)
let set_source source =
  current_source := source

(* Visualize parse error with source code context *)
let visualize_parse_error start_pos end_pos =
  Printf.eprintf "\n%s+============================================================+%s\n"
    color_bold color_reset;
  Printf.eprintf "%s|              PARSER ERROR VISUALIZATION                    |%s\n"
    color_bold color_reset;
  Printf.eprintf "%s+============================================================+%s\n"
    color_bold color_reset;

  Printf.eprintf "\n%s[Source Code with Error Position]%s\n" color_bold color_reset;
  Printf.eprintf "%s============================================================%s\n"
    color_cyan color_reset;

  (* Get the full source from our stored reference *)
  let full_source = !current_source in

  (* Split source into lines for display *)
  let lines = String.split_on_char '\n' full_source in
  let char_pos = ref 0 in

  List.iteri (fun line_idx line ->
    let line_start = !char_pos in
    let line_end = line_start + String.length line in

    (* Print line number and content *)
    Printf.eprintf "%s%3d |%s %s\n" color_cyan (line_idx + 1) color_reset line;

    (* Check if error is on this line *)
    if start_pos >= line_start && start_pos <= line_end then begin
      let col_start = start_pos - line_start in
      let col_end = min (end_pos - line_start) (String.length line) in
      let marker_len = max 1 (col_end - col_start) in

      (* Print error indicator *)
      Printf.eprintf "%s    |%s " color_cyan color_reset;
      for _ = 0 to col_start - 1 do Printf.eprintf " " done;
      Printf.eprintf "%s" color_red;
      for _ = 1 to marker_len do Printf.eprintf "^" done;
      Printf.eprintf "%s\n" color_reset;
      Printf.eprintf "%s    |%s " color_cyan color_reset;
      for _ = 0 to col_start - 1 do Printf.eprintf " " done;
      Printf.eprintf "%s+- Error here (characters %d-%d)%s\n"
        color_red start_pos end_pos color_reset;
    end;

    char_pos := line_end + 1 (* +1 for newline *)
  ) lines;

  Printf.eprintf "%s============================================================%s\n"
    color_cyan color_reset;

  Printf.eprintf "\n%s[Error Message]%s\n" color_bold color_reset;
  Printf.eprintf "%sSyntax error: unexpected input at characters %d-%d.%s\n"
    color_red start_pos end_pos color_reset;
  Printf.eprintf "%sPlease check your While language syntax.%s\n"
    color_red color_reset;
  Printf.eprintf "%s(statements should end with ';', use keywords: while, do, if, then, else, skip, print)%s\n\n"
    color_red color_reset
