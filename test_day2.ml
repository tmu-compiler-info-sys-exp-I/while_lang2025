open Syntax

(* 課題1,2,3 *)
(* 入力文字列が構文木に変換できるか確かめる*)
let test_syntax s =
  print_string "[TEST syntax]\n";
  print_string "input: ";
  print_endline s;
  try
    print_string "output: ";
    let l = Lexing.from_string s in
    Parser.start Lexer.token l
    |> Syntax.string_of_statement
    |> print_string
  with e ->
      print_string "error";
      print_newline ()


(* 課題1,2,3 *)
(* 入力文字列 (predicate) が構文木に変換できるか確かめる*)
let test_syntax_predicate p =
  print_string "[TEST predicate]\n";
  print_string "input: ";
  print_endline p;
  try
    print_string "output: ";
    let l = Lexing.from_string p in
    Parser.predicate Lexer.token l
    |> Syntax.string_of_predicate
    |> print_string; print_newline()
  with e ->
      print_string "error";
      print_newline ()

(* 課題4 *)
(* 入力文字列が仮想スタック機械命令列に変換できるか確かめる*)
let test_stack_ops s =
  print_string "[TEST stack ops]\n";
  print_string "input: ";
  print_endline s;
  print_string "output: ";
  try
    let l = Lexing.from_string s in
    Parser.start Lexer.token l
    |> Virtual_stack.compile_stack
    |> Virtual_stack.print_code stdout
  with e ->
      print_string "error";
      print_newline ()


let () =
    test_syntax "i := i + 1;";
    test_syntax "i := i - 1;";
    test_syntax "i := i * 1;";
    test_syntax_predicate "i < 10";
    test_syntax_predicate "i > 10";
    test_stack_ops "i := i + 1;";
    test_stack_ops "i := i - 1;";
    test_stack_ops "i := i * 1;";
    test_stack_ops "i := i / 1;";
    ()
