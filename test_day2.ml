open Syntax

(* 課題1,2,3 *)
(* 入力文字列が構文木に変換できるか確かめる*)
let test_syntax s msg =
  print_string "[input] ";
  print_endline s;
  try
    let l = Lexing.from_string s in
    let output = Parser.start Lexer.token l
                 |> Syntax.string_of_statement in
    print_string "[output] ";
    output |> print_string; print_newline ()
  with e ->
      print_string "[error] ";
      print_endline msg;
      print_newline ();
      raise e


(* 課題1,2,3 *)
(* 入力文字列 (predicate) が構文木に変換できるか確かめる*)
let test_syntax_predicate p msg =
  print_string "[input] ";
  print_endline p;
  try
    let l = Lexing.from_string p in
    let output = Parser.predicate Lexer.token l
                 |> Syntax.string_of_predicate in
    print_string "[output] ";
    output |> print_string; print_newline()
  with e ->
      print_string "[error] ";
      print_endline msg;
      print_newline ();
      raise e

(* 課題4 *)
(* 入力文字列が仮想スタック機械命令列に変換できるか確かめる*)
let test_stack_ops s msg =
  print_string "[input] ";
  print_endline s;
  try
    let l = Lexing.from_string s in
    let output = Parser.start Lexer.token l
                 |> Virtual_stack.compile_stack in
    print_string "[output] ";
    Virtual_stack.print_code stdout output;
    print_newline ()
  with e ->
      print_string "[error] ";
      print_endline msg;
      print_newline ();
      raise e

let () =
    print_endline "[TEST] syntax";
    test_syntax "i := i + 1;"
      "足し算 (+) を sytax.ml/lexer.mll/parser.mly に実装してください";
    test_syntax "i := i - 1;"
      "引き算 (-) を sytax.ml/lexer.mll/parser.mly に実装してください";
    test_syntax "i := i * 1;"
      "掛け算 (*) を sytax.ml/lexer.mll/parser.mly に実装してください";
    test_syntax "i := i / 1;"
      "割り算 (*) を sytax.ml/lexer.mll/parser.mly に実装してください";
    test_syntax_predicate "i < 10"
      "比較 (<) を sytax.ml/lexer.mll/parser.mly に実装してください";
    test_syntax_predicate "i > 10"
      "比較 (>) を sytax.ml/lexer.mll/parser.mly に実装してください";
    print_endline "[TEST] virtual stack ops";
    test_stack_ops "begin i := i + 1; end;"
      "begin - end を実装してください";
    test_stack_ops "begin i := i + 1; j := j + 2; end;"
      "begin - end と 連続する文を処理する Seq を実装してください";
    test_stack_ops "while i < 10 do i := i + 1;"
      "while を実装してください";
    test_stack_ops "while i < 10 do begin i := i + 1; end;"
      "while と begin - end を実装してください";
    ()
