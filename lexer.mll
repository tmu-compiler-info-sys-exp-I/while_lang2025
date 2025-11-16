{
    (* Lexer が仕様する変数、関数などの定義 *)
    open Parser
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let letter = lower | upper

(*  文字列からparser.mly で宣言したトークンにでマッピングする *)
rule token = parse
| space+ { token lexbuf }
| '+'  { PLUS }
(* | '-'  { ... } *)
| '*'  { TIMES }
| '/'  { DIVIDE }
| digit+  { NUMBER (int_of_string (Lexing.lexeme lexbuf)) }
(* | "<"  { ... } *)
(* | "<=" { ... } *)
(* | ">"  { ... } *)
(* | ">=" { ... } *)
(* | "==" { ... } *)
| ":=" { ASSIGN }
| ';'  { SEMICOLON }
| "begin" { BEGIN }
| "end"   { END }
| "while" { WHILE }
| "do"    { DO }
| "true"  { TRUE }
| "not"   { NOT }
| "false" { FALSE }
| "and"   { AND }
| "or"    { OR }
| "skip"  { SKIP }
| "print" { PRINT }
| eof     { EOF }
| letter+ { VARIANT (Lexing.lexeme lexbuf) }
| _       {
    let pos = lexbuf.Lexing.lex_curr_p in
    failwith (Printf.sprintf "Lexical error: unknown token '%s' at line %d, column %d.\nValid tokens include: keywords (while, do, if, then, else, skip, print, begin, end, true, false, not, and, or), operators (+, *, /, :=, ;), numbers, and identifiers."
      (Lexing.lexeme lexbuf)
      pos.Lexing.pos_lnum
      (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1))
  }
