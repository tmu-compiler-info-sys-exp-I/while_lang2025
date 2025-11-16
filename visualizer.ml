open Syntax
open Virtual_stack
open Printf

(* ANSI color codes for better visualization *)
let color_reset = "\027[0m"
let color_blue = "\027[34m"
let color_green = "\027[32m"
let color_yellow = "\027[33m"
let color_cyan = "\027[36m"
let color_magenta = "\027[35m"
let color_red = "\027[31m"
let color_bold = "\027[1m"

(* Box drawing characters for tree structure *)
let box_vr = "├──"  (* vertical right *)
let box_vl = "└──"  (* vertical left *)
let box_v = "│   "  (* vertical *)
let box_sp = "    "  (* space *)

(* Helper function to print with indentation *)
let print_indent indent str =
  printf "%s%s\n" indent str

(* AST Visualizer - Pretty print arithmetic expressions *)
let rec visualize_arith ?(prefix="") ?(is_last=true) (a : a) =
  let connector = if is_last then box_vl else box_vr in
  let new_prefix = prefix ^ (if is_last then box_sp else box_v) in

  match a with
  | Var id ->
      printf "%s%s%sVar%s: %s%s%s\n"
        prefix connector color_cyan color_reset color_yellow id color_reset
  | Num n ->
      printf "%s%s%sNum%s: %s%d%s\n"
        prefix connector color_cyan color_reset color_yellow n color_reset
  | Add (a1, a2) ->
      printf "%s%s%sAdd%s\n" prefix connector color_green color_reset;
      visualize_arith ~prefix:new_prefix ~is_last:false a1;
      visualize_arith ~prefix:new_prefix ~is_last:true a2

(* AST Visualizer - Pretty print predicates *)
let rec visualize_predicate ?(prefix="") ?(is_last=true) (p : p) =
  let connector = if is_last then box_vl else box_vr in
  let new_prefix = prefix ^ (if is_last then box_sp else box_v) in

  match p with
  | True ->
      printf "%s%s%sTrue%s\n" prefix connector color_green color_reset
  | False ->
      printf "%s%s%sFalse%s\n" prefix connector color_red color_reset
  | Not p ->
      printf "%s%s%sNot%s\n" prefix connector color_magenta color_reset;
      visualize_predicate ~prefix:new_prefix ~is_last:true p
  | And (p1, p2) ->
      printf "%s%s%sAnd%s\n" prefix connector color_magenta color_reset;
      visualize_predicate ~prefix:new_prefix ~is_last:false p1;
      visualize_predicate ~prefix:new_prefix ~is_last:true p2
  | Or (p1, p2) ->
      printf "%s%s%sOr%s\n" prefix connector color_magenta color_reset;
      visualize_predicate ~prefix:new_prefix ~is_last:false p1;
      visualize_predicate ~prefix:new_prefix ~is_last:true p2
  | LT (a1, a2) ->
      printf "%s%s%sLT (<)%s\n" prefix connector color_blue color_reset;
      visualize_arith ~prefix:new_prefix ~is_last:false a1;
      visualize_arith ~prefix:new_prefix ~is_last:true a2
  | GT (a1, a2) ->
      printf "%s%s%sGT (>)%s\n" prefix connector color_blue color_reset;
      visualize_arith ~prefix:new_prefix ~is_last:false a1;
      visualize_arith ~prefix:new_prefix ~is_last:true a2

(* AST Visualizer - Pretty print statements *)
let rec visualize_statement ?(prefix="") ?(is_last=true) (s : s) =
  let connector = if is_last then box_vl else box_vr in
  let new_prefix = prefix ^ (if is_last then box_sp else box_v) in

  match s with
  | Assign (id, a) ->
      printf "%s%s%sAssign%s: %s%s%s\n"
        prefix connector color_bold color_reset color_cyan id color_reset;
      visualize_arith ~prefix:new_prefix ~is_last:true a
  | Skip ->
      printf "%s%s%sSkip%s\n" prefix connector color_bold color_reset
  | Print a ->
      printf "%s%s%sPrint%s\n" prefix connector color_bold color_reset;
      visualize_arith ~prefix:new_prefix ~is_last:true a

(* Virtual Stack Visualizer - Pretty print stack instructions with line numbers *)
let visualize_stack_instruction idx instr =
  let line_num = sprintf "%s%3d%s │ " color_cyan idx color_reset in
  match instr with
  | LPush id ->
      printf "%s%slpush%s    %s%s%s\n" line_num color_green color_reset color_yellow id color_reset
  | RValue id ->
      printf "%s%srvalue%s   %s%s%s\n" line_num color_green color_reset color_yellow id color_reset
  | Push n ->
      printf "%s%spush%s     %s%d%s\n" line_num color_green color_reset color_yellow n color_reset
  | PLUS ->
      printf "%s%s+%s\n" line_num color_magenta color_reset
  | MINUS ->
      printf "%s%s-%s\n" line_num color_magenta color_reset
  | TIMES ->
      printf "%s%s*%s\n" line_num color_magenta color_reset
  | DIV ->
      printf "%s%s/%s\n" line_num color_magenta color_reset
  | LT ->
      printf "%s%s<%s\n" line_num color_blue color_reset
  | LE ->
      printf "%s%s<=%s\n" line_num color_blue color_reset
  | GT ->
      printf "%s%s>%s\n" line_num color_blue color_reset
  | GE ->
      printf "%s%s>=%s\n" line_num color_blue color_reset
  | EQ ->
      printf "%s%s==%s\n" line_num color_blue color_reset
  | NOT ->
      printf "%s%snot%s\n" line_num color_magenta color_reset
  | AND ->
      printf "%s%sand%s\n" line_num color_magenta color_reset
  | OR ->
      printf "%s%sor%s\n" line_num color_magenta color_reset
  | TRUE ->
      printf "%s%strue%s\n" line_num color_green color_reset
  | FALSE ->
      printf "%s%sfalse%s\n" line_num color_red color_reset
  | LabelTest (l, _) ->
      printf "%s%slabel%s    %s%s%s\n" line_num color_cyan color_reset color_yellow l color_reset
  | LabelOut (_, l) ->
      printf "%s%slabel%s    %s%s%s\n" line_num color_cyan color_reset color_yellow l color_reset
  | GoFalse l ->
      printf "%s%sgofalse%s  %s%s%s\n" line_num color_cyan color_reset color_yellow l color_reset
  | GoTo l ->
      printf "%s%sgoto%s     %s%s%s\n" line_num color_cyan color_reset color_yellow l color_reset
  | PRINT ->
      printf "%s%sprint%s\n" line_num color_bold color_reset

let visualize_stack_code code =
  printf "\n%s╔════════════════════════════════════════╗%s\n" color_bold color_reset;
  printf "%s║   Virtual Stack Machine Instructions   ║%s\n" color_bold color_reset;
  printf "%s╚════════════════════════════════════════╝%s\n" color_bold color_reset;
  printf "%s────┬────────────────────────────────────%s\n" color_cyan color_reset;
  printf "%s Ln │ Instruction%s\n" color_cyan color_reset;
  printf "%s────┼────────────────────────────────────%s\n" color_cyan color_reset;
  List.iteri visualize_stack_instruction code;
  printf "%s────┴────────────────────────────────────%s\n\n" color_cyan color_reset

(* Compilation Pipeline Visualizer *)
let visualize_compilation_arith arith =
  printf "\n%s╔════════════════════════════════════════╗%s\n" color_bold color_reset;
  printf "%s║      Arithmetic Expression AST         ║%s\n" color_bold color_reset;
  printf "%s╚════════════════════════════════════════╝%s\n" color_bold color_reset;
  visualize_arith arith;

  let code = compile_arith arith in
  printf "\n%s           ↓ Compilation ↓%s\n" color_cyan color_reset;
  visualize_stack_code code

let visualize_compilation_predicate pred =
  printf "\n%s╔════════════════════════════════════════╗%s\n" color_bold color_reset;
  printf "%s║          Predicate AST                 ║%s\n" color_bold color_reset;
  printf "%s╚════════════════════════════════════════╝%s\n" color_bold color_reset;
  visualize_predicate pred;

  let code = compile_predicate pred in
  printf "\n%s           ↓ Compilation ↓%s\n" color_cyan color_reset;
  visualize_stack_code code

let visualize_compilation_statement stmt =
  printf "\n%s╔════════════════════════════════════════╗%s\n" color_bold color_reset;
  printf "%s║           Statement AST                ║%s\n" color_bold color_reset;
  printf "%s╚════════════════════════════════════════╝%s\n" color_bold color_reset;
  visualize_statement stmt;

  let code = compile_stack stmt in
  printf "\n%s           ↓ Compilation ↓%s\n" color_cyan color_reset;
  visualize_stack_code code

(* Full pipeline visualization from source string *)
let visualize_full_pipeline source_str =
  printf "\n%s╔════════════════════════════════════════════════════════════╗%s\n"
    color_bold color_reset;
  printf "%s║              COMPILATION PIPELINE VISUALIZATION            ║%s\n"
    color_bold color_reset;
  printf "%s╚════════════════════════════════════════════════════════════╝%s\n"
    color_bold color_reset;

  printf "\n%s[1] Source Code:%s\n" color_bold color_reset;
  printf "%s────────────────────────────────────────%s\n" color_cyan color_reset;
  printf "%s%s%s\n" color_yellow source_str color_reset;

  try
    let lexbuf = Lexing.from_string source_str in
    let ast = Parser.start Lexer.token lexbuf in

    printf "\n%s[2] Abstract Syntax Tree (AST):%s\n" color_bold color_reset;
    printf "%s────────────────────────────────────────%s\n" color_cyan color_reset;
    visualize_statement ast;

    printf "\n%s[3] Virtual Stack Machine Code:%s\n" color_bold color_reset;
    printf "%s────────────────────────────────────────%s\n" color_cyan color_reset;
    let code = compile_stack ast in
    visualize_stack_code code;

  with e ->
    printf "\n%s[ERROR]%s %s\n" color_red color_reset (Printexc.to_string e)
