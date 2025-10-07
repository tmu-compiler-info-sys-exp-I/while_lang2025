(* https://www.cs.cmu.edu/~aldrich/courses/15-819O-13sp/resources/ *)
(* Whie言語の文法を定義する *)
(* 具体的には、 type 宣言を使ってデータ型を定義する *)

open Printf

type id = string
type num = int

(* arithmetic expressions *)
type a =
  | Var of id                   (* Var 型。引数は文字列 id *)
  | Num of num                  (* Num 型。引数は数値 num *)
  | Add of a * a                (* Add 型。引数はタプル (arith, arith)。*)
                                (* 再帰的な定義も可能 *)
(* 課題1 Sub, Mul, Div を実装する *)

(* boolean predicates *)
type p =
    True                        (* True 型。引数はなし。 *)
  | False
  | Not of p
  | And of p * p
  | Or of p * p                 (* Or 型。 (x | y) に相当。 *)
  | LT of a * a                 (* LT 型。 (x < y) に相当。 *)
  | GT of a * a                 (* GT 型。 (x > y) に相当。 *)
(* 課題2 > (GT), >= (GE), <= (LE), == (EQ) を実装する *)

(* statements *)
type s =
  | Assign of id * a           (* Assign 型。 i := 1; や x := x + y; に相当。 *)
  | Skip
  | Print of a                 (* Print 型。 print 1 に相当 *)
(* 課題3 Block, Seq, While の実装を行う。 ... を適切な引数に置き換える  *)
  (* | Block of ...                 (\* Block 型。 begin i := 1; end に相当。 *\) *)
  (* | Seq of  ...               (\* Seq 型。連続する文 (i := i + 1; j := i + 1;) に相当。*\) *)
  (* | While of ...             (\* While 型。 while i < 3 do ... end; に相当 *\) *)


(* デバッグ用の補助関数。 Syntax を文字列として表示する *)
let rec string_of_arith a =
  match a with
  | Var id -> sprintf "Var(%s)" id
  | Num n -> sprintf "Num(%d)" n
  | Add (a1, a2) -> sprintf "Add (%s, %s)" (string_of_arith a1) (string_of_arith a2)
  (* | Sub (...) -> sprintf "Sub (%s, %s)" (string_of_arith ...) (string_of_arith ...) *)  (* TODO Sub を実装したらコメントアウトし、 ... を適切な引数に変更する  *)
  (* | Mul (...) -> sprintf "Mul (%s, %s)" (string_of_arith ...) (string_of_arith ...) *)  (* TODO Mul を実装したらコメントアウトし、 ... を適切な引数に変更する  *)
  (* | Div (...) -> sprintf "Div (%s, %s)" (string_of_arith ...) (string_of_arith ...) *)  (* TODO Div を実装したらコメントアウトし、 ... を適切な引数に変更する  *)

let rec string_of_predicate p =
  match p with
  | True -> "true"
  | False -> "false"
  | Not p -> sprintf "not %s" (string_of_predicate p)
  | And (p1, p2) -> sprintf "%s and %s" (string_of_predicate p1) (string_of_predicate p2)
  | Or (p1, p2) -> sprintf "%s or %s" (string_of_predicate p1) (string_of_predicate p2)
  | LT (a1, a2) -> sprintf "%s < %s" (string_of_arith a1) (string_of_arith a2)
  (* TODO LE, GT, GE, EQ を実装したらコメントアウトし、 ... を適切な引数に変更する *)
  (* | LE (...) -> sprintf "%s <= %s" (string_of_arith ...) (string_of_arith ...) *)
  (* | GT (...) -> sprintf "%s > %s" (string_of_arith ...) (string_of_arith ...)  *)
  (* | GE (...) -> sprintf "%s >= %s" (string_of_arith ...) (string_of_arith ...) *)
  (* | EQ (...) -> sprintf "%s == %s" (string_of_arith ...) (string_of_arith ...) *)

let rec string_of_statement s =
  match s with
  | Assign (id, a) -> sprintf "Assing (%s, %s);\n" id (string_of_arith a)
  | Skip -> sprintf "Skip;\n"
  (* | Block (s) -> sprintf "Block (%s);\n" (string_of_statement s) *)
  (* | Seq (s1, s2) -> sprintf "%s%s\n" (string_of_statement s1) (string_of_statement s2) *)
  (* | While (p, s) -> sprintf "While (%s, %s);\n" (string_of_predicate p) (string_of_statement s) *)
  | Print (a) -> sprintf "Print (%s);\n" (string_of_arith a)
