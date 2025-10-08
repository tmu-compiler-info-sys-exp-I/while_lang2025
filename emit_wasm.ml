open Printf

open Virtual_stack

exception Error of string

exception NotImplementedError of string

let indent = ref 2

let inc_indent _ =
    indent := !indent + 2

let dec_indent _ =
    indent := !indent - 2

let make_indent oc ?(extra=0) =
  List.iter (fun _ -> Printf.fprintf oc " ")
    (List.init (!indent + extra) (fun _ -> ()))

let emit_code oc =
    make_indent oc ~extra:2;
    Printf.fprintf oc

type env = { mutable globals: (string, unit) Hashtbl.t }

type cframe = { test: string; out: string }

let collect_globals prog =
  let g = Hashtbl.create 8 in
  List.iter (function
      | RValue x | LPush x -> Hashtbl.replace g x ()
      | _ -> ()
    ) prog;
  g

let emit_instr (oc : out_channel) (ctrl : cframe list ref) (pending_store: string option ref) =
  function
  | Push n      -> emit_code oc "i32.const %d\n" n
  | TRUE        -> emit_code oc "i32.const 1\n"
  | FALSE       -> emit_code oc "i32.const 0\n"
  | NOT         -> emit_code oc "i32.eqz\n"
  | PLUS        -> emit_code oc "i32.add\n"
  | MINUS       -> raise (NotImplementedError("引き算 (i32.sub) を実装してください"))
  | TIMES       -> raise (NotImplementedError("かけ算 (i32.mul) を実装してください"))
  | DIV         -> emit_code oc "i32.div_s\n"
  | EQ          -> raise (NotImplementedError("== (i32.eq) を実装してください"))
  | LT          -> emit_code oc "i32.lt_s\n"
  | LE          -> raise (NotImplementedError("<= (i32.le_s) を実装してください"))
  | GT          -> raise (NotImplementedError(">  (i32.gt_s) を実装してください"))
  | GE          -> raise (NotImplementedError(">= (i32.ge_s) を実装してください"))
  | AND         -> emit_code oc "i32.and\n"
  | OR          -> emit_code oc "i32.and\n"
  | RValue x    -> emit_code oc "global.get $%s\n" x
  | PRINT       -> emit_code oc "call $print\n"
  | LPush x     -> emit_code oc "global.set $%s\n" x
  | LabelTest (t, out) ->
     ctrl := {test=t; out=out} :: !ctrl;
     emit_code oc "(block $%s\n" out;
     inc_indent ();
     emit_code oc "(loop $%s\n" t;
     inc_indent ()
  | GoFalse out ->
     (* 直前までに cond がスタックにある想定 *)
     emit_code oc "i32.eqz\n";
     emit_code oc "br_if $%s\n" out
  | GoTo t -> emit_code oc "br $%s\n" t
  | LabelOut (t, out) ->
     (* スタック整合性を軽く確認（任意） *)
     begin match !ctrl with
     | {test; out=_} :: rest when test = t ->
        ctrl := rest;
        (emit_code oc ") ;; loop\n";
         dec_indent ();
         emit_code oc ") ;; block\n";
         dec_indent ())
     | _ ->
        raise (Error (Printf.sprintf "LabelOut mismatch for %s/%s" t out))
     end

let compile (oc : out_channel) (prog : t list) : unit =
  let g = collect_globals prog in
  let pending_store = ref None in
  let ctrl = ref [] in
  Printf.fprintf oc "(module\n";
  Printf.fprintf oc "  (import \"env\" \"print\" (func $print (param i32)))\n";
  Hashtbl.to_seq_keys g
  |> Seq.iter (fun x ->
         Printf.fprintf oc "  (global $%s (mut i32) (i32.const 0))\n" x);
  Printf.fprintf oc "  (func $main\n";
  List.iter (fun p ->
      emit_instr oc ctrl pending_store p
    ) prog;
  Printf.fprintf oc "  )\n";
  Printf.fprintf oc "\n  (export \"main\" (func $main))\n";
  Printf.fprintf oc ")\n"
