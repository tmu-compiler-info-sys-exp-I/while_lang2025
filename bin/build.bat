@echo off
set OCAMLLIB=.\win64ocaml\lib\ocaml

set ocamlyacc=.\win64ocaml\bin\ocamlyacc.exe
set ocamllex=.\win64ocaml\bin\ocamllex.exe
set ocamlc=.\win64ocaml\bin\ocamlc.exe

%ocamlc% -c .\syntax.ml
%ocamlyacc% .\parser.mly
%ocamllex% .\lexer.mll
%ocamlc% .\syntax.ml .\parser.mli .\parser.ml .\lexer.ml .\virtual_stack.ml .\emit_wasm.ml .\main.ml -o .\while_lang
