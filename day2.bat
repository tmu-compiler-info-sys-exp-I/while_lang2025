@echo off
set OCAMLLIB=.\win64ocaml\lib\ocaml

set ocamlyacc=.\win64ocaml\bin\ocamlyacc.exe
set ocamllex=.\win64ocaml\bin\ocamllex.exe
set ocamlc=.\win64ocaml\bin\ocamlc.exe

%ocamlc% -c .\syntax.ml
%ocamlyacc% .\parser.mly
%ocamllex% .\lexer.mll
%ocamlc% -c .\*.mli
%ocamlc% .\syntax.ml .\parser.ml .\lexer.ml .\virtual_stack.ml .\test_day2.ml -o .\test_day2
.\test_day2
