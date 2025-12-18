@echo off
set OCAMLLIB=.\win64ocaml\lib\ocaml

set ocamlyacc=.\win64ocaml\bin\ocamlyacc.exe
set ocamllex=.\win64ocaml\bin\ocamllex.exe
set ocamlc=.\win64ocaml\bin\ocamlc.exe

%ocamlc% -c .\syntax.ml
%ocamlc% -c .\error.ml
%ocamlyacc% .\parser.mly
%ocamllex% .\lexer.mll
%ocamlc% -c .\*.mli
%ocamlc% .\syntax.ml .\error.ml .\parser.ml .\lexer.ml .\virtual_stack.ml .\visualizer.ml .\test_day2.ml -o .\test_day2
.\bin\run.bat .\test_day2
