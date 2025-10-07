@echo off
set OCAMLLIB=.\win64ocaml\lib\ocaml
set arg1=%1
shift

.\win64ocaml\bin\ocamlrun.exe %*
