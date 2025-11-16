# もし実装を改良したら SOURCES にファイルを足す
SOURCES = syntax.ml parser.mly lexer.mll virtual_stack.ml \
	emit_wasm.ml main.ml

SOURCES_DAY2 = syntax.ml parser.mli parser.ml lexer.ml virtual_stack.ml visualizer.ml test_day2.ml


# テストを追加したらテストの名前 (拡張子をは取る) を TESTS に足す
TESTS = assign loop
# 最終的に生成されるバイナリ名
RESULT = while_lang

OCAMLFLAGS = -w -a

all: bc # test

day2:
	ocamllex lexer.mll
	ocamlyacc parser.mly
	ocamlc $(OCAMLFLAGS) $(SOURCES_DAY2) -o test_day2
	./test_day2

test: bc $(TESTS:%=test/%.res)

clean::
	$(RM) *.cmt *.cmti **/*.pyc **/*.res

clean_test:
	$(RM) **/*.pyc **/*.res

test/%.pyc: test/%.while
	./$(RESULT) test/$*.while

-include OCamlMakefile
