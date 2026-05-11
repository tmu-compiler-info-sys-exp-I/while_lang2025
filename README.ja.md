# while_lang2025

WHILE 言語から WebAssembly Text Format (.wat) へのコンパイラです。東京都立大学のコンパイラ構成論の教材として開発されています。

## 概要

単純な手続き型言語 **WHILE** のソースコードを **WebAssembly Text Format (WAT)** に変換するコンパイラです。コンパイルパイプラインは、字句解析 (ocamllex)、構文解析 (ocamlyacc)、仮想スタックマシンへの中間表現変換、WebAssembly コード生成の 4 段階で構成されます。

## 必要環境

- **OCaml 4.11+** — コンパイラのビルドに使用
  - macOS: `brew install ocaml`
  - Ubuntu: `sudo apt install ocaml ocaml-findlib`
- **(任意) WebAssembly ランタイム** — 出力された `.wat` ファイルを実行する場合
  - [wasmtime](https://wasmtime.dev/)、[wasmer](https://wasmer.io/)、またはブラウザ

## ビルド

```bash
# コンパイラのビルド
make

# day2 課題テストのビルドと実行
make day2

# テスト (.while ファイルのコンパイル)
make test
```

成功すると `while_lang` バイナリが生成されます。

### Windows

`win64ocaml/` サブモジュールに OCaml 4.11.1 の Windows 用バイナリが同梱されています。

```batch
bin\setup.bat    # サブモジュールの取得
bin\build.bat    # コンパイラのビルド
bin\run.bat      # 実行
```

## 使い方

```bash
# .while ファイルをコンパイル
./while_lang test/assign.while

# 出力: test/assign.wat

# 引数なしで実行すると使用方法が表示されます
./while_lang
# [usage] ./while_lang filename.while
```

## WHILE 言語の文法

### 算術式
```
<arith> ::= <数値> | <変数> | <arith> + <arith>
```

- `Var` — 変数参照 (`i`, `j`, `x`)
- `Num` — 整数リテラル (`0`, `42`, `-1`)
- `Add` — 加算

### 条件式 (述語)
```
<pred> ::= true | false
         | not <pred>
         | <pred> and <pred> | <pred> or <pred>
         | <arith> < <arith>
         | GT, GE, LE, EQ は課題
```

### 文
```
<stmt> ::= skip
         | <id> := <arith>
         | print <arith>
         | <stmt> ; <stmt>
         | begin <stmt> end
         | while <pred> do <stmt>
```

### サンプルプログラム

**test/assign.while** — 代入と出力:
```
i := 1;
j := 2;
print i + j;
```

**test/loop.while** — 入れ子の while ループ:
```
i := 0;

while i < 10 do
  begin
    i := i + 1;
    j := 0;
    while j < 10 do
    begin
      j := j + 1;
      print j;
    end;
  end;

print i;
print j;
```

## プロジェクト構成

```
while_lang2025/
├── main.ml               # エントリポイント (コンパイル全体を統括)
├── syntax.ml             # AST データ型定義 (算術式、述語、文)
├── lexer.mll             # ocamllex 字句解析仕様
├── parser.mly            # ocamlyacc 構文解析仕様
├── virtual_stack.ml      # 仮想スタックマシンへの中間表現変換
├── emit_wasm.ml          # 仮想スタック命令 → WebAssembly テキスト生成
├── error.ml              # パースエラーの可視化
├── visualizer.ml         # AST / 仮想スタックコードの整形表示
├── test_day2.ml          # 課題用テストハーネス
├── Makefile              # ビルド設定
├── OCamlMakefile         # OCaml 用 Makefile フレームワーク
├── bin/                  # Windows 用バッチスクリプト
├── test/                 # サンプル WHILE プログラム
└── win64ocaml/           # Windows 用 OCaml 配布 (サブモジュール)
```

## コンパイラのパイプライン

```
ソースコード (.while)
  │
  ▼ lexer.mll (ocamllex)
トークン列
  │
  ▼ parser.mly (ocamlyacc)
AST (syntax.ml)
  │
  ▼ virtual_stack.ml
仮想スタック命令列
  │
  ▼ emit_wasm.ml
WebAssembly Text Format (.wat)
```

### 仮想スタック命令

| 命令 | 説明 |
|------|------|
| `Push n` | 整数定数 n をプッシュ |
| `RValue id` | 変数 id の値をプッシュ |
| `LPush id` | スタックトップの値を変数 id に代入 |
| `PLUS`, `MINUS`, `TIMES`, `DIV` | 二項算術演算 |
| `EQ`, `LT`, `LE`, `GT`, `GE` | 比較演算 |
| `NOT`, `AND`, `OR` | 論理演算 |
| `LabelTest`, `LabelOut`, `GoTo`, `GoFalse` | 制御フロー (while 用) |
| `PRINT` | 出力 |

### WASM コード生成

生成される `.wat` ファイルは、WHILE プログラムの変数をミュータブルなグローバル変数として定義し、コンパイル結果を `$main` 関数としてエクスポートします。`print` 関数はホスト環境からインポートされます。

## 課題

このプロジェクトには学生が実装する TODO 項目が含まれています。

1. **課題1** (`syntax.ml`, `lexer.mll`, `parser.mly`, `virtual_stack.ml`, `emit_wasm.ml`): `Sub`, `Mul`, `Div` 演算子の追加
2. **課題2** (`syntax.ml` 他): `GT`, `GE`, `LE`, `EQ` 比較演算子の追加
3. **課題3** (`syntax.ml` 他): `Block`, `Seq`, `While` 構文の追加

各課題では、AST の型定義、字句解析器・構文解析器へのトークン/文法規則の追加、仮想スタック命令の生成、WebAssembly コード生成までを一貫して実装します。

## クリーン

```bash
make clean        # ビルド成果物の削除
make clean_test   # テスト生成物 (.pyc, .res) の削除
```

## 参考

- [WHILE Language Resources (CMU)](https://www.cs.cmu.edu/~aldrich/courses/15-819O-13sp/resources/)
- [WebAssembly Specification](https://webassembly.github.io/spec/)
- [OCamlMakefile](https://github.com/mmottl/ocaml-makefile)
