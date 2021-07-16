#!/bin/sh
cd src

ocamlopt -c tokens.ml nodes.ml options.ml
ocamlc -c memory.mli lexer.mli parser.mli interpreter.mli utility.mli
ocamlopt -o brainfuck unix.cmxa memory.ml utility.ml lexer.ml parser.ml interpreter.ml brainfuck.ml

rm *.cmi *.cmx *.o

mv brainfuck ../

cd ..
