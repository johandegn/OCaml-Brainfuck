#!/bin/bash
cd src

ocamlopt -c tokens.ml nodes.ml options.ml
ocamlc -c memory.mli lexer.mli parser.mli interpreter.mli
ocamlopt -o brainfuck memory.ml lexer.ml parser.ml interpreter.ml brainfuck.ml

rm *.cmi *.cmx *.o

mv brainfuck ../

cd ..
