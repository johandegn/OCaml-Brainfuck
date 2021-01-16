#!/bin/bash
cd src

ocamlopt -c tokens.ml nodes.ml
ocamlc -c lexer.mli parser.mli interpreter.mli
ocamlopt -o brainfuck lexer.ml parser.ml interpreter.ml brainfuck.ml

rm *.cmi *.cmx *.o

mv brainfuck ../

cd ..
