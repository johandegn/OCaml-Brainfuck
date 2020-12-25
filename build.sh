#!/bin/bash

ocamlopt -c tokens.ml nodes.ml
ocamlc -c lexer.mli parser.mli interpreter.mli
ocamlopt lexer.ml parser.ml interpreter.ml brainfuck.ml

rm *.cmi *.cmx *.o
