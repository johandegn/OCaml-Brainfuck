#!/bin/bash

ocamlopt -c tokens.ml
ocamlc -c lexer.mli
ocamlopt lexer.ml

rm *.cmi *.cmx *.o
