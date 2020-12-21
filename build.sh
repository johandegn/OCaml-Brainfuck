#!/bin/bash

ocamlc -c lexer.mli
#ocamlopt -c tokens.ml lexer.ml
ocamlopt tokens.ml lexer.ml

rm *.cmi *.cmx
