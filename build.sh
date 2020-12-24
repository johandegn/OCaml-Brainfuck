#!/bin/bash

ocamlopt -c tokens.ml nodes.ml
ocamlc -c lexer.mli parser.mli
ocamlopt lexer.ml parser.ml

rm *.cmi *.cmx *.o
