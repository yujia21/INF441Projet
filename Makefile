SOURCES = language.ml parser.mly lexer.mll analyzer.ml simulator.ml sign.ml
RESULT = analyzer
ANNOTATE = true

all: dnc
	./analyzer --sign test1.prog

include OCamlMakefile