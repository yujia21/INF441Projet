SOURCES = language.ml parser.mly lexer.mll simulator.ml sign.ml analyzer.ml
RESULT = analyzer
ANNOTATE = true

all: dnc
	./analyzer test1.prog
	./analyzer test2.prog
        
sign: dnc
	./analyzer --sign test1.prog
	./analyzer --sign test2.prog


include OCamlMakefile
