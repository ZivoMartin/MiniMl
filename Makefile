all: parser_files interpreter.out visualiser.out doc.html

interpreter.out: language/*.ml* bin/*.ml util/*.ml
	rm -rf interpreter.out
	dune build
	ln -s _build/default/bin/main.exe interpreter.out

visualiser.out: language/*.ml* bin/*.ml
	rm -rf visualiser.out
	dune build
	ln -s _build/default/bin/visualiser.exe visualiser.out

parser_files: language/*.mly
	dune build
	mkdir -p parser_files
	find _build/default/language/ -name *.automaton -exec cp {} parser_files/ \;
	find _build/default/language/ -name *.conflicts -exec cp {} parser_files/ \;
	find _build/default/language/ -name *.parser_files -exec cp {} parser_files/ \;

doc.html:
	rm -f doc.html
	dune build @doc
	ln -s _build/default/_doc/_html/index.html doc.html

.PHONY: clean
clean:
	rm -rf interpreter.out visualiser.out doc.html _build parser_files
