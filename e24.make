#### -*- mode: makefile -*-

.PHONY: all install compile clean

all: compile install

install: e24.dot.emacs edit-server.elc
	install -d ~/.emacs.d/include
	install -m 644 --backup=numbered e24.dot.emacs ~/.emacs
	install -m 644 --backup=numbered edit-server.* ~/.emacs.d/include

compile: edit-server.elc

clean:
	rm -f edit-server.elc

%.elc: %.el
	emacs --batch -l bytecomp --eval '(byte-recompile-file "$<" t 0)'


