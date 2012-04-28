#### -*- mode: makefile -*-

.PHONY: all install compile clean

SOURCES:=edit-server.el
ELCS:=$(patsubst %.el, %.elc, $(SOURCES))

all: compile install

install: dot.emacs $(ELCS)
	install -d ~/.emacs.d/include
	install -m 644 --backup=numbered dot.emacs ~/.emacs
	install -m 644 --backup=numbered $(SOURCES) $(ELCS) ~/.emacs.d/include

compile: $(ELCS)

clean:
	rm -f $(ELCS)

%.elc: %.el
	emacs --batch -l bytecomp --eval '(byte-recompile-file "$<" t 0)'


