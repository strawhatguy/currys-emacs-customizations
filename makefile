#### -*- mode: makefile -*-

.PHONY: all install compile clean

SOURCES:=edit-server.el
ELCS:=$(patsubst %.el, %.elc, $(SOURCES))
SNIPPETS:=$(shell find snippets -name '*.yasnippet')

all: compile install

install: dot.emacs $(ELCS) $(SNIPPETS)
	install -d ~/.emacs.d/include
	install -m 644 -b dot.emacs ~/.emacs
	install -m 644 -b $(SOURCES) $(ELCS) ~/.emacs.d/include
	rsync -ai snippets ~/.emacs.d

compile: $(ELCS)

clean:
	rm -f $(ELCS)

%.elc: %.el
	/usr/local/bin/emacs --batch -l bytecomp --eval '(byte-recompile-file "$<" t 0)'


