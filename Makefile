
PWD:=$(shell pwd)

.PHONY: all install compile clean

all: compile install

install: .emacs-installable
	/usr/bin/install -m 644 --backup=numbered .emacs-installable ~/.emacs

compile:
	/usr/bin/emacs --batch --eval '(byte-recompile-directory "." 0)'

.emacs-installable: .emacs
	sed -e 's,REPLACEME,$(PWD),g' $< > $@

clean:
	rm -f *~ .emacs-installable *.elc */*.elc
