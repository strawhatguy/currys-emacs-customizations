
PWD:=$(shell pwd)

.PHONY: all install clean

all: install

install: .emacs-installable
	/usr/bin/install -m 644 --backup=numbered .emacs-installable ~/.emacs

.emacs-installable: .emacs
	sed -e 's,REPLACEME,$(PWD),g' $< > $@

clean:
	rm -f *~ .emacs-installable
