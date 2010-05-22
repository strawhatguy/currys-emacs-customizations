
all: slime-update

install: slime-update
	/usr/bin/install -m 644 --backup=numbered .emacs ~/.emacs

slime-update: slime
	cd slime; git pull; cd -;

slime:
	git clone git://git.boinkor.net/slime.git