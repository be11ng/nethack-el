# This Makefile requires that the environment variables NH_VER and NH_VER_NODOTS
# exist.  Thees environment variables form the ``input'' for this Makefile.
# There is probably a better way to do this, but this works well, especially
# since make needs to communicate with the Emacs side of things.

SHELL = /bin/sh

.PHONY: patch hints build clean nuke

patch:
	cd nethack-src && patch -p 1 < ../../enh-$(NH_VER_NODOTS).patch

hints:
	cd nethack-src/sys/unix && $(SHELL) ./setup.sh

hints-3.6:
	cd nethack-src/sys/unix && $(SHELL) ./setup.sh hints/linux-lisp

build:
	$(MAKE) -C nethack-src all
	$(MAKE) -C nethack-src install

clean:
	rm -f nethack
	rm -rf nethackdir
	rm -rf nethack-src

nuke: clean
	rm -f nethack.tgz
