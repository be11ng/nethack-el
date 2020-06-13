# This Makefile requires that the environment variables NH_VER and NH_VER_NODOTS
# exist.  Thees environment variables form the ``input'' for this Makefile.
# There is probably a better way to do this, but this works well, especially
# since make needs to communicate with the Emacs side of things.

SHELL = /bin/sh

.PHONY: patch hints build

patch:
	cd nethack && patch -p 1 < ../../enh-$(NH_VER_NODOTS).patch

hints:
	cd nethack/sys/unix && $(SHELL) ./setup.sh

hints-3.6:
	cd nethack/sys/unix && $(SHELL) ./setup.sh hints/linux-lisp

build:
	$(MAKE) -C nethack all
	$(MAKE) -C nethack install
