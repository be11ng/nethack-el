EMACS = emacs

.PHONY: clean patch

all: nethack.elc nethack-api.elc nethack-cmd.elc nethack-glyphs.elc nethack-keys.elc

%.elc: %.el
	$(EMACS) -batch --eval "(add-to-list 'load-path \".\")" \
		-f batch-byte-compile $<

clean:
	$(RM) *elc

# Rule for generating the patch to the nethack sources 
# 
# You need to have a clean nethack-3.3.1 directory and a *clean*
# version of the sources from cvs in the directory below the one this
# Makefile lives in.
#
# Note: For some reason doc/tmac.n got clobbered when importing the
# sources, so we ignore it.  Also, sys/msdos/old/README.old and
# sys/msdos/old/schema.old didn't get imported, so we ignore those as
# well.
patch:
	-(cd ..; diff -C 2 -N -r -x CVS -x .cvsignore -x '*.n' -x '*.old' \
	  nethack-3.3.1 nethack) > enh-331.patch
# apply the generated patch from inside the nethack dir like this: 
# src/nethack-3.3.1$ patch -p 1 < PATH/TO/PATCH/enh-331.patch
