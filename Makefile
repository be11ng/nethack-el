EMACS = emacs

.PHONY: clean patch

all: nethack.elc nethack-api.elc nethack-cmd.elc nethack-glyphs.elc nethack-keys.elc

%.elc: %.el
	$(EMACS) -batch --eval "(add-to-list 'load-path \".\")" \
		-f batch-byte-compile $<

clean:
	$(RM) *elc

patch:
	sh ./mkpatch > enh-331.patch

# apply the generated patch from inside the nethack dir like this: 
# src/nethack-3.3.1$ patch -p 1 < PATH/TO/enh-331.patch
