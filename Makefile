EMACS = emacs
RM = /bin/rm -f

.PHONY: clean patch dist

all: nethack.elc nethack-api.elc nethack-cmd.elc nethack-glyphs.elc nethack-keys.elc

%.elc: %.el
	$(EMACS) -batch --eval "(add-to-list 'load-path \".\")" \
		-f batch-byte-compile $<

clean:
	$(RM) *elc

# make a datestamped distribution in /tmp
DISTFILES=AUTHORS BUGS COPYING ChangeLog INSTALL Makefile README TODO		\
	enh-331.patch mkpatch nethack-api.el nethack-cmd.el nethack-example.el	\
	nethack-glyphs.el nethack-keys-dvorak.el nethack-keys.el		\
	nethack.el
DISTDIR=nethack_el-$(shell date +%Y%m%d)

dist:
	mkdir /tmp/$(DISTDIR)
	cp $(DISTFILES) /tmp/$(DISTDIR)
	(cd /tmp; tar -cf /dev/stdout ./$(DISTDIR) | gzip > /tmp/$(DISTDIR).tar.gz)
	rm -r /tmp/$(DISTDIR)

# apply the generated patch from inside the nethack dir like this: 
# src/nethack-3.4.0$ patch -p 1 < PATH/TO/enh-340.patch
patch:
	sh ./mkpatch > enh-340.patch
