EMACS = emacs

.PHONY: clean patch dist

all: nethack.elc nethack-api.elc nethack-cmd.elc nethack-glyphs.elc nethack-keys.elc

%.elc: %.el
	$(EMACS) -batch --eval "(add-to-list 'load-path \".\")" \
		-f batch-byte-compile $<

clean:
	$(RM) *elc

# make a datestamped distribution in the current directory
PATCHFILE=enh-340.patch
DISTFILES=AUTHORS BUGS COPYING ChangeLog INSTALL Makefile README TODO		\
	$(PATCHFILE) mkpatch nethack-api.el nethack-cmd.el nethack-example.el	\
	nethack-glyphs.el nethack-keys-dvorak.el nethack-keys.el		\
	nethack.el
DISTDIR=nethack_el-$(shell date +%Y%m%d)

PATCH_OK=.patch-ok
$(PATCH_OK): test-patch ;

dist: $(PATCH_OK)
	mkdir ./$(DISTDIR)
	cp $(DISTFILES) ./$(DISTDIR)
	tar -cf /dev/stdout ./$(DISTDIR) | gzip > ./$(DISTDIR).tar.gz
	rm -rf ./$(DISTDIR)

# Generate patch.
# Apply the generated patch from inside the nethack dir like this: 
# src/nethack-3.4.0$ patch -p 1 < PATH/TO/$(PATCHFILE)
patch:
	sh ./mkpatch > $(PATCHFILE)
	rm -f $(PATCH_OK)

# Make sure nothing obvious was missed when hacking the source and
# creating the patch
test-patch: $(PATCHFILE)
	@echo testing: $< for sanity
	cd ..; tar -xzf nethack-340.tgz
	cd ../nethack-3.4.0 ; patch -fp1 < ../nethack-el/$(PATCHFILE)
	cd ../nethack-3.4.0/sys/unix; sh setup.sh
	$(MAKE) -C ../nethack-3.4.0
	HACKDIR=. ../nethack-3.4.0/src/nethack | grep api-init
	rm -r ../nethack-3.4.0
	touch $(PATCH_OK)
