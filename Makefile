# path to your emacs
EMACS = emacs

.PHONY: clean patch dist

all: nethack.elc nethack-api.elc nethack-cmd.elc nethack-glyphs.elc nethack-keys.elc

%.elc: %.el
	$(EMACS) -batch --eval "(add-to-list 'load-path \".\")" \
		-f batch-byte-compile $<

clean:
	$(RM) *.elc

#
# Make a datestamped distribution in the current directory
#
# ../nethack must be your CVS version of the winlisp nethack sources
# ../nethack-$(NH_VER).tgz must exist
#

# nethack-el version
NETHACK_EL_VER=0.9.0

# nethack source version
NH_VER=3.4.1

NH_VER_NODOTS=$(shell echo $(NH_VER) | sed s/\\.//g)

PATCHFILE=enh-$(NH_VER_NODOTS).patch ese-007e0.patch
DISTFILES=AUTHORS BUGS COPYING ChangeLog INSTALL Makefile README TODO		\
	$(PATCHFILES) mkpatch nethack-api.el nethack-cmd.el nethack-example.el	\
	nethack-glyphs.el nethack-keys-dvorak.el nethack-keys.el		\
	nethack.el
DISTDIR=nethack_el-$(NETHACK_EL_VER)

PATCH_OK=.patch-ok
$(PATCH_OK): test-patch ;

dist: clean all $(PATCH_OK)
	@echo
	@echo Things look in order... building the distribution tarball.
	mkdir ./$(DISTDIR)
	cp $(DISTFILES) ./$(DISTDIR)
	tar -cf /dev/stdout ./$(DISTDIR) | gzip > ./$(DISTDIR).tar.gz
	rm -rf ./$(DISTDIR)

# Generate patch.
# Apply the generated patch from inside the nethack dir like this: 
# src/nethack-x.y.z patch -p 1 < PATH/TO/enh-xyz.patch
patch:
	@echo
	@echo Creating $(PATCHFILE)
	NH_VER=$(NH_VER) NH_VER_NODOTS=$(NH_VER_NODOTS) sh ./mkpatch > $(PATCHFILE)
	rm -f $(PATCH_OK)

# Make sure nothing obvious was missed when hacking the source and
# creating the patch
test-patch: $(PATCHFILE)
	@echo
	@echo Testing $< for sanity
	cd ..; tar -xzf nethack-$(NH_VER_NODOTS).tgz
	cd ../nethack-$(NH_VER) ; patch -fp1 < ../nethack-el/$(PATCHFILE)
	cd ../nethack-$(NH_VER)/sys/unix; sh setup.sh
	$(MAKE) -C ../nethack-$(NH_VER)
	cd ../nethack-$(NH_VER)/src; HACKDIR=. ./nethack | grep api-init
	rm -r ../nethack-$(NH_VER)
	touch $(PATCH_OK)
