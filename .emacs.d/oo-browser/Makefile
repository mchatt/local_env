#
# SUMMARY:      Build OO-Browser directories and distributions.
#
# AUTHOR:       Bob Weiner
#
# ORIG-DATE:     6-Oct-94 at 03:42:38
# LAST-MOD:      3-Jan-02 at 18:21:40 by Bob Weiner
#
# Copyright (C) 2000-2002  Bob Weiner
# Copyright (C) 1994-1999  BeOpen.com
# See the file BR-COPY for license information.
#
# This file is part of the OO-Browser.
#
# DESCRIPTION:  
#
#   The OO-Browser (the standard distribution) contains all supported languages.
#   The C++ OO-Browser contains C++ support only, for example.
#
#   Before doing your first make, edit the CONFIGURABLE SECTION below.
#   You will also have to edit the tree-x/Makefile to match your system if
#   you want the `xoobr' executable built (graphical browsing under the X
#   Window System).
#
#   Make any needed changes now and save the file.  Then select from the USAGE
#   lines immediately following.
#
#   USAGE:	Whatever Emacs version you use, invoke:
#   	             make install        (installs only the textual OO-Browser)
#
#               Use this command if you want to build and install the X graphical
#               interface to the OO-Browser:
#                    make install-xoobr
#
#               For languages which support linking with C language files,
#               the OO-Browser uses a separate etags-like executable called
#               `ootags' built from the ootags.c source code to capture and
#               then list C constructs.  This executable is built and
#               installed automatically when building InfoDock or modern
#               versions of XEmacs (it is one of the executables from `lib-src').
#               If you run GNU Emacs, and you want C constructs included in
#               OO-Browser listings, you MUST copy the ootags.c file from
#               <OO-BROWSER-DIR> to your <EMACS-ROOT-DIR>/lib-src directory.
#               Then edit the Makefile in that directory by copying the etags 
#               build target and changing `etags' to `ootags' so that a new
#               `ootags' executable will be built.  Then do a `make install'
#               in that directory or build the executable and copy it into
#               the oo-browser/ directory.
#
#               Then the OO-Browser should be ready for use if you have
#               followed the installation instructions in "BR-README".
#
#   -----------------
#               To build a PostScript version of user manual:
#                    make ps
#
#               The process of building a correctly printable OO-Browser
#               manual is quite involved and not recommended for any but
#               the most skilled users.

#   -----------------
#
#               To force rebuilding of all .elc files, even ones that are not
#               out of date:
#                    make all-elc
#
#   -----------------
#     For OO-Browser maintainers:
#
#               Use `make help' to list distribution build targets.
#
# DESCRIP-END.

##########################################################################
#                         CONFIGURABLE SECTION                           #
##########################################################################

# Emacs executable used to byte-compile .el files into .elc's.
# Possibilities include: emacs, infodock, xemacs, etc.
EMACS = xemacs

# Command used to build the .info version of the user manual.
MAKEINFO=makeinfo --no-split

# Maintainer-specific command used to build the .html version of the user
# manual.  If you do decide to build the html version of the manual,
# substitute your own command below.
TEXI2HTML = id-texi2html -html_only -number -split_chapter

# `BWCTO_HTML_MANUALS' is an environment variable that specifies where
# the HTML versions of the manuals are to be installed.  If must be set
# before use of the `make html' target but is not needed for any other
# builds.  Uncomment and add an absolute pathname to the next line to set
# this variable (omitting the trailing directory separator).
# BWCTO_HTML_MANUALS =

# Where to find the root of the InfoDock tree for making OO-Browser
# distributions.
id_dir = $(HOME)/infodock
# Where to find the .texi source of the user manual.
man_dir := $(shell pwd)/man
# Where to install the Postscript version of the user manual.
ps_dir   = $(man_dir)
# Where to install BeOpen.com banner image.
data_dir = $(id_dir)/id-etc
# Where to install the Info version of the OO-Browser manual.
info_dir = $(id_dir)/id-info
# The parent directory of the OO-Browser Lisp code directory.
lisp_dir = $(id_dir)/id-lisp

# Site-specific Emacs Lisp libraries to load before byte-compiling any files
# from this package.  Typically the only reason to set this is to get Emacs
# to include the directory of this package into its load-path variable, which
# determines where it will find Lisp library files to load.
#
# InfoDock and XEmacs 19.12 or higher include this package and automatically
# add its directory to load-path.  Under Emacs, if you add this directory
# to load-path in your site-lisp/site-start.el file, then you need not change
# this setting since site-start is automatically loaded whenever Emacs starts
# up.  If, however, you set load-path in your personal ~/.emacs file, you
# must add that to this setting.
#
# You must include the .el or .elc file suffix on each library name and each
# must be preceded by the `-l ' command-line flag.  If the directory in which
# the library is stored will not be in your Emacs load-path when Emacs
# attempts to load the library, you must include the full pathname to the
# library.  Here is an example setting.
#
# SITE_PRELOADS = -l ~/.emacs -l set-load-path.el
#
SITE_PRELOADS =

# Shell used to process this Makefile.  Bourne shell syntax is required.
SHELL = /bin/sh

# UNIX commands you may want to change for your particular system.
CP = \cp -p
ETAGS = \etags
INSTALL = \install -m 644 -c 
MKDIR = \mkdir -p
MAKE = \make
RM = \rm -f
TAR = \tar
ZIP = \zip -qry

# Directory in which to create new distributions of the OO-Browser.
dist_dir = /tmp
# Directory in which to create temporary files for building distributions.
temp_dir = /tmp
# Working subdirectory used to build distributions; don't change this.
temp_oo_browser = $(temp_dir)/oo-browser

# Temp file to use to build .elc files.
ELISP_TO_COMPILE = $(temp_dir)/elc-${USER}

##########################################################################
#                     NO CHANGES REQUIRED BELOW HERE                     #
##########################################################################

# This ver setup won't work under any make except GNU make, so set it manually.
# OO_BROWSER_VERSION := $(shell head -3 br-vers.el | tail -1 | sed -e 's/.*|0*\([0-9.]*\)|[^\|]*$$/\1/')
OO_BROWSER_VERSION = 4.08

# Libraries that must be pre-loaded before trying to byte-compile anything.
PRELOADS = $(SITE_PRELOADS) -l ./br-start.el

# Compile in batch mode. Under Emacs and XEmacs, load
# site-lisp/site-start.el, which may set load-path.
BATCHFLAGS = -batch

# Directories other than the current directory in which to find files.
# This doesn't work in all versions of make, so we also add the
# path explicitly to those files which need it.
VPATH = hypb man

HYPB_EL   = hypb/h-skip-bytec.lsp hypb/hact.el hypb/hargs.el \
	    hypb/hhist.el hypb/hmail.el hypb/hmouse-drv.el \
	    hypb/hmouse-key.el hypb/hmous-info.el \
	    hypb/hmouse-reg.el hypb/hmouse-sh.el hypb/hmouse-tag.el \
	    hypb/hpath.el hypb/hui-mouse.el \
	    hypb/hui-window.el hypb/hvar.el \
	    hypb/hversion.el hypb/hypb.el hypb/hsys-w3.el hypb/set.el

HYPB_ELC  = hypb/hact.elc hypb/hargs.elc \
	    hypb/hhist.elc hypb/hmail.elc hypb/hmouse-drv.elc \
	    hypb/hmouse-key.elc hypb/hmous-info.elc \
	    hypb/hmouse-reg.elc hypb/hmouse-sh.elc hypb/hmouse-tag.elc \
	    hypb/hpath.elc hypb/hui-mouse.elc \
	    hypb/hui-window.elc hypb/hvar.elc \
	    hypb/hversion.elc hypb/hypb.elc hypb/hsys-w3.elc hypb/set.elc

# Files from Hyperbole necessary for OO-Browser mouse support.
HYPERBOLE_FILES = $(HYPB_EL) $(HYPB_ELC)

CXX_FILES         = br-c++-ft.el  br-c++.el  c++-browse.el \
		    br-c++-ft.elc br-c++.elc c++-browse.elc
CLOS_FILES        = br-clos-ft.el  br-clos.el  clos-brows.el \
                    br-clos-ft.elc br-clos.elc clos-brows.elc 
EIFFEL_FILES      = br-eif-ft.el  br-eif.el  eif-browse.el  eif-calls.el \
		    eif-ise-er.el \
                    br-eif-ft.elc br-eif.elc eif-browse.elc eif-calls.elc \
		    eif-ise-er.elc
INFO_FILES        = br-info.el info-brows.el \
                    br-info.elc info-brows.elc
JAVA_FILES        = br-java.el  br-java-ft.el  java-brows.el \
                    br-java.elc br-java-ft.elc java-brows.elc
OBJECTIVE_C_FILES = br-objc-ft.el  br-objc.el  objc-brows.el \
                    br-objc-ft.elc br-objc.elc objc-brows.elc
PYTHON_FILES      = br-python.el br-python-ft.el pyth-brows.el \
                    br-python.elc br-python-ft.elc pyth-brows.elc
SMALLTALK_FILES   = br-smt.el smt-browse.el \
                    br-smt.elc smt-browse.elc

EL_COMPILE  = br-c-ft.el br-c++-ft.el br-c++.el br-clos-ft.el \
	      br-clos.el br-compl.el \
              br-eif-ft.el br-eif.el br-env.el br-ftr.el br-info.el \
              br-init.el br-java.el br-java-ft.el \
	      br-lib.el br-menu.el br-name.el br-objc-ft.el \
              br-objc.el br-python.el br-python-ft.el br-site.el br-vers.el \
	      br-smt.el br-start.el br-tree.el br.el c++-browse.el \
	      clos-brows.el eif-browse.el eif-calls.el eif-ise-er.el \
	      hasht.el hmouse-br.el info-brows.el java-brows.el \
	      objc-brows.el pyth-brows.el smt-browse.el auto-autoloads.el

ELC_COMPILE = br-c-ft.elc br-c++-ft.elc br-c++.elc br-clos-ft.elc \
	      br-clos.elc br-compl.elc \
              br-eif-ft.elc br-eif.elc br-env.elc br-ftr.elc br-info.elc \
              br-init.elc br-java.elc br-java-ft.elc \
	      br-lib.elc br-menu.elc br-name.elc br-objc-ft.elc \
              br-objc.elc br-python.elc br-python-ft.elc \
              br-site.elc br-vers.elc \
	      br-smt.elc br-start.elc br-tree.elc br.elc c++-browse.elc \
	      clos-brows.elc eif-browse.elc eif-calls.elc eif-ise-er.elc \
	      hasht.elc hmouse-br.elc info-brows.elc java-brows.elc \
	      objc-brows.elc pyth-brows.elc smt-browse.elc auto-autoloads.elc

OO_BROWSER_FILES = $(EL_COMPILE) $(ELC_COMPILE) $(HYPERBOLE_FILES) \
	Makefile GNUmakefile.id BR-* $(man_dir)/oo-browser.texi

EL_TAGS = $(EL_COMPILE)

# Prevent make from trying to compile this file.
.DEFAULT: hypb/h-skip-bytec.lsp ;

.SUFFIXES:          # Delete the default suffixes
.SUFFIXES: .el .elc # Define the list of file suffixes to match to rules

# Build the OO-Browser for use in current directory.
all: elc

# Build the X OO-Browser for graphical tree browsing.
xoobr:
	cd tree-x; $(MAKE)

version: info
	fgrep -L ${OO_BROWSER_VERSION} Makefile GNUmakefile.id BR-RELEASE br-vers.el $(man_dir)/oo-browser.texi
	@ echo "Any fgrep output means the version number has not been updated in that file."
	@ echo ""

# Build the Info and Postscript versions of the OO-Browser user manual.
doc: info ps

info: $(man_dir)/oo-browser.info
$(man_dir)/oo-browser.info: $(man_dir)/oo-browser.texi
	cd $(man_dir); $(MAKEINFO) -o oo-browser.info oo-browser.texi

ps: $(ps_dir)/oo-browser.ps
$(ps_dir)/oo-browser.ps: $(man_dir)/oo-browser.texi
	$(MKDIR) $(ps_dir); cd $(ps_dir) \
	  && $(RM) im && ln -s ~/infodock/id-info/im \
	  && texi2dvi $(man_dir)/oo-browser.texi && $(RM) oo-browser.ps \
	  && dvips -D 600 -Z -T7in,9.25in -f < oo-browser.dvi | psorder > oo-browser-$(OO_BROWSER_VERSION).ps \
	  && dvips -D 600 -Z -k -T7in,9.25in -f < oo-browser.dvi | psorder > oo-browser-$(OO_BROWSER_VERSION)-crop.ps \
	  && ln -s oo-browser-$(OO_BROWSER_VERSION)-crop.ps oo-browser.ps \
	  && echo "The OO-Browser Postscript manual was saved in: $(ps_dir)"

# Gzip the Postscript versions of the user manual.
ps.gz: $(ps_dir)/oo-browser.ps.gz
	cd $(ps_dir) && echo "gzip oo-browser-$(OO_BROWSER_VERSION).ps oo-browser-$(OO_BROWSER_VERSION)-crop.ps" \
	  && gzip oo-browser-$(OO_BROWSER_VERSION).ps oo-browser-$(OO_BROWSER_VERSION)-crop.ps \
	  && $(RM) oo-browser.ps \
	  && ln -s oo-browser-$(OO_BROWSER_VERSION)-crop.ps.gz oo-browser.ps.gz

dvi: $(ps_dir)/oo-browser.dvi
$(ps_dir)/oo-browser.dvi: $(man_dir)/oo-browser.texi
	cd $(ps_dir) && texi2dvi $(man_dir)/oo-browser.texi

draft: $(ps_dir)/oo-browser-draft.ps
$(ps_dir)/oo-browser-draft.ps: $(ps_dir)/oo-browser-draft.dvi
	cd $(ps_dir) && $(RM) oo-browser.ps \
	  && dvips -D 600 -Z -T7in,9.25in -f < oo-browser.dvi | psorder > oo-browser-draft-$(OO_BROWSER_VERSION).ps
	  && ln -s oo-browser-draft-$(OO_BROWSER_VERSION).ps oo-browser-draft.ps

html: ${BWCTO_HTML_MANUALS}/oo-browser_toc.html
${BWCTO_HTML_MANUALS}/oo-browser_toc.html: $(man_dir)/oo-browser.texi
	cd ${BWCTO_HTML_MANUALS} && $(TEXI2HTML) $(man_dir)/oo-browser.texi

install: elc install-info $(data_dir)/beopen-banner.xpm

install-xoobr:
	# Install the X OO-Browser in its executable directory.
	cd tree-x; $(MAKE) install

install-info: $(info_dir)/oo-browser.info
$(info_dir)/oo-browser.info: $(man_dir)/oo-browser.info
	$(MKDIR) $(info_dir)/im; \
	  cd $(man_dir); $(INSTALL) oo-browser.info* $(info_dir); \
	  $(INSTALL) im/*.{gif,ps} $(info_dir)/im

$(data_dir)/beopen-banner.xpm: beopen-banner.xpm
	$(INSTALL) beopen-banner.xpm $(data_dir)

# Record any .el files that need to be compiled.
.el.elc:
	@ echo $< >> $(ELISP_TO_COMPILE)

# Compile all recorded .el files.
elc: elc-init $(ELC_COMPILE) $(HYPB_ELC)
	@- \test ! -f $(ELISP_TO_COMPILE) \
            || (echo "These files will be compiled: " \
                 && echo "`cat $(ELISP_TO_COMPILE)`" \
                 && $(EMACS) $(BATCHFLAGS) $(PRELOADS) \
                       -f batch-byte-compile `cat $(ELISP_TO_COMPILE)`)
	@ $(RM) $(ELISP_TO_COMPILE)
#	@ $(RM) hypb

elc-debug: elc-init $(ELC_COMPILE) $(HYPB_ELC)
	@- \test ! -f $(ELISP_TO_COMPILE) \
            || (echo "These files will be compiled: " \
                 && echo "`cat $(ELISP_TO_COMPILE)`" \
                 && $(EMACS) $(BATCHFLAGS) $(PRELOADS) \
	               -eval "(call-with-condition-handler 'really-early-error-handler 'batch-byte-compile `cat $(ELISP_TO_COMPILE)`)")
	@ $(RM) $(ELISP_TO_COMPILE)
#	@ $(RM) hypb

elc-init:
	@-if [ ! -d hypb ]; then ln -s ../hyperbole hypb; fi
	@ $(RM) $(ELISP_TO_COMPILE)

# Remove and then rebuild all byte-compiled .elc files for Emacs, even
# those .elc files which do not yet exist.
all-elc:
	$(RM) *.elc
	$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile $(EL_COMPILE)

tags: TAGS
TAGS: $(EL_TAGS)
	$(ETAGS) $(EL_TAGS)

hypb-tags: $(EL_TAGS) $(HYPB_EL)
	cd $(temp_oo_browser); $(ETAGS) $(EL_TAGS) $(HYPB_EL)

clean:  distclean
	$(RM) *.elc

#
# Distribution building help
#  
help:
	@ echo -e "\n OO-Browser distributions for all platforms:  make dist\n" \
	     "Sun Solaris distribution only:               make solaris\n" \
	     "PA-RISC HP-UX distribution only:             make hpux\n" \
	     "Intel Linux distribution only:               make linux\n" \
	     "Intel Windows distribution only:             make w32.\n"

# Build all available OO-Browser distributions.
dist: $(dist_dir)/oo-browser-$(OO_BROWSER_VERSION).tgz \
      $(dist_dir)/oo-browser-$(OO_BROWSER_VERSION).zip \
      $(dist_dir)/oo-browser-$(OO_BROWSER_VERSION)-sparc-sun-solaris.tgz \
      $(dist_dir)/oo-browser-$(OO_BROWSER_VERSION)-hppa-hp-hpux.tgz \
      $(dist_dir)/oo-browser-$(OO_BROWSER_VERSION)-i386-intel-linux.tgz \
      $(dist_dir)/oo-browser-$(OO_BROWSER_VERSION)-i386-intel-w32.zip \
      $(dist_dir)/BR-FEATURES $(dist_dir)/BR-README $(dist_dir)/BR-RELEASE
	chmod u+rw,g+r,o+r $(dist_dir)/*
	@ chmod u+rwx $(dist_dir)
	@ echo; echo "All distributions:"
	@ ls -l $(dist_dir)/oo-browser*.{tgz,zip}

$(dist_dir)/BR-FEATURES: BR-FEATURES
	$(INSTALL) BR-FEATURES $(dist_dir)/
$(dist_dir)/BR-README: BR-README
	$(INSTALL) BR-README $(dist_dir)/
$(dist_dir)/BR-RELEASE: BR-RELEASE
	$(INSTALL) BR-RELEASE $(dist_dir)/

#
# Build dependencies required before making a distribution.
#
predist: version elc install html

#
# Source and runtime targets
#

runtime: predist
	@ echo; echo "Building src and runtime dist..."
	$(RM) -r $(temp_oo_browser)
	cd .. && $(TAR) clf $(temp_dir)/br.tar oo-browser
	cd $(temp_dir) && $(TAR) xf br.tar \
	  && cd $(temp_oo_browser) && $(RM) -r hash-test.* hypb man tree-nx; \
	  $(MAKE) distclean; \
	  $(MKDIR) hypb man man/im; chmod 755 hypb man man/im; \
	  $(CP) $(lisp_dir)/oo-browser/man/im/*.{gif,ps} man/im/; \
	  $(CP) $(lisp_dir)/oo-browser/man/oo-browser.texi man/; \
	  $(CP) $(id_dir)/id-info/oo-browser.info* man/
	$(CP) $(HYPERBOLE_FILES) $(temp_oo_browser)/hypb/
	$(RM) hypb
	$(CP) $(data_dir)/beopen-banner.xpm $(temp_oo_browser)/
	$(RM) $(temp_dir)/br.tar

$(dist_dir)/oo-browser-$(OO_BROWSER_VERSION).tgz: $(OO_BROWSER_FILES)
	make runtime && cd $(temp_dir) \
	  && $(TAR) -czlf $(dist_dir)/oo-browser-$(OO_BROWSER_VERSION).tgz oo-browser

$(dist_dir)/oo-browser-$(OO_BROWSER_VERSION).zip: $(OO_BROWSER_FILES)
	cd $(temp_dir) \
	  && $(ZIP) $(dist_dir)/oo-browser-$(OO_BROWSER_VERSION).zip oo-browser


#
# Sun Solaris distribution targets
#

solaris solarisdist:
	@ echo; echo "Building solaris dist..."
	$(RM) -r $(temp_oo_browser)
	$(MKDIR) $(temp_oo_browser) && cd $(temp_oo_browser) \
	  && $(CP) $(id_dir)/bin/sparc-sun-solaris/{ootags,xoobr} ./ \
	  && chmod 755 $(temp_oo_browser)/{ootags,xoobr}

$(dist_dir)/oo-browser-$(OO_BROWSER_VERSION)-sparc-sun-solaris.tgz: \
	$(id_dir)/bin/sparc-sun-solaris/ootags \
	$(id_dir)/bin/sparc-sun-solaris/xoobr
	  make solaris && cd $(temp_dir) \
	    && $(TAR) -czlf $(dist_dir)/oo-browser-$(OO_BROWSER_VERSION)-sparc-sun-solaris.tgz oo-browser

#
# HP-UX PA-RISC distribution targets
#

hpux hpuxdist: predist
	@ echo; echo "Building hpux dist..."
	$(RM) -r $(temp_oo_browser)
	$(MKDIR) $(temp_oo_browser) && cd $(temp_oo_browser) \
	  && $(CP) $(id_dir)/bin/hppa-hp-hpux/{ootags,xoobr} ./ \
	  && chmod 755 $(temp_oo_browser)/{ootags,xoobr}

$(dist_dir)/oo-browser-$(OO_BROWSER_VERSION)-hppa-hp-hpux.tgz: \
	$(id_dir)/bin/hppa-hp-hpux/ootags \
	$(id_dir)/bin/hppa-hp-hpux/xoobr
	  make hpux && cd $(temp_dir) \
	    && $(TAR) -czlf $(dist_dir)/oo-browser-$(OO_BROWSER_VERSION)-hppa-hp-hpux.tgz oo-browser


#
# Linux distribution targets
#

linux linuxdist: predist
	@ echo; echo "Building linux dist..."
	$(RM) -r $(temp_oo_browser)
	$(MKDIR) $(temp_oo_browser) && cd $(temp_oo_browser) \
	  && $(CP) $(id_dir)/bin/i386-intel-linux/{ootags,xoobr} ./ \
	  && chmod 755 $(temp_oo_browser)/{ootags,xoobr}

$(dist_dir)/oo-browser-$(OO_BROWSER_VERSION)-i386-intel-linux.tgz: \
	$(id_dir)/bin/i386-intel-linux/ootags \
	$(id_dir)/bin/i386-intel-linux/xoobr
	  make linux && cd $(temp_dir) \
	    && $(TAR) -czlf $(dist_dir)/oo-browser-$(OO_BROWSER_VERSION)-i386-intel-linux.tgz oo-browser


#
# Windows distribution targets
#

w32 w32dist: predist
	@ echo; echo "Building windows (w32) dist..."
	$(RM) -r $(temp_oo_browser)
	$(MKDIR) $(temp_oo_browser) && cd $(temp_oo_browser) \
	  && $(CP) $(lisp_dir)/oo-browser/tree-w32/{ootags.exe,oobr.exe,OOBR.HLP} ./ \
	  && chmod 755 $(temp_oo_browser)/{ootags.exe,oobr.exe}

$(dist_dir)/oo-browser-$(OO_BROWSER_VERSION)-i386-intel-w32.zip: \
	$(lisp_dir)/oo-browser/tree-w32/ootags.exe \
	$(lisp_dir)/oo-browser/tree-w32/oobr.exe \
	$(lisp_dir)/oo-browser/tree-w32/OOBR.HLP
	  make w32 && cd $(temp_dir) \
	    && $(ZIP) $(dist_dir)/oo-browser-$(OO_BROWSER_VERSION)-i386-intel-w32.zip oo-browser


#
# Cleanup targets
#

distclean:
	if [ -d $(temp_oo_browser) ]; then \
	  cd $(temp_oo_browser) && $(RM) -r beopen-banner.xpm TAGS core .place* .*~ *~ *\# \
	    *- *.orig *.rej .nfs* CVS .cvsignore; fi
	if [ -d $(temp_oo_browser)/man ]; then \
	  cd $(temp_oo_browser)/man && $(RM) -r core .place* .*~ *~ \
	    *\# *- *.orig *.rej .nfs* CVS .cvsignore; fi
	if [ -d $(temp_oo_browser)/man/im ]; then \
	  cd $(temp_oo_browser)/man/im && $(RM) -r core .place* .*~ *~ \
	    *\# *- *.orig *.rej .nfs* CVS .cvsignore; fi
	if [ -d $(temp_oo_browser)/tree-x ]; then \
	  cd $(temp_oo_browser)/tree-x && $(RM) -r .*~ *~ *\# *- *.orig *.rej \
	    *.o xoobr .nfs* CVS .cvsignore; fi
	if [ -d $(temp_oo_browser)/tree-w32 ]; then \
	  cd $(temp_oo_browser)/tree-w32 && $(RM) -r .*~ *~ *\# *- *.orig *.rej \
	    *.o .nfs* CVS .cvsignore ootags.exe oobr.exe OOBR.HLP; fi
