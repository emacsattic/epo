#
# Makefile for EPO
#

all:

PREFIX	= /usr/local

## mule2
#EMACS	= mule
#EMACSDIR= ${PREFIX}/lib/${EMACS}
## emacs20, emacs21
EMACS	= emacs
EMACSDIR= ${PREFIX}/share/${EMACS}
## XEmacs
#EMACS	= xemacs
#EMACSDIR= ${PREFIX}/lib/${EMACS}
## Meadow (Sample)
#EMACS	= meadow
#EMACSDIR = c:/usr/local/meadow

# Comment out below if you are using Emacs Windows(meadow, etc)
GEO	= -geometry 80x20+0+0

LISPDIR	= ${EMACSDIR}/site-lisp/epo
# LISPDIR	= ${EMACSDIR}/site-packages/lisp/epo
DOCDIR	= ${LISPDIR}/docs
HELPDIR	= ${EMACSDIR}/site-lisp
INFODIR	= ${PREFIX}/info

TAR	= tar
# INSTALL	= install -c -m 444
INSTALL	= cp
MKDIR	= mkdir -p


###################
# Do not edit below
###################
# make install		to install EPO into public space
# make package		to create package for relase
# make clean		to delete all producted files
# make ci		to check in all
# make co		to check out all
MVER	= 1.2
LISP	= ${LISPMAIN} ${LISPEXTRA}
LISPMAIN = epo.el epolib.el epoi.el epoc.el epop.el epor.el
# epom.el
LISPEXTRA= epo-c.el epo-elisp.el epo-perl.el epo-ruby.el epo-java.el \
	   epo-tex.el epo-html.el contrib/*.el
DOCS	= ${DOCSRC} ${DOCOBJ} ${NEWS} COPYING
NEWS	= epo.new
DOCSRC	= docs/epoj.texi docs/dir docs/epo.ref
DOCOBJ	= docs/epoj
EXTRA	= makefile ruby-methods epo-example.el
DISTRIB = ${EXTRA} ${LISP} ${DOCS}
RCSFILE	= ${LISP} ${NEWS} ${DOCSRC}
PACK	= `ls ${DISTRIB}`
TMPDIR	= /tmp
VERSION = `head epo.el|awk '/rev\./{print $$4}'`
PACKDIR	= ${TMPDIR}/epo-${VERSION}

all:
	@echo "Edit this makefile first."
	@echo 'Type "make install" to install EPO.'
	@echo 'If you cling to elc files. type "make elc" before make install'

install: install-real

install-real:
	if [ ! -d ${LISPDIR} ]; then ${MKDIR} ${LISPDIR}; fi
	if [ ! -d ${DOCDIR} ]; then ${MKDIR} ${DOCDIR}; fi
	if [ ! -d ${INFODIR} ]; then ${MKDIR} ${INFODIR}; fi
	for f in *.el; do \
	 rm -f ${LISPDIR}/$${f}c; \
	done
	${INSTALL} *.el* contrib/*.el* ${NEWS} ${LISPDIR}
	${INSTALL} ${DOCSRC} ${DOCDIR}
	${INSTALL} ${DOCOBJ} ${INFODIR}
	@echo "Add next few lines into your site's info dir manually please!"
	@cat docs/dir

install-nw: bytecompile-nw install-real

elc:	bytecompile

bytecompile: lp1
	${EMACS} -batch -l ./lp.el -e batch-byte-compile ${LISP}

lp:
	echo '(setq load-path (cons "." load-path))'	> lp.el
	echo '(load-file "./epolib.el")'		>>lp.el

lp1:	lp
	echo '(load-file "./epo.el")'			>>lp.el
	echo '(load-file "./epo-html.el")'		>>lp.el


clean:
	rm -f *.elc *~ lp.el

info: docs/epoj

docs/epoj: docs/epoj.texi
	(cd docs; ${EMACS} -batch epoj.texi -e texinfo-format-buffer \
	 -e basic-save-buffer)

package: info
	@-mkdir ${PACKDIR}
	@tar cf - ${PACK} | (cd ${PACKDIR}; tar xf -)
	( version=${VERSION}; cd ${TMPDIR}; \
	     ${TAR} vzcf ${TMPDIR}/epo-$$version.tar.gz epo-$$version)

ci:
	ci -r${VERSION} -sRel -f ${RCSFILE}
	ci -u${VERSION} makefile 00readme

co:
	co ${RCSFILE}

co-l:
	co -l ${RCSFILE}

tci:
	ci -l${VERSION}.0 -Ncurrent ${RCSFILE} makefile

dostci:
	ci -l${MVER}.0 -Ncurrent @rcsfile

RSYNCDIR	= ${HOME}/http/yatex/rsync/epo
sync:
	@-mkdir ${PACKDIR}
	@tar cf - ${PACK} | (cd ${PACKDIR}; tar xf -)
	syncdir -A ${PACKDIR} ${RSYNCDIR}
	rm -rf ${PACKDIR} 

tobuell:
	rsync -auvzH --delete ~/work/dthesis/emg/EPO buell:work
tobuell-n:
	rsync -auvzHn --delete ~/work/dthesis/emg/EPO buell:work
frombuell:
	rsync -auvzH --delete buell:work/EPO ~/work/dthesis/emg
frombuell-n:
	rsync -auvzHn --delete buell:work/EPO ~/work/dthesis/emg
