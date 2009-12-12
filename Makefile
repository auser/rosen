#
# Makefile
#
# $Id: Makefile,v 1.17 2008/03/27 15:55:05 corrado_santoro Exp $
#
VSN=0.4.1
VERSION=$(VSN)-alpha
APP_NAME=rosen


all:
	@[ -d ebin ] || mkdir ebin
	@[ -f src/esdl.incl ] || \
	{ echo "need to configure first"; exit; } && \
	(cd src; make; cd ../examples; make; cd ..)

docs:
	@[ -d doc ] || mkdir doc
	@(cd src; \
	erl -noshell -run edoc_run application "'$(APP_NAME)'" \
               '".."' '[{def,{vsn,"$(VSN)"}}]'; \
	cd ..)

clean:
	@[ -f src/esdl.incl ] || \
	{ echo "need to configure first"; exit; } && \
	(cd src; make clean; cd ../examples; make clean; cd ..)

test:
	@erl -pa ebin -pa examples -run eurobot go

dist:
	@[ -d distibution ] || mkdir distibution
	@(cd distibution; rm -rf *; \
	cvs -z3 \
	-d:ext:corrado_santoro@rosen.cvs.sourceforge.net:/cvsroot/rosen \
	co -P rosen; \
	cd rosen; make docs; cd ..; \
	mv rosen rosen-$(VERSION); \
	find rosen-$(VERSION) | grep CVS | xargs rm -rf ;\
	tar zcvf rosen-$(VERSION).tar.gz rosen-$(VERSION)/ )

