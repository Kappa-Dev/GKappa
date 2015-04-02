#VN:=$(shell cat tag/number)#            #Git version number for commit tag
#VERSION:=$(shell cat tag/version)#      #Major revision release number
#RELEASE:=$(shell cat tag/release)#      #Release number
DATE:=`date +'%Y-%m-%d %H:%M:%S'`#      #date YYYY-MM-DD 

.DEFAULT_GOAL := all
BIN=bin/


KAPPAMODELSREP=examples/

EXAMPLES=$(wildcard $(KAPPAMODELSREP)*.ml)
BINAS=$(patsubst $(KAPPAMODELSREP)%,$(BIN)%,$(EXAMPLES))
BINS=$(BINAS:%.ml=%)
TERM = $(shell echo $$TERM)

OCAMLINCLUDES=-I examples

ifeq ($(TERM), dumb) # An approximation of "am I launched from emacs ?" :-)
 OCAMLBUILDFLAGS = -classic-display -use-ocamlfind
else
 OCAMLBUILDFLAGS = -use-ocamlfind
endif

.PHONY: all clean temp-clean-for-ignorant-that-clean-must-be-done-before-fetch
.PHONY: check build-tests doc clean_doc fetch_version

.PRECIOUS: 

%.native %.byte: $(filter-out _build/,$(wildcard *.ml*)) $(wildcard examples/*.ml*) 
	$(OCAMLBINPATH)ocamlbuild $(OCAMLBUILDFLAGS) $(OCAMLINCLUDES) $@

bin/%: %.native Makefile
	[ -d bin ] || mkdir bin && cp $< $@
	rm -f $(notdir $@) && ln -s $@ $(notdir $@)

all: $(BINS)

clean: 
	$(OCAMLBINPATH)ocamlbuild -clean
	rm -rf $(BIN) 
	find . -name \*~ -delete

full: 
	make clean
	make USE_TK=1 || echo 0 

light: 
	make clean
	make USE_TK=0 || echo 0 

commit: fetch_version 
	echo -n `expr $(VN) + 1` > tag/number 
	echo -n $(DATE) > tag/date 
	make PREF="Not a release" send_caml

major_version: fetch_version
	echo -n `expr $(VERSION) + 1` > tag/version
	echo -n `expr $(VN) + 1`> tag/number 
	echo -n 1 > tag/release
	echo -n $(DATE) > tag/date 
	make PREF="Release " send_caml

release: fetch_version
	echo -n `expr $(RELEASE) + 1`> tag/release
	echo -n `expr $(VN) + 1`> tag/number 
	echo -n $(DATE) > tag/date 
	make PREF="Release " send_caml

fetch_version:
	cd tag ; git checkout HEAD * 

send_caml: 
	echo -n xxx$(VN)$(RELEASE)$(VERSION)$(DATE)xxx
	echo -n let git_commit_version,git_commit_release,git_commit_tag,git_commit_date  = $(VERSION),$(RELEASE),$(VN),\"$(DATE)\" > KaSa_rep/automatically_generated/git_commit_info.ml 
	git commit -a 
	git tag -a $(VN)  -m "$(PREF) v$(VERSION).$(RELEASE)...$(VN) $(DATE)"  
	git push --tags
	git push 
