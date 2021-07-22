# A GNU Makefile to run various tasks - compatibility for us old-timers.

# Note: This makefile include remake-style target comments.
# These comments before the targets start with #:
# remake --tasks to shows the targets and the comments

GIT2CL ?= admin-tools/git2cl
PYTHON ?= python3
PIP ?= pip3
RM  ?= rm

.PHONY: all build \
   check clean \
   develop dist doctest doc-data djangotest \
   gstest pytest \
   rmChangeLog \
   test

SANDBOX	?=
ifeq ($(OS),Windows_NT)
	SANDBOX = t
else
	UNAME_S := $(shell uname -s)
	ifeq ($(UNAME_S),Darwin)
		SANDBOX = t
	endif
endif

#: Default target - same as "develop"
all: develop

#: build everything needed to install
build:
	$(PYTHON) ./setup.py build

#: Set up to run from the source tree
develop:
	$(PIP) install -e .

#: Set up to run from the source tree with full dependencies
develop-full:
	$(PIP) install -e .[full]


#: Make distirbution: wheels, eggs, tarball
dist:
	./admin-tools/make-dist.sh

#: Install Mathics
install:
	$(PYTHON) setup.py install

check: pytest gstest doctest


#: Remove derived files
clean:
	rm mathics/*/*.so; \
	for dir in mathics/doc ; do \
	   ($(MAKE) -C "$$dir" clean); \
	done; \
	rm -f factorials || true

#: Run py.test tests. Use environment variable "o" for pytest options
pytest:
	py.test $(PYTEST_WORKERS) test $o


#: Run a more extensive pattern-matching test
gstest:
	(cd examples/symbolic_logic/gries_schneider && $(PYTHON) test_gs.py)


#: Create data that is used to in Django docs and to build TeX PDF
doc-data: mathics/builtin/*.py mathics/doc/documentation/*.mdoc mathics/doc/documentation/images/*
	$(PYTHON) mathics/docpipeline.py --output --keep-going

#: Run tests that appear in docstring in the code.
doctest-workaround:
	SANDBOX=$(SANDBOX) $(PYTHON) mathics/docpipeline.py --exclude=NIntegrate,MaxRecursion
	SANDBOX=$(SANDBOX) $(PYTHON) mathics/docpipeline.py --sections=NIntegrate,MaxRecursion

#: Run tests that appear in docstring in the code.
doctest:
	SANDBOX=$(SANDBOX) $(PYTHON) mathics/docpipeline.py $o

#: Make Mathics PDF manual via Asymptote and LaTeX
texdoc doc:
	(cd mathics/doc/tex && $(MAKE) doc)

#: Remove ChangeLog
rmChangeLog:
	$(RM) ChangeLog || true

#: Create a ChangeLog from git via git log and git2cl
ChangeLog: rmChangeLog
	git log --pretty --numstat --summary | $(GIT2CL) >$@
