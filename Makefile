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
   develop dist doc doc-data djangotest \
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

#: Build developer guide
developer-docs:
	$(MAKE) -C docs html

#: Build docker image
docker-image:
	$(DOCKER_COMPOSE) $(DOCKER_COMPOSE_FILE) build

#: Install mathics
install:
	$(PYTHON) setup.py install

check: pytest gstest

#: Remove derived files
clean:
	rm mathics/*/*.so; \
	for dir in mathics/doc ; do \
	   ($(MAKE) -C "$$dir" clean); \
	done;

#: Run py.test tests. Use environment variable "o" for pytest options
pytest:
	py.test test $o


#: Run a more extensive pattern-matching test
gstest:
	(cd examples/symbolic_logic/gries_schneider && $(PYTHON) test_gs.py)


#: Create data that is used to in Django docs and to build TeX PDF
doc-data mathics/doc/tex/data: mathics/builtin/*.py mathics/doc/documentation/*.mdoc mathics/doc/documentation/images/*
	$(PYTHON) mathics/test.py -ot -k

#: Run tests that appear in docstring in the code.
doctest:
	SANDBOX=$(SANDBOX) $(PYTHON) mathics/test.py $o

#: Make Mathics PDF manual
doc mathics.pdf: mathics/doc/tex/data
	(cd mathics/doc/tex && $(MAKE) mathics.pdf)

#: Remove ChangeLog
rmChangeLog:
	$(RM) ChangeLog || true

#: Create a ChangeLog from git via git log and git2cl
ChangeLog: rmChangeLog
	git log --pretty --numstat --summary | $(GIT2CL) >$@
