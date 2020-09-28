# A GNU Makefile to run various tasks - compatibility for us old-timers.

# Note: This makefile include remake-style target comments.
# These comments before the targets start with #:
# remake --tasks to shows the targets and the comments

PYTHON ?= python3
PIP ?= pip3
RM  ?= rm

.PHONY: all build check clean develop dist doc pytest test rmChangeLog

#: Default target - same as "develop"
all: develop

#: build everything needed to install; (runs Cython)
build:
	$(PYTHON) ./setup.py build

#: Set up to run from source
develop:
	$(PIP) install -e .

#: Set up to run from source
install:
	$(PYTHON) setup.py install

#: Run tests. For now there's not much here
check:
	python test.py

#: Create a ChangeLog from git via git log and git2cl
doc doctest ChangeLog:
	$(MAKE) -C .. $@
