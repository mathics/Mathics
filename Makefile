# A GNU Makefile to run various tasks - compatibility for us old-timers.

# Note: This makefile include remake-style target comments.
# These comments before the targets start with #:
# remake --tasks to shows the targets and the comments

GIT2CL ?= admin-tools/git2cl
PYTHON ?= python3
PIP ?= pip3
RM      ?= rm

PHONY=all build check clean develop dist doc pytest test rmChangeLog

#: Default target - same as "develop"
all: develop

#: build everything needed to install; (runs Cython)
build:
	$(PYTHON) ./setup.py build

#: Set up to run from the source tree
develop:
	$(PIP) install -e .

#: Install mathics
install:
	$(PYTHON) setup.py install

check: pytest doctest 

# Run py.test tests. You can environment variable o for pytest options
pytest:
	py.test test $o

#: Run tests that appear in docstring in the code.
doctest:
	$(PYTHON) mathics/test.py $(output)

#: Make Mathics PDF manual
doc:
	(cd mathics && $(PYTHON) test.py -t && \
	cd doc/tex && make)

#: Remove ChangeLog
rmChangeLog:
	$(RM) ChangeLog || true

#: Create a ChangeLog from git via git log and git2cl
ChangeLog: rmChangeLog
	git log --pretty --numstat --summary | $(GIT2CL) >$@
