
XETEX ?= "xelatex -halt-on-error"
LATEXMK ?= latexmk
BASH ?= /bin/bash
#-quiet

#: Default target: Make everything
all: mathics.pdf

#: Build mathics PDF
mathics.pdf: mathics.tex documentation.tex logo-text-nodrop.pdf logo-heptatom.pdf data
	$(LATEXMK) -f -pdf -pdflatex=$(XETEX) mathics

#: Generate logos used in the titlepage
logo-heptatom.pdf logo-text-nodrop.pdf:
	(cd .. && $(BASH) ./images.sh)

#: The build of the documentation which is derived from docstrings in the Python code
documentation.tex:
	(cd ../.. && $(PYTHON) test.py -o && $(PYTHON) test.py -t)

#: Same as
latex: mathics.pdf

#: Remove all auto-generated files
clean:
	rm -f mathics.asy mathics.aux mathics.idx mathics.log mathics.mtc mathics.mtc* mathics.out mathics.toc
	rm -f mathics.fdb_latexmk mathics.ilg mathics.ind mathics.maf mathics.pre
	rm -f mathics_*.*
	rm -f mathics-*.*
	rm -f mathics.pdf
