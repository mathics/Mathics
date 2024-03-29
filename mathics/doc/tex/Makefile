.PHONY: all clean doc doc-data latex texdoc

PYTHON ?= python
XETEX ?= xelatex
LATEXMK ?= latexmk
BASH ?= /bin/bash
#-quiet

DOC_TEX_DATA_PCL ?= $(HOME)/.local/var/mathics/doc_tex_data.pcl

#: Default target: Make everything
all doc texdoc: mathics.pdf

#: Create internal Document Data from .mdoc and Python builtin module docstrings
doc-data $(DOC_TEX_DATA_PCL):
	(cd ../.. && $(PYTHON) docpipeline.py --output --keep-going)

#: Build mathics PDF
mathics.pdf: mathics.tex documentation.tex logo-text-nodrop.pdf logo-heptatom.pdf version-info.tex $(DOC_TEX_DATA_PCL)
	$(LATEXMK) --verbose -f -pdf -pdflatex="$(XETEX) -halt-on-error" mathics

#: File containing version information
version-info.tex: doc2latex.py
	$(PYTHON) doc2latex.py

#: Build test PDF
mathics-test.pdf: mathics-test.tex testing.tex
	$(LATEXMK) --verbose -f -pdf -pdflatex="$(XETEX) -halt-on-error" mathics-test


#: Generate logos used in the titlepage
logo-heptatom.pdf logo-text-nodrop.pdf:
	(cd .. && $(BASH) ./images.sh)

#: The build of the documentation which is derived from docstrings in the Python code
documentation.tex: $(DOC_TEX_DATA_PCL)
	$(PYTHON) ./doc2latex.py

#: Same as mathics.pdf
latex: mathics.pdf

#: Remove all auto-generated files
clean:
	rm -f mathics.asy mathics.aux mathics.idx mathics.log mathics.mtc mathics.mtc* mathics.out mathics.toc || true
	rm -f test-mathics.aux test-mathics.idx test-mathics.log test-mathics.mtc test-mathics.mtc* test-mathics.out test-mathics.toc || true
	rm -f mathics.fdb_latexmk mathics.ilg mathics.ind mathics.maf mathics.pre || true
	rm -f mathics_*.* || true
	rm -f mathics-*.* documentation.tex $(DOC_TEX_DATA_PCL) || true
	rm -f mathics.pdf mathics.dvi test-mathics.pdf test-mathics.dvi || true
	rm -f mathics-test.pdf mathics-test.dvi version-info.tex || true
