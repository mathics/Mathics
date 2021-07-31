.PHONY: all clean doc doc-data latex texdoc

PYTHON ?= python
XETEX ?= xelatex
LATEXMK ?= latexmk
BASH ?= /bin/bash
#-quiet

DOC_DATA_PCL ?= $(HOME)/.local/var/mathics/doc_data.pcl

#: Default target: Make everything
all doc texdoc: mathics.pdf

doc-data $(DOC_DATA_PCL):
	(cd ../.. && $(PYTHON) docpipeline.py --output --keep-going)

#: Build mathics PDF
mathics.pdf: mathics.tex documentation.tex logo-text-nodrop.pdf logo-heptatom.pdf $(DOC_DATA_PCL)
	$(LATEXMK) --verbose -f -pdf -pdflatex="$(XETEX) -halt-on-error" mathics

#: Build test PDF
mathics-test.pdf: mathics-test.tex testing.tex
	$(LATEXMK) --verbose -f -pdf -pdflatex="$(XETEX) -halt-on-error" mathics-test


#: Generate logos used in the titlepage
logo-heptatom.pdf logo-text-nodrop.pdf:
	(cd .. && $(BASH) ./images.sh)

#: The build of the documentation which is derived from docstrings in the Python code
documentation.tex: $(DOC_DATA_PCL)
	$(PYTHON) ./doc2latex.py

#: Same as mathics.pdf
latex: mathics.pdf

#: Remove all auto-generated files
clean:
	rm -f mathics.asy mathics.aux mathics.idx mathics.log mathics.mtc mathics.mtc* mathics.out mathics.toc || true
	rm -f test-mathics.aux test-mathics.idx test-mathics.log test-mathics.mtc test-mathics.mtc* test-mathics.out test-mathics.toc || true
	rm -f mathics.fdb_latexmk mathics.ilg mathics.ind mathics.maf mathics.pre || true
	rm -f mathics_*.* || true
	rm -f mathics-*.* documentation.tex doc_data.pcl || true
	rm -f mathics.pdf mathics.dvi data_tex_data.pcl test-mathics.pdf test-mathics.dvi || true
	rm -f mathics-test.pdf mathics-test.dvi || true
