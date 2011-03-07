TEX = /usr/texbin/pdflatex
ASY = asy
MAKEINDEX = /usr/texbin/makeindex

mathics.pdf: mathics.tex documentation.tex
	python prepare_latex.py
	$(TEX) mathics.tex
	$(ASY) mathics.asy
	$(TEX) mathics.tex
	$(TEX) mathics.tex
	$(MAKEINDEX) mathics.tex
	$(TEX) mathics.tex
	$(TEX) mathics.tex
	
latex: mathics.pdf