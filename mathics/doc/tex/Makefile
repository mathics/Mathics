
TEX = xelatex
LATEXMK = latexmk -f 
#-quiet

mathics.pdf: mathics.tex documentation.tex
	$(LATEXMK) -pdf -pdflatex=$(TEX) mathics

latex: mathics.pdf

clean:
	rm -f mathics.asy mathics.aux mathics.idx mathics.log mathics.mtc mathics.mtc* mathics.out mathics.toc
	rm -f mathics.fdb_latexmk mathics.ilg mathics.ind mathics.maf mathics.pre
	rm -f mathics_*.*
	rm -f mathics-*.*
	rm -f mathics.pdf
