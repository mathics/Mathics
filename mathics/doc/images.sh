#!/bin/bash
# The program create PDF images that can be imbedded into the
# Mathics manual. In particular the Mathics heptatom logo and the
# Mathics logo with a showdow that extends a little bit down forward right.
mkdir -p "tex/images"

for filename in $(find documentation/images/ -name "*.eps"); do
	pdf="$(dirname "$filename")/$(basename "$filename" .eps).pdf"
	epstopdf "$filename"
	mv "$pdf" "tex/images/"
done

for filename in images/logo-{heptatom,text-nodrop}.svg; do
    inkscape $filename --export-filename="tex/$(basename "$filename" .svg).pdf" --batch-process
done
