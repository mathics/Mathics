#!/bin/sh
mkdir -p "tex/images"

for filename in $(find documentation/images/ -name "*.eps"); do
	pdf="$(dirname "$filename")/$(basename "$filename" .eps).pdf"
	epstopdf "$filename"
	mv "$pdf" "tex/images/"
done

for filename in images/logo-{heptatom,text-nodrop}.svg; do
    inkscape $filename --export-pdf="tex/$(basename "$filename" .svg).pdf"
done
