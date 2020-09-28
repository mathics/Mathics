#!/bin/bash
for filename in $(find documentation/images/ -name "*.eps"); do
	pdf="$(dirname "$filename")/$(basename "$filename" .eps).pdf"
	png="$(dirname "$filename")/$(basename "$filename" .eps).png"
	epstopdf "$filename"
	inkscape -D -z --export-width=400 --export-png="$png" "$pdf"
	mv "$png" "../web/media/doc/"
	cp "$pdf" "../web/media/doc/"
	mkdir -p "tex/images"
	mv "$pdf" "tex/images/"
done

for filename in ../web/media/img/logo-{heptatom,text-nodrop}.svg; do
    inkscape $filename --export-pdf="tex/$(basename "$filename" .svg).pdf"
done
