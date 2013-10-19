for filename in $(find documentation/images/ -name "*.eps"); do
	pdf="$(dirname "$filename")/$(basename "$filename" .eps).pdf"
	png="$(dirname "$filename")/$(basename "$filename" .eps).png"
	epstopdf "$filename"
	convert -resize 400 "$filename" "$png"
	mv "$png" "../web/media/doc/"
	cp "$pdf" "../web/media/doc/"
	mkdir -p "tex/images"
	mv "$pdf" "tex/images/"
done
