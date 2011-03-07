images:
	for filename in $(find documentation/images/ -name "*.eps") do
	pdf="$(dirname '$filename')/$(basename '$filename' .eps).pdf"
	png="$(dirname '$filename')/$(basename '$filename' .eps).png"
	eps2pdf "$filename" "$pdf"
	convert -size 400 "$filename" "$png"
	done