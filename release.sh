#!/usr/bin/env bash

# Before releasing:
# Clear directory build/
# python setup.py develop
# mathics/
#   python test.py -o
#   python test.py -t
# mathics/doc/tex/
#   make latex
# Then run this file.

version=`python -c "import mathics; print(mathics.__version__)"`
echo "Releasing Mathics $version"

rm -rf build/release
python setup.py build
mkdir build/release
cp -r build/lib*/mathics build/release/
rm build/release/*/*/*.so
cp setup.py AUTHORS.txt CHANGES.rst COPYING.txt README.rst build/release/
cp mathics/doc/tex/mathics.pdf build/release/

zipfilename="mathics-$version.zip"
rm -f "build/$zipfilename"
cd build/release
echo "Creating ZIP file $zipfilename"
zip -r "../$zipfilename" .
cd ../..

mkdir -p Homepage/release
cp "build/$zipfilename" Homepage/release/

echo "Done"
