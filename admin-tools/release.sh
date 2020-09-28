#!/usr/bin/env bash

# Before releasing:
# In root directory
# Clear directory build/
# python setup.py develop
# mathics/
#   python test.py -o
#   python test.py -t
# mathics/doc/tex/
#   make latex
# Then run this file.

admin_dir=$(dirname ${BASH_SOURCE[0]})
cd $(dirname ${BASH_SOURCE[0]})
owd=$(pwd)
cd ..
version=`python -c "import mathics; print(mathics.__version__)"`
echo "Releasing Mathics $version"

rm -rf build/release
python setup.py build
mkdir build/release
rm build/lib/mathics/doc/xml/data # data file is big so make sure we don't package it
cp -r build/lib*/mathics build/release/
rm build/release/*/*/*.so
cp setup.py AUTHORS.txt CHANGES.rst COPYING.txt README.rst build/release/
cp mathics/doc/tex/mathics.pdf build/release/

zipfilename="mathics-$version.zip"
tgzfilename="mathics-$version.tgz"
releasedir="mathics-$version"
rm -f "build/$zipfilename"
rm -f "build/$tgzfilename"
cd build/
cp -r release/ $releasedir/
echo "Creating ZIP file $zipfilename"
zip -r $zipfilename $releasedir/
echo "Creating TGZ file $tgzfilename"
tar czf $tgzfilename $releasedir/
cd ..

mkdir -p Homepage/release
cp "build/$zipfilename" Homepage/release/

echo "Done"
