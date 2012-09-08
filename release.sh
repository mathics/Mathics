#!/usr/bin/env bash

# Before releasing:
# Clear directory build/
# python setup.py install
# sage -python setup.py install
# sage -python setup.py develop
# mathics/
#   sage -python test.py
#   sage -python test.py -t
# mathics/doc/tex/
#   make latex
# Then run this file.

versionline=`grep -E "VERSION = '(.+)'" mathics/settings.py`
version=`expr "$versionline" : 'VERSION = .\([0-9.a-z]*\)'`
echo "Releasing Mathics $version"

rm -rf build/release
python setup.py build
mkdir build/release
cp -r build/lib*/mathics build/release/
rm build/release/*/*/*.so
cp setup.py initialize.py distribute_setup.py build/release/
cp install_sage_scripts.py build/release/
cp AUTHORS.txt CHANGES.rst COPYING.txt README.rst build/release/
cp mathics/doc/tex/mathics.pdf build/release/

zipfilename="mathics-$version.zip"
rm -f "build/$zipfilename"
cd build/release
echo "Creating ZIP file $zipfilename"
zip -r "../$zipfilename" .
cd ../..

cp "build/$zipfilename" Homepage/release/

echo "Done"