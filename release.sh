versionline=`grep -E "VERSION = '(.+)'" mathics/settings.py`
version=`expr "$versionline" : 'VERSION = .\([0-9.a-z]*\)'`
echo "Releasing Mathics $version"

rm -rf build/release
python setup.py build
mkdir build/release
cp -r build/lib/mathics build/release/
cp setup.py build/release/
cp distribute_setup.py build/release/
cp install_sage_scripts.py build/release/

zipfilename="mathics-$version.zip"
rm -f "build/$zipfilename"
cd build/release
echo "Creating ZIP file $zipfilename"
zip -r "../$zipfilename" .
cd ../..

echo "Done"