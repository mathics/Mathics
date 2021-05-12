#!/bin/bash
function finish {
  cd $owd
}

# FIXME put some of the below in a common routine
owd=$(pwd)
trap finish EXIT

cd $(dirname ${BASH_SOURCE[0]})
if ! source ./pyenv-versions ; then
    exit $?
fi

for version in $PYVERSIONS; do
    echo --- $version ---
    if ! pyenv local $version ; then
	exit $?
    fi
    make clean && pip install -e .
    if ! make check; then
	exit $?
    fi
    echo === $version ===
done
