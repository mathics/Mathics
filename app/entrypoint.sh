#!/bin/bash

script_cmd="${ENTRYPOINT_COMMAND:-$(basename $0)}"

function help() {
    cat <<EOF

Usage:
    $script_cmd [arg] [-- params, ...]

Arg:

    -h | --help               Print this help and exit

    --pythonpath              This will be added to PYTHONPATH. Example use-case - absolute path to
                              basedir of mathics source code inside container, ie:
                                 mathics src = /usr/src/app/mathics
                                 --pythonpaht /usr/src/app

    -m | --mode {cli|ui}      Start mathics in web-ui mode (ui), or in cli mode (cli). Default is cli.
                              See: https://github.com/mathics/Mathics/wiki/Installing#running-mathics

Params:

    Everything passed after '--' will be passed to mathics as is.

EOF
}

mathics_mode=cli

while (( $# )) ; do
    case "$1" in
        -h | --help)  help ; exit ;;
        -m | --mode)  mathics_mode="$2" ; shift 2 ;;
        --pythonpath) export PYTHONPATH="$2":$PYTHONPATH ; shift 2 ;;
        --)           shift ; break ;;
        *)            echo "Can't parse '$@'. See '$0 --help'" ; exit 1 ;;
    esac
done

echo
echo "~~~~ app/data has been mounted to $MATHICS_HOME/data ~~~~"
echo "$ ls $MATHICS_HOME/data"
ls -p $MATHICS_HOME/data
echo "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
echo

case $mathics_mode in
    cli) tmathics $@ ;;
    ui)  mathicsserver -e $@ ;;
    *)   echo "unknown mathics_mode=$mathics_mode. See '$script_cmd --help'" ; exit 2 ;;
esac
