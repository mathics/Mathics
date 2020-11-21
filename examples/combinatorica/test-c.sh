#!/bin/bash
for c_test in c-test.m; do
    date
    echo "**** $c_test starting.. ****"
    logfile=/tmp/${c_test}$$.log
    mathics -e "<<$c_test" >$logfile
    rc=$?
    if ((rc != 0)) ; then
	echo "**** $c_test failed; $rc errors. ****"
	grep -B 5 'right?, False}' $logfile
	exit $rc
    fi
    echo "**** $c_test succeeded. ****"
done
echo '**** All Combinatorica tests succeeded. ****'
exit 0
