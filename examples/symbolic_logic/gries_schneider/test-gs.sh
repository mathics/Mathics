#!/bin/bash
for gs_test in GS[1-3].m; do
    date
    echo "**** $gs_test starting.. ****"
    logfile=/tmp/${gs_test}$$.log
    mathics -e "<<$gs_test" >$logfile
    rc=$?
    if ((rc != 0)) ; then
	echo "**** $gs_test failed; $rc errors. ****"
	grep -B 5 'right?, False}' $logfile
	exit $rc
    fi
    echo "**** $gs_test succeeded. ****"
done
echo '**** All Gries and Schneider tests succeeded. ****'
exit 0
