#!/bin/bash
if [ ! -e config/password.txt ]; then
    echo "Please create or edit config/username.txt and config/password.txt!";
fi
# Here follows a disgusting hack; the classpath repeated twice, once ; separated, once : separated. As lame as it gets.
java -classpath "wikilink.jar:jars/commons-httpclient-3.0-rc2.jar:jars/commons-codec-1.3.jar:jars/commons-lang-2.1.jar:jars/commons-logging.jar:jars/jdom.jar:;wikilink.jar;jars/commons-httpclient-3.0-rc2.jar;jars/commons-codec-1.3.jar;jars/commons-lang-2.1.jar;jars/commons-logging.jar;jars/jdom.jar" wikilink.wikidelete `cat config/base-url.txt` `cat config/username.txt` `cat config/password.txt` $1 $2 $3 $4 $5 $6 $7 $8
