#!/usr/bin/env python2
"""
Creates the database used by Django
"""

import os
from os import path
import subprocess
from sys import executable

from mathics import settings

def setup():
    database_file = settings.DATABASES['default']['NAME']
    print "Creating data directory %s" % settings.DATA_DIR
    if not path.exists(settings.DATA_DIR):
        os.makedirs(settings.DATA_DIR)
    print "Creating database %s" % database_file
    dn = os.path.dirname(os.path.realpath(__file__))
    subprocess.call(executable, [dn+'/mathics/manage.py', 'syncdb', '--noinput'])
    os.chmod(database_file, 0766)
    print ""
    print "Mathics initialized successfully."

if __name__ == '__main__':
    setup()
