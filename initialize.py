#!/usr/bin/env python
"""
Creates the database used by Django
"""

import sys
import os
from os import path
import subprocess
from sys import executable


def setup():
    from mathics import settings
    database_file = settings.DATABASES['default']['NAME']
    print ("Creating data directory %s" % settings.DATA_DIR)
    if not path.exists(settings.DATA_DIR):
        os.makedirs(settings.DATA_DIR)
    print ("Creating database %s" % database_file)
    dn = os.path.dirname(os.path.realpath(__file__))
    subprocess.call([executable, dn+'/mathics/manage.py', 'syncdb', '--noinput'])
    os.chmod(database_file, 0o766)
    print ("")
    print ("Mathics initialized successfully.")

if __name__ == '__main__':
    if sys.version_info.major > (2):
        print ("python version higher than 2.7, try to launch with python2")
        subprocess.call(['/usr/bin/python2',  __file__ ])
        sys.exit()
    setup()
