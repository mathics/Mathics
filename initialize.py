"""
Creates the database used by Django
"""

import os
from os import path
import subprocess

from mathics import settings

def setup():
    print "Creating data directory %s" % settings.DATA_DIR
    if not path.exists(settings.DATA_DIR):
        os.makedirs(settings.DATA_DIR)
    print "Creating database %s" % settings.DATABASE_NAME
    subprocess.call(['python', 'mathics/manage.py', 'syncdb', '--noinput'])
    os.chmod(settings.DATABASE_NAME, 0766)
    print ""
    print "Mathics initialized successfully."

if __name__ == '__main__':
    setup()
