"""
Creates the database used by Django
"""

import os
from os import path
import subprocess

from mathics import settings

def setup():
    database_file = settings.DATABASES['default']['NAME']
    print "Creating data directory %s" % settings.DATA_DIR
    if not path.exists(settings.DATA_DIR):
        os.makedirs(settings.DATA_DIR)
    print "Creating database %s" % database_file
    subprocess.call(['python', 'mathics/manage.py', 'syncdb', '--noinput'])
    os.chmod(database_file, 0766)
    print ""
    print "Mathics initialized successfully."

if __name__ == '__main__':
    setup()
