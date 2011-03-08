"""
Creates the database used by Django
"""

import os
import subprocess

from mathics import settings

def setup():
    print "Creating database %s" % settings.DATABASE_NAME
    subprocess.call(['python', 'mathics/manage.py', 'syncdb', '--noinput'])
    os.chmod(settings.DATABASE_NAME, 0766)

if __name__ == '__main__':
    setup()
