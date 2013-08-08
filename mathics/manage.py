#!/usr/bin/env python

from django.core.management import execute_manager
try:
    import settings  # Assumed to be in the same directory.
except ImportError:
    import sys
    sys.stderr.write(
        """Error: Can't find the file 'settings.py' in the directory containing
%r. It appears you've customized things.\n
You'll have to run django-admin.py, passing it your settings module.
(If the file settings.py does indeed exist, it's causing an ImportError.)\n"""
        % __file__)
    sys.exit(1)

if __name__ == "__main__":
    execute_manager(settings)

    # fix known PyPy bug (see https://bugs.pypy.org/issue1116)
    import gc
    gc.collect()
    gc.collect()
