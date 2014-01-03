#!/usr/bin/env python

import sys

sys.stderr.write(
    """Since Mathics 0.5, the functionality of initialize.py has been moved
into setup.py. Please run:

  {python} setup.py initialize

to initialize the database.
""".format(python=sys.executable or 'python'))
sys.exit(1)
