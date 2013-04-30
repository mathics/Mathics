"""
Sets up scripts to run mathics and mathicsserver when installed into Sage.
"""

from __future__ import with_statement

import os
from os import path
from argparse import ArgumentParser

from mathics import settings


def setup():
    parser = ArgumentParser(
        description="Sets up scripts to run mathics and mathicsserver when installed into Sage.")
    parser.add_argument(
        "-d", "--directory", dest="dir", metavar="DIR", default='/usr/local/bin',
        help="install scripts into directory DIR")
    args = parser.parse_args()

    for cmd in ['mathics', 'mathicsserver']:
        filename = path.join(args.dir, cmd)
        with open(filename, 'w') as file:
            file.write("""#!/usr/bin/env bash
sage -python -c "__requires__ = 'Mathics==%(version)s'; import sys; from pkg_resources import load_entry_point; sys.exit(load_entry_point('Mathics==%(version)s', 'console_scripts', '%(cmd)s')())" "$@"
""" % {
                       'version': settings.VERSION,
                       'cmd': cmd,
                       })
        os.chmod(filename, 0755)
                 # make file readable and executable by everyone, writeable by
                 # owner

if __name__ == '__main__':
    setup()
