# -*- coding: utf8 -*-

u"""
    Mathics: a general-purpose computer algebra system
    Copyright (C) 2011-2013 The Mathics Team

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
"""

import sys
import os
import argparse
import socket
import errno

import mathics
from mathics import print_version, print_license
from mathics import settings as mathics_settings  # Prevents UnboundLocalError


def main():
    # Check for the database
    database_file = mathics_settings.DATABASES['default']['NAME']
    if not os.path.exists(database_file):
        print("warning: database file %s not found\n" % database_file)
        import subprocess
        if not os.path.exists(mathics_settings.DATA_DIR):
            print("Creating data directory %s" % mathics_settings.DATA_DIR)
            os.makedirs(mathics_settings.DATA_DIR)
        print("Creating database %s" % database_file)
        manage_file = os.path.join(
            os.path.dirname(os.path.realpath(__file__)), "manage.py")
        try:
            subprocess.check_call(
                [sys.executable, manage_file, 'syncdb', '--noinput'])
            print("\ndatabase initialized sucessfully")
        except subprocess.CalledProcessError:
            print("error: failed to create database")
            sys.exit(1)

    os.environ['DJANGO_SETTINGS_MODULE'] = 'mathics.settings'
    # os.putenv('DJANGO_SETTINGS_MODULE', 'mathics.settings')

    from django.conf import settings
    from django.core.servers.basehttp import run
    from django.core.handlers.wsgi import WSGIHandler

    argparser = argparse.ArgumentParser(
        prog='mathicsserver',
        usage='%(prog)s [options]',
        add_help=False,
        description="""Mathics server for the graphical user interface in a
            web browser. It is not intended for production use on a public Web
            server!""",
        epilog="""Please feel encouraged to contribute to Mathics! Create
            your own fork, make the desired changes, commit, and make a pull
            request.""")

    argparser.add_argument(
        '--help', '-h', help='show this help message and exit', action='help')
    argparser.add_argument(
        '--quiet', '-q', help='don\'t print message at startup',
        action='store_true')
    argparser.add_argument(
        '--version', '-v', action='version',
        version='%(prog)s ' + mathics.__version__)
    argparser.add_argument(
        "--port", "-p", dest="port", metavar="PORT", default=8000, type=int,
        help="use PORT as server port")
    argparser.add_argument(
        "--external", "-e", dest="external", action="store_true",
        help="allow external access to server")

    args = argparser.parse_args()

    quit_command = 'CTRL-BREAK' if sys.platform == 'win32' else 'CONTROL-C'
    port = args.port

    if not args.quiet:
        print_version(is_server=True)
        print_license()
        print u"Quit by pressing %s\n" % quit_command

        print u"""Open the graphical user interface at
http://localhost:%d\nin Firefox, Chrome, or Safari to use Mathics\n""" % port

    if args.external:
        addr = '0.0.0.0'
    else:
        addr = '127.0.0.1'

    try:
        if settings.DJANGO_VERSION < (1, 4):
            from django.core.servers.basehttp import AdminMediaHandler
            handler = AdminMediaHandler(WSGIHandler(), '')
        else:
            from django.core.servers.basehttp import (
                get_internal_wsgi_application)
            handler = get_internal_wsgi_application()
        run(addr, port, handler)
    except socket.error as e:
        # Use helpful error messages instead of ugly tracebacks.
        ERRORS = {
            errno.EACCES: "You don't have permission to access that port.",
            errno.EADDRINUSE: "That port is already in use.",
            errno.EADDRNOTAVAIL: "That IP address can't be assigned to.",
        }
        try:
            error_text = ERRORS[e.errno]
        except KeyError:
            error_text = str(e)
        sys.stderr.write("Error: %s" % error_text + '\n')
        # Need to use an OS exit because sys.exit doesn't work in a thread
        os._exit(1)
    except KeyboardInterrupt:
        print "\nGoodbye!\n"
        sys.exit(0)

if __name__ == '__main__':
    main()
