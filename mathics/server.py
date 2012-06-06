# -*- coding: utf8 -*-

u"""
    Mathics: a general-purpose computer algebra system
    Copyright (C) 2011 Jan Pöschko

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
from optparse import OptionParser

import django

from mathics import print_version, print_license

def main():
    os.environ['DJANGO_SETTINGS_MODULE'] = 'mathics.settings'
    #os.putenv('DJANGO_SETTINGS_MODULE', 'mathics.settings')
    
    from django.conf import settings
    from django.core.servers.basehttp import run, WSGIServerException
    from django.core.handlers.wsgi import WSGIHandler
    
    parser = OptionParser(version='%prog ' + settings.VERSION,
        description="Mathics server for the graphical user interface in Firefox. It is not intended for production use on a public Web server!")
    parser.add_option("-p", "--port", dest="port", metavar="PORT", default=8000, type='int',
        help="use PORT as server port")
    parser.add_option("-e", "--external", dest="external", action="store_true",
        help="allow external access to server")
    options, args = parser.parse_args()
            
    print_version(is_server=True)
    print_license()
    quit_command = 'CTRL-BREAK' if sys.platform == 'win32' else 'CONTROL-C'
    print u"Quit by pressing %s\n" % quit_command
    
    port = options.port
    print u"Open the graphical user interface at\nhttp://localhost:%d\nin Firefox, Chrome, or Safari to use Mathics\n" % port
    
    if options.external:
        addr = '0.0.0.0'
    else:
        addr = ''
    
    try:
        if settings.DJANGO_VERSION < (1, 4):
            from django.core.servers.basehttp import AdminMediaHandler
            handler = AdminMediaHandler(WSGIHandler(), '')
        else:
            from django.core.servers.basehttp import get_internal_wsgi_application
            handler = get_internal_wsgi_application()
        run(addr, port, handler)
    except WSGIServerException, e:
        # Use helpful error messages instead of ugly tracebacks.
        ERRORS = {
            13: "You don't have permission to access that port.",
            98: "That port is already in use.",
            99: "That IP address can't be assigned to.",
        }
        try:
            error_text = ERRORS[e.args[0].args[0]]
        except (AttributeError, KeyError):
            error_text = str(e)
        sys.stderr.write("Error: %s" % error_text + '\n')
        # Need to use an OS exit because sys.exit doesn't work in a thread
        os._exit(1)
    except KeyboardInterrupt:
        print "\nGoodbye!\n"
        sys.exit(0)

if __name__ == '__main__':
    main()