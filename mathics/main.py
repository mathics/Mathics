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

import os
import sys
import argparse

# Try importing readline to enable arrow keys support etc.
try:
    import readline
except ImportError:
    pass

from mathics.core.definitions import Definitions
from mathics.core.expression import Symbol, Expression
from mathics.core.evaluation import Evaluation
from mathics import settings
from mathics import print_version, print_license

def to_output(text):
    return '\n . '.join(text.splitlines())

def main():
    argparser = argparse.ArgumentParser(
        prog='mathics',
        usage='%(prog)s [options] [FILE]',
        description = "Mathics is a general-purpose computer algebra system.",
        epilog = """Please feel encouraged to contribute to Mathics! Create
            your own fork, make the desired changes, commit, and make a pull 
            request.""")
    argparser.add_argument('FILE',  nargs='?', type=argparse.FileType('r'), help='Execute commands from FILE.')
    argparser.add_argument('-q', '--quiet',  help='don\'t print message at startup.', action='store_true')
    args = argparser.parse_args()

    quit_command = (sys.platform == 'win32') and 'CTRL-BREAK' or 'CONTROL-D'
    
    if not args.quiet:
        print_version(is_server=False)
        print_license()
        print u"Quit by pressing %s" % quit_command
    
        print ''
    
    definitions = Definitions(add_builtin=True)

    if args.FILE is not None:
        for line in args.FILE:
            print '>> ', line,

            def out_callback(out):
                    print to_output(unicode(out))

            evaluation = Evaluation(line, definitions, timeout=30, out_callback=out_callback)
            for result in evaluation.results:
                    if result.result is not None:
                        print ' = %s' % to_output(unicode(result.result))           
        return
    
    try:
        while True:
            try: 
                input = raw_input('>> ')
            
                def out_callback(out):
                    print to_output(unicode(out))
                
                evaluation = Evaluation(input, definitions, timeout=30, out_callback=out_callback)
            
                for result in evaluation.results:
                    if result.result is not None:
                        print ' = %s' % to_output(unicode(result.result))
            except (KeyboardInterrupt):
                print '\nKeyboardInterrupt'

    except (SystemExit, EOFError):
        print "\n\nGood bye!\n"

if __name__ == '__main__':
    main()
