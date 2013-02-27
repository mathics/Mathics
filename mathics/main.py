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
from mathics import print_version, print_license, get_version_string

def to_output(text):
    return '\n . '.join(text.splitlines())

def out_callback(out):
    print to_output(unicode(out))

# Adapted from code at http://mydezigns.wordpress.com/2009/09/22/balanced-brackets-in-python/
def brackets_balanced(input_string):
    brackets = [ ('(',')'), ('[',']'), ('{','}')]
    kStart, kEnd, stack = 0, 1, []
    for char in input_string:
        for bracketPair in brackets:
            if char == bracketPair[kStart]:
                stack.append(char)
            elif char == bracketPair[kEnd] and (len(stack) == 0 or stack.pop() != bracketPair[kStart]):
                # Brackets are not balanced, but return True so that a parse error can be raised
                return True
    if len(stack) == 0:
        return True

    return False

def main():
    argparser = argparse.ArgumentParser(
        prog='mathics',
        usage='%(prog)s [options] [FILE]',
        add_help=False,
        description = "Mathics is a general-purpose computer algebra system.",
        epilog = """Please feel encouraged to contribute to Mathics! Create
            your own fork, make the desired changes, commit, and make a pull 
            request.""")

    argparser.add_argument('FILE',  nargs='?', type=argparse.FileType('r'), help='execute commands from FILE')

    argparser.add_argument('--help', '-h', help='show this help message and exit', action='help')
    argparser.add_argument('--persist',  help='go to interactive shell after evaluating FILE', action='store_true')
    argparser.add_argument('--quiet', '-q', help='don\'t print message at startup', action='store_true')
    argparser.add_argument('-script', help='run a mathics file in script mode', action='store_true')
    argparser.add_argument('--execute', '-e', nargs='?', help='execute a command')
    argparser.add_argument('--version', '-v', action='version', version=get_version_string(False))

    args = argparser.parse_args()

    quit_command = 'CTRL-BREAK' if sys.platform == 'win32' else 'CONTROL-D'
    
    definitions = Definitions(add_builtin=True)

    # TODO all binary operators?
    trailing_ops = ['+', '-', '/', '*', '^', '=', 
	    '>', '<', '/;', '/:', '/.', '&&', '||'] 

    if args.execute:
        print ">> %s" % args.execute
        evaluation = Evaluation(args.execute, definitions, timeout=30, out_callback=out_callback)
        for result in evaluation.results:
                if result.result is not None:
                    print ' = %s' % to_output(unicode(result.result))           
        return


    if not (args.quiet or args.script):
        print_version(is_server=False)
        print_license()
        print u"Quit by pressing %s" % quit_command
    
        print ''


    if args.FILE is not None:
        total_input = ""
        for line in args.FILE:

            if args.script and line.startswith('#!'):
                continue

            if total_input == "":
                print '>> ', line,
            else:
                print '       ', line,

            total_input += line

            if line == "":
                pass
            elif any(line.rstrip().endswith(op) for op in trailing_ops) or not brackets_balanced(total_input):
                continue

            evaluation = Evaluation(total_input, definitions, timeout=30, out_callback=out_callback)
            for result in evaluation.results:
                    if result.result is not None:
                        print ' = %s' % to_output(unicode(result.result))           
            total_input = ""
        if not args.persist:
            return

    while True:
        try: 
            total_input = ""
            line_input = raw_input('>> ')
            while line_input != "":
                total_input += ' ' + line_input
                if any([line_input.rstrip().endswith(op) for op in trailing_ops]):
                    pass
                elif brackets_balanced(total_input):
                    break
                line_input = raw_input('       ')
        
            evaluation = Evaluation(total_input, definitions, timeout=30, out_callback=out_callback)
        
            for result in evaluation.results:
                if result.result is not None:
                    print ' = %s' % to_output(unicode(result.result))
        except (KeyboardInterrupt):
            print '\nKeyboardInterrupt'
        except (SystemExit, EOFError):
            print "\n\nGood bye!\n"
            break

if __name__ == '__main__':
    main()
