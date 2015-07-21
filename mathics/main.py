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
import re
import locale

from mathics.core.definitions import Definitions
from mathics.core.expression import Integer, strip_context
from mathics.core.evaluation import Evaluation
from mathics import print_version, print_license, get_version_string
from mathics import settings


class TerminalShell(object):
    def __init__(self, definitions, colors, want_readline, want_completion):
        self.input_encoding = locale.getpreferredencoding()

        # Try importing readline to enable arrow keys support etc.
        self.using_readline = False
        try:
            if want_readline:
                import readline
                self.using_readline = sys.stdin.isatty() and sys.stdout.isatty()
                self.ansi_color_re = re.compile("\033\\[[0-9;]+m")
                if want_completion:
                    readline.set_completer(
                        lambda text, state:
                            self.complete_symbol_name(text, state))
                    readline.set_completer_delims(
                        # Make _ a delimiter, but not $ or `
                        ' \t\n_~!@#%^&*()-=+[{]}\\|;:\'",<>/?')
                    readline.parse_and_bind("tab: complete")
                    self.completion_candidates = []
        except ImportError:
            pass

        # Try importing colorama to escape ansi sequences for cross platform
        # colors
        try:
            from colorama import init as colorama_init
        except ImportError:
            colors = 'NoColor'
        else:
            colorama_init()
            if colors is None:
                terminal_supports_color = (sys.stdout.isatty() and
                                           os.getenv('TERM') != 'dumb')
                colors = 'Linux' if terminal_supports_color else 'NoColor'

        color_schemes = {
            'NOCOLOR': (
                ['', '', '', ''],
                ['', '', '', '']),
            'LINUX': (
                ['\033[32m', '\033[1m', '\033[22m', '\033[39m'],
                ['\033[31m', '\033[1m', '\033[22m', '\033[39m']),
            'LIGHTBG': (
                ['\033[34m', '\033[1m', '\033[22m', '\033[39m'],
                ['\033[31m', '\033[1m', '\033[22m', '\033[39m']),
        }

        # Handle any case by using .upper()
        term_colors = color_schemes.get(colors.upper())
        if term_colors is None:
            out_msg = "The 'colors' argument must be {0} or None"
            print out_msg.format(repr(color_schemes.keys()))
            quit()

        self.incolors, self.outcolors = term_colors
        self.definitions = definitions

    def get_last_line_number(self):
        line = self.definitions.get_definition('$Line').ownvalues[0].replace
        return line.get_int_value()

    def get_in_prompt(self, continued=False):
        next_line_number = self.get_last_line_number() + 1
        if continued:
            return ' ' * len('In[{0}]:= '.format(next_line_number))
        else:
            return '{1}In[{2}{0}{3}]:= {4}'.format(next_line_number,
                                                   *self.incolors)

    def get_out_prompt(self):
        line_number = self.get_last_line_number()
        return '{1}Out[{2}{0}{3}]= {4}'.format(line_number, *self.outcolors)

    def evaluate(self, text):
        def to_output(text):
            line_number = self.get_last_line_number()
            newline = '\n' + ' ' * len('Out[{0}]= '.format(line_number))
            return newline.join(text.splitlines())

        def out_callback(out):
            print to_output(unicode(out))

        evaluation = Evaluation(text,
                                self.definitions,
                                timeout=settings.TIMEOUT,
                                out_callback=out_callback)
        for result in evaluation.results:
            if result.result is not None:
                print(self.get_out_prompt() +
                      to_output(unicode(result.result)) + '\n')

    def read_line(self, prompt):
        if self.using_readline:
            return self.rl_read_line(prompt)
        return raw_input(prompt)

    def rl_read_line(self, prompt):
        # sys.stdout is wrapped by a codecs.StreamWriter object in
        # mathics/__init__.py, which interferes with raw_input's use
        # of readline.
        #
        # To work around this issue, call raw_input with the original
        # file object as sys.stdout, which is in the undocumented
        # 'stream' field of codecs.StreamWriter.
        orig_stdout = sys.stdout
        try:
            # Wrap ANSI colour sequences in \001 and \002, so readline
            # knows that they're nonprinting.
            prompt = self.ansi_color_re.sub(
                lambda m: "\001" + m.group(0) + "\002", prompt)
            sys.stdout = sys.stdout.stream
            ret = raw_input(prompt)
            return ret
        finally:
            sys.stdout = orig_stdout

    def complete_symbol_name(self, text, state):
        try:
            return self._complete_symbol_name(text, state)
        except Exception:
            # any exception thrown inside the completer gets silently
            # thrown away otherwise
            print "Unhandled error in readline completion"

    def _complete_symbol_name(self, text, state):
        # The readline module calls this function repeatedly,
        # increasing 'state' each time and expecting one string to be
        # returned per call.

        if state == 0:
            self.completion_candidates = self.get_completion_candidates(text)

        try:
            return self.completion_candidates[state]
        except IndexError:
            return None

    def get_completion_candidates(self, text):
        matches = self.definitions.get_matching_names(text + '*')
        if '`' not in text:
            matches = [strip_context(m) for m in matches]
        return matches


# Adapted from code at http://mydezigns.wordpress.com/2009/09/22/balanced-brackets-in-python/       # nopep8


def wait_for_line(input_string):
    """
    Should the intepreter wait for another line of input or try to evaluate the
    current line as is.
    """
    trailing_ops = ['+', '-', '/', '*', '^', '=',
                    '>', '<', '/;', '/:', '/.', '&&', '||']
    if any(input_string.rstrip().endswith(op) for op in trailing_ops):
        return True

    brackets = [('(', ')'), ('[', ']'), ('{', '}')]
    kStart, kEnd, stack = 0, 1, []
    in_string = False
    for char in input_string:
        if char == '"':
            in_string = not in_string
        if not in_string:
            for bracketPair in brackets:
                if char == bracketPair[kStart]:
                    stack.append(char)
                elif char == bracketPair[kEnd]:
                    if len(stack) == 0:
                        return False
                    if stack.pop() != bracketPair[kStart]:
                        # Brackets are not balanced, but return False so that a
                        # parse error can be raised
                        return False
    if len(stack) == 0 and not in_string:
        return False
    return True


def main():
    argparser = argparse.ArgumentParser(
        prog='mathics',
        usage='%(prog)s [options] [FILE]',
        add_help=False,
        description="Mathics is a general-purpose computer algebra system.",
        epilog="""Please feel encouraged to contribute to Mathics! Create
            your own fork, make the desired changes, commit, and make a pull
            request.""")

    argparser.add_argument(
        'FILE', nargs='?', type=argparse.FileType('r'),
        help='execute commands from FILE')

    argparser.add_argument(
        '--help', '-h', help='show this help message and exit', action='help')

    argparser.add_argument(
        '--persist', help='go to interactive shell after evaluating FILE or -e',
        action='store_true')

    argparser.add_argument(
        '--quiet', '-q', help='don\'t print message at startup',
        action='store_true')

    argparser.add_argument(
        '-script', help='run a mathics file in script mode',
        action='store_true')

    argparser.add_argument(
        '--execute', '-e', action='append', metavar='EXPR',
        help='evaluate EXPR before processing any input files (may be given '
        'multiple times)')

    argparser.add_argument(
        '--colors', nargs='?', help='interactive shell colors')

    argparser.add_argument(
        '--no-completion', help="disable tab completion", action='store_true')

    argparser.add_argument(
        '--no-readline', help="disable line editing (implies --no-completion)",
        action='store_true')

    argparser.add_argument(
        '--version', '-v', action='version', version=get_version_string(False))

    args = argparser.parse_args()

    quit_command = 'CTRL-BREAK' if sys.platform == 'win32' else 'CONTROL-D'

    definitions = Definitions(add_builtin=True)
    definitions.set_ownvalue('$Line', Integer(0))  # Reset the line number

    shell = TerminalShell(
        definitions, args.colors, want_readline=not(args.no_readline),
        want_completion=not(args.no_completion))

    if not (args.quiet or args.script):
        print_version(is_server=False)
        print_license()
        print u"Quit by pressing {0}\n".format(quit_command)

    if args.execute:
        for expr in args.execute:
            total_input = expr.decode(shell.input_encoding)
            print shell.get_in_prompt() + total_input
            shell.evaluate(total_input)
        if not args.persist:
            return

    if args.FILE is not None:
        total_input = ''
        for line_no, line in enumerate(args.FILE):
            try:
                line = line.decode('utf-8')     # TODO: other encodings
                if args.script and line_no == 0 and line.startswith('#!'):
                    continue
                print shell.get_in_prompt(continued=total_input != '') + line,
                total_input += ' ' + line
                if line != "" and wait_for_line(total_input):
                    continue
                shell.evaluate(total_input)
                total_input = ""
            except (KeyboardInterrupt):
                print '\nKeyboardInterrupt'
            except (SystemExit, EOFError):
                print "\n\nGood bye!\n"
                break
        if not args.persist:
            return

    total_input = ""
    while True:
        try:
            line = shell.read_line(
                shell.get_in_prompt(continued=total_input != ''))
            line = line.decode(shell.input_encoding)
            total_input += line
            if line != "" and wait_for_line(total_input):
                continue
            shell.evaluate(total_input)
            total_input = ""
        except (KeyboardInterrupt):
            print '\nKeyboardInterrupt'
        except (SystemExit, EOFError):
            print "\n\nGood bye!\n"
            break

if __name__ == '__main__':
    main()
