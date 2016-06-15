#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import unicode_literals
from __future__ import print_function
from __future__ import absolute_import

import sys
import os
import argparse
import re
import locale

from mathics.core.definitions import Definitions
from mathics.core.expression import Integer, strip_context
from mathics.core.evaluation import Evaluation
from mathics.core.parser import (
    parse, parse_lines, TranslateError, IncompleteSyntaxError, LineFeeder,
    FileLineFeeder, ExpressionGenerator)
from mathics import version_string, license_string, __version__
from mathics import settings

import six
from six.moves import input


class TerminalShell(LineFeeder):
    def __init__(self, definitions, colors, want_readline, want_completion):
        self.input_encoding = locale.getpreferredencoding()
        self.continued = False  # continued input on later lines

        # Try importing readline to enable arrow keys support etc.
        self.using_readline = False
        try:
            if want_readline:
                import readline
                self.using_readline = sys.stdin.isatty() and sys.stdout.isatty()
                self.ansi_color_re = re.compile("\033\\[[0-9;]+m")
                if want_completion:
                    readline.set_completer(lambda text, state: self.complete_symbol_name(text, state))

                    # Make _ a delimiter, but not $ or `
                    readline.set_completer_delims(' \t\n_~!@#%^&*()-=+[{]}\\|;:\'",<>/?')

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
                terminal_supports_color = (sys.stdout.isatty() and os.getenv('TERM') != 'dumb')
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
            print(out_msg.format(repr(list(color_schemes.keys()))))
            quit()

        self.incolors, self.outcolors = term_colors
        self.definitions = definitions

    def get_last_line_number(self):
        return self.definitions.get_line_no()

    def get_in_prompt(self):
        next_line_number = self.get_last_line_number() + 1
        if self.continued:
            return ' ' * len('In[{0}]:= '.format(next_line_number))
        else:
            return '{1}In[{2}{0}{3}]:= {4}'.format(next_line_number, *self.incolors)

    def get_out_prompt(self):
        line_number = self.get_last_line_number()
        return '{1}Out[{2}{0}{3}]= {4}'.format(line_number, *self.outcolors)

    def to_output(self, text):
        line_number = self.get_last_line_number()
        newline = '\n' + ' ' * len('Out[{0}]= '.format(line_number))
        return newline.join(text.splitlines())

    def out_callback(self, out):
        print(self.to_output(six.text_type(out)))

    def read_line(self, prompt):
        if self.using_readline:
            return self.rl_read_line(prompt)
        return input(prompt)

    def print_results(self, results):
        for result in results:
            if result.result is not None:
                output = self.to_output(six.text_type(result.result))
                print(self.get_out_prompt() + output + '\n')

    def rl_read_line(self, prompt):
        # Wrap ANSI colour sequences in \001 and \002, so readline
        # knows that they're nonprinting.
        prompt = self.ansi_color_re.sub(
            lambda m: "\001" + m.group(0) + "\002", prompt)

        # For Py2 sys.stdout is wrapped by a codecs.StreamWriter object in
        # mathics/__init__.py which interferes with raw_input's use of readline
        #
        # To work around this issue, call raw_input with the original
        # file object as sys.stdout, which is in the undocumented
        # 'stream' field of codecs.StreamWriter.
        if six.PY2:
            orig_stdout = sys.stdout
            try:
                sys.stdout = sys.stdout.stream
                ret = input(prompt).decode(self.input_encoding)
                return ret
            finally:
                sys.stdout = orig_stdout
        else:
            return input(prompt)

    def complete_symbol_name(self, text, state):
        try:
            return self._complete_symbol_name(text, state)
        except Exception:
            # any exception thrown inside the completer gets silently
            # thrown away otherwise
            print("Unhandled error in readline completion")

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

    def reset_continued(self):
        self.continued = False

    def feed(self):
        result = self.read_line(self.get_in_prompt()) + '\n'
        if result == '\n':
            return ''   # end of input
        self.continued = True
        return result

    def empty(self):
        return False


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
        '--version', '-v', action='version',
        version='%(prog)s ' + __version__)

    args = argparser.parse_args()

    quit_command = 'CTRL-BREAK' if sys.platform == 'win32' else 'CONTROL-D'

    definitions = Definitions(add_builtin=True)
    definitions.set_line_no(0)  # Reset the line number

    shell = TerminalShell(
        definitions, args.colors, want_readline=not(args.no_readline),
        want_completion=not(args.no_completion))

    if not (args.quiet or args.script):
        print()
        print(version_string + '\n')
        print(license_string + '\n')
        print("Quit by pressing {0}\n".format(quit_command))

    if args.execute:
        for expr in args.execute:
            print(shell.get_in_prompt() + expr)
            evaluation = Evaluation(shell.definitions, out_callback=shell.out_callback)
            exprs = evaluation.parse(expr)
            results = evaluation.evaluate(exprs, timeout=settings.TIMEOUT)
            shell.print_results(results)

        if not args.persist:
            return

    if args.FILE is not None:
        results = []
        query_gen = ExpressionGenerator(shell.definitions, FileLineFeeder(args.FILE))
        evaluation = Evaluation(shell.definitions, out_callback=shell.out_callback)
        try:
            for query in query_gen:
                results.extend(evaluation.evaluate([query], timeout=settings.TIMEOUT))
        except TranslateError as exc:
            evaluation.recursion_depth = 0
            evaluation.stopped = False
            evaluation.message('Syntax', exc.msg, *exc.args)
        except (KeyboardInterrupt):
            print('\nKeyboardInterrupt')
        except (SystemExit, EOFError):
            print("\n\nGood bye!\n")

        if not args.persist:
            return


    while True:
        try:
            evaluation = Evaluation(shell.definitions, out_callback=shell.out_callback)
            try:
                query = parse(shell.definitions, shell)
            except TranslateError as exc:
                evaluation.message('Syntax', exc.msg, *exc.args)
                continue
            if query is None:
                continue
            results = evaluation.evaluate([query], timeout=settings.TIMEOUT)
            shell.print_results(results)
        except (KeyboardInterrupt):
            print('\nKeyboardInterrupt')
        except (SystemExit, EOFError):
            print("\n\nGood bye!\n")
            break
        finally:
            shell.reset_continued()

if __name__ == '__main__':
    main()
