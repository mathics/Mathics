#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import print_function
from __future__ import unicode_literals
from __future__ import absolute_import


from argparse import ArgumentParser

import mathics
import mathics.test as testsuite
from mathics.test import write_latex, test_all, test_section
from mathics.core.definitions import Definitions
from mathics.doc.doc import PyMathicsDocumentation


module = "pymathics"

definitions = Definitions(add_builtin=True)
definitions.load_pymathics_module(module)
documentation = PyMathicsDocumentation(module)

def main():
    testsuite.documentation = documentation
    testsuite.definitions = definitions
    parser = ArgumentParser(description="Mathics test suite, Pymathics module.", add_help=False)
    parser.add_argument(
        '--help', '-h', help='show this help message and exit', action='help')
    parser.add_argument(
        '--version', '-v', action='version',
        version='%(prog)s ' + mathics.__version__)
    parser.add_argument('--section', '-s', dest="section",
                        metavar="SECTION", help="only test SECTION")
    parser.add_argument('--output', '-o', dest="output", action="store_true",
                        help="generate TeX and XML output data")
    parser.add_argument('--tex', '-t', dest="tex", action="store_true",
                        help="generate TeX documentation file")
    parser.add_argument('--quiet', '-q', dest="quiet",
                        action="store_true", help="hide passed tests")
    parser.add_argument('--stop-on-failure', action="store_true",
                        help="stop on failure")
    parser.add_argument('--pymathics', '-l', dest="pymathics", action="store_true",
                        help="also checks pymathics modules.")
    parser.add_argument('--skip', metavar='N', dest="skip", type=int,
                        default=0, help="skip the first N tests")
    args = parser.parse_args()

    if args.tex:
        write_latex()
    else:
        if args.section:
            test_section(args.section, stop_on_failure=args.stop_on_failure)
        else:
            start_at = args.skip + 1
            test_all(quiet=args.quiet, generate_output=args.output,
                     stop_on_failure=args.stop_on_failure,
                     start_at=start_at)

if __name__ == '__main__':
    main()
