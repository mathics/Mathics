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


definitions = Definitions(add_builtin=True)
documentation = PyMathicsDocumentation()


def load_all_pymathics_modules():
    from mathics.settings import default_pymathics_modules
    from os import listdir, path
    from mathics.doc.doc import DocPart, DocChapter

    global definitions
    global documentation

    pymathics_dir = path.dirname(__file__) + "/"
    documentation.doc_dir = pymathics_dir + "/doc/"
    documentation.xml_data_file = pymathics_dir + "xml/"
    documentation.tex_data_file = pymathics_dir + "tex/"
    documentation.latex_file = pymathics_dir + "tex/pymathics-documentation.tex"

    pymathics_modules = []
    subdirs = listdir(pymathics_dir)
    for folder in subdirs:
        if folder[0] == "_":
            continue
        if path.isdir(pymathics_dir + folder):
            pymathics_modules.append(folder)

    modules_part = DocPart(documentation, "Modules", False)

    for module in pymathics_modules:
        try:
            definitions.load_pymathics_module(module)
        except Exception:
            continue
        docs = PyMathicsDocumentation(module)
        part = docs.get_part("pymathics-modules")
        for ch in part.chapters:
            ch.part = modules_part
            modules_part.chapters_by_slug[ch.slug] = ch
            modules_part.chapters.append(ch)
    documentation.parts.append(modules_part)


def main():
    load_all_pymathics_modules()
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
