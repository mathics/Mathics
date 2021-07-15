#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Reads in Pickle'd file and write LaTeX file containing the entire User Manual
"""

import os
import pickle

from argparse import ArgumentParser

import mathics

from mathics import version_string
from mathics import settings

# Global variables
documentation = None
logfile = None


def load_doc_data():
    print(f"Loading LaTeX internal data from {settings.DOC_TEX_DATA_PATH}")
    with open_ensure_dir(settings.DOC_TEX_DATA_PATH, "rb") as tex_data_file:
        return pickle.load(tex_data_file)


def print_and_log(*args):
    global logfile
    a = [a.decode("utf-8") if isinstance(a, bytes) else a for a in args]
    string = "".join(a)
    print(string)
    if logfile:
        logfile.write(string)


stars = "*" * 10


def open_ensure_dir(f, *args, **kwargs):
    try:
        return open(f, *args, **kwargs)
    except (IOError, OSError):
        d = os.path.dirname(f)
        if d and not os.path.exists(d):
            os.makedirs(d)
        return open(f, *args, **kwargs)


def extract_doc_from_source(quiet=False):
    """
    Write internal (pickled) TeX doc mdoc files and example data in docstrings.
    """
    if not quiet:
        print(f"Extracting internal doc data for {version_string}")
        print("This may take a while...")

    try:
        output_tex = load_doc_data()
    except KeyboardInterrupt:
        print("\nAborted.\n")
        return


def write_latex():
    print(f"Load data {settings.DOC_TEX_DATA_PATH}")
    with open_ensure_dir(settings.DOC_TEX_DATA_PATH, "rb") as output_file:
        output_tex = pickle.load(output_file)

    print(f"Write LaTeX {settings.DOC_LATEX_FILE}")
    with open_ensure_dir(settings.DOC_LATEX_FILE, "wb") as doc:
        content = documentation.latex(output_tex)
        content = content.encode("utf-8")
        doc.write(content)


def main():
    from mathics.doc import documentation as main_mathics_documentation

    global documentation
    global logfile
    documentation = main_mathics_documentation

    parser = ArgumentParser(description="Mathics test suite.", add_help=False)
    parser.add_argument(
        "--help", "-h", help="show this help message and exit", action="help"
    )
    parser.add_argument(
        "--version", "-v", action="version", version="%(prog)s " + mathics.__version__
    )
    parser.add_argument(
        "--quiet", "-q", dest="quiet", action="store_true", help="hide passed tests"
    )
    args = parser.parse_args()

    extract_doc_from_source(quiet=args.quiet)
    write_latex()


if __name__ == "__main__":
    main()
