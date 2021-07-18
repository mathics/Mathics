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
from mathics.doc.common_doc import MathicsMainDocumentation

# Global variables
logfile = None


def extract_doc_from_source(quiet=False):
    """
    Write internal (pickled) TeX doc mdoc files and example data in docstrings.
    """
    if not quiet:
        print(f"Extracting internal doc data for {version_string}")
    try:
        return load_doc_data(settings.DOC_DATA_PATH)
    except KeyboardInterrupt:
        print("\nAborted.\n")
        return


def load_doc_data(data_path, quiet=False):
    if not quiet:
        print(f"Loading LaTeX internal data from {data_path}")
    with open_ensure_dir(data_path, "rb") as doc_data_fp:
        return pickle.load(doc_data_fp)


def open_ensure_dir(f, *args, **kwargs):
    try:
        return open(f, *args, **kwargs)
    except (IOError, OSError):
        d = os.path.dirname(f)
        if d and not os.path.exists(d):
            os.makedirs(d)
        return open(f, *args, **kwargs)


def print_and_log(*args):
    global logfile
    a = [a.decode("utf-8") if isinstance(a, bytes) else a for a in args]
    string = "".join(a)
    print(string)
    if logfile:
        logfile.write(string)


def write_latex(doc_data, quiet=False):
    documentation = MathicsMainDocumentation()
    if not quiet:
        print(f"Writing LaTeX {settings.DOC_LATEX_FILE}")
    with open_ensure_dir(settings.DOC_LATEX_FILE, "wb") as doc:
        content = documentation.latex(doc_data, quiet=quiet)
        content = content.encode("utf-8")
        doc.write(content)


def main():

    global logfile

    parser = ArgumentParser(description="Mathics test suite.", add_help=False)
    parser.add_argument(
        "--help", "-h", help="show this help message and exit", action="help"
    )
    parser.add_argument(
        "--version", "-v", action="version", version="%(prog)s " + mathics.__version__
    )
    parser.add_argument(
        "--quiet",
        "-q",
        dest="quiet",
        action="store_true",
        help="Don't show formatting progress tests",
    )
    args = parser.parse_args()
    doc_data = extract_doc_from_source(quiet=args.quiet)
    write_latex(doc_data)


if __name__ == "__main__":
    main()
