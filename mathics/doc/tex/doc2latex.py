#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Reads in Pickle'd file and write LaTeX file containing the entire User Manual
"""

import os
import os.path as osp
import pickle
import subprocess
import sys

from argparse import ArgumentParser

import mathics

from mathics import version_string, __version__
from mathics import settings
from mathics.doc.common_doc import MathicsMainDocumentation

# Global variables
logfile = None

DOC_LATEX_FILE = os.environ.get("DOC_LATEX_FILE", settings.DOC_LATEX_FILE)


def extract_doc_from_source(quiet=False):
    """
    Write internal (pickled) TeX doc mdoc files and example data in docstrings.
    """
    if not quiet:
        print(f"Extracting internal doc data for {version_string}")
    try:
        return load_doc_data(settings.get_doc_tex_data_path(should_be_readable=True))
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
        d = osp.dirname(f)
        if d and not osp.exists(d):
            os.makedirs(d)
        return open(f, *args, **kwargs)


def print_and_log(*args):
    global logfile
    a = [a.decode("utf-8") if isinstance(a, bytes) else a for a in args]
    string = "".join(a)
    print(string)
    if logfile:
        logfile.write(string)


def get_versions():
    def try_cmd(cmd_list: tuple, stdout_or_stderr: str) -> str:
        status = subprocess.run(cmd_list, capture_output=True)
        if status.returncode == 0:
            out = getattr(status, stdout_or_stderr)
            return out.decode("utf-8").split("\n")[0]
        else:
            return "Unknown"

    versions = {
        "MathicsCoreVersion": __version__,
        "PythonVersion": sys.version,
    }

    for name, cmd, field in (
        ["AsymptoteVersion", ("asy", "--version"), "stderr"],
        ["XeTeXVersion", ("xetex", "--version"), "stdout"],
        ["GhostscriptVersion", ("gs", "--version"), "stdout"],
    ):
        versions[name] = try_cmd(cmd, field)
    return versions


def write_latex(doc_data, quiet=False, filter_parts=None, filter_chapters=None):
    documentation = MathicsMainDocumentation()
    if not quiet:
        print(f"Writing LaTeX document to {DOC_LATEX_FILE}")
    with open_ensure_dir(DOC_LATEX_FILE, "wb") as doc:
        content = documentation.latex(
            doc_data,
            quiet=quiet,
            filter_parts=filter_parts,
            filter_chapters=filter_chapters,
        )
        content = content.encode("utf-8")
        doc.write(content)
    DOC_VERSION_FILE = osp.join(osp.dirname(DOC_LATEX_FILE), "version-info.tex")
    if not quiet:
        print(f"Writing Mathics Core Version Information to {DOC_VERSION_FILE}")
    with open(DOC_VERSION_FILE, "w") as doc:
        doc.write("%% Mathics core version number created via doc2latex.py\n\n")
        for name, version_info in get_versions().items():
            doc.write("""\\newcommand{\\%s}{%s}\n""" % (name, version_info))


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
        "--chapters",
        "-c",
        dest="chapters",
        metavar="CHAPTER",
        help="only test CHAPTER(s). "
        "You can list multiple chapters by adding a comma (and no space) in between chapter names.",
    )
    parser.add_argument(
        "--parts",
        "-p",
        dest="parts",
        metavar="PART",
        help="only test PART(s). "
        "You can list multiple parts by adding a comma (and no space) in between part names.",
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
    write_latex(
        doc_data,
        quiet=args.quiet,
        filter_parts=args.parts,
        filter_chapters=args.chapters,
    )


if __name__ == "__main__":
    main()
