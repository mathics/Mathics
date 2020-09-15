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
    documentation.xml_data_file = pymathics_dir + "xml/data"
    documentation.tex_data_file = pymathics_dir + "tex/data"
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
    return modules_part


def test_basic():
    # This seems to test the document extraction
    modules_part = load_all_pymathics_modules()
    assert modules_part

    # Well test importing the one known module we can import pymathics.natlang
    import pymathics.natlang

    assert pymathics.natlang


if __name__ == "__main__":
    test_basic()
