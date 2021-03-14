#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
import platform
import sympy
import mpmath
import numpy

from mathics.version import __version__
from mathics.core.expression import (
    Expression,
    Symbol,
    String,
    Number,
    Integer,
    Real,
    Complex,
    Rational,
    from_python,
    MachineReal,
    PrecisionReal,
)
from mathics.core.convert import from_sympy


version_info = {
    "mathics": __version__,
    "sympy": sympy.__version__,
    "mpmath": mpmath.__version__,
    "numpy": numpy.__version__,
    "python": platform.python_implementation() + " " + sys.version.split("\n")[0],
}

try:
    import cython
except ImportError:
    pass
else:
    version_info["cython"] = cython.__version__


version_string = """Mathics {mathics}
on {python}
using SymPy {sympy}, mpmath {mpmath}, numpy {numpy}""".format(
    **version_info
)


if "cython" in version_info:
    version_string += f", cython {version_info['cython']}"

license_string = """\
Copyright (C) 2011-2021 The Mathics Team.
This program comes with ABSOLUTELY NO WARRANTY.
This is free software, and you are welcome to redistribute it
under certain conditions.
See the documentation for the full license."""
