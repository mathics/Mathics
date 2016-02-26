#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import unicode_literals
from __future__ import print_function
from __future__ import absolute_import

import sys
import platform
import sympy
import mpmath
import django
import six

from mathics.version import __version__
from mathics.core.expression import (
    Expression, Symbol, String, Number, Integer, Real, Complex, Rational,
    from_python)
from mathics.core.convert import from_sympy


if six.PY2:
    import codecs
    writer = codecs.getwriter("utf-8")
    sys.stdout = writer(sys.stdout)


version_info = {
    'mathics': __version__,
    'sympy': sympy.__version__,
    'mpmath': mpmath.__version__,
    'python': platform.python_implementation() + " " + sys.version.split('\n')[0],
    'django': django.__version__,
}


version_string = '''Mathics {mathics}
on {python}
using SymPy {sympy}, mpmath {mpmath}'''.format(**version_info)


server_version_string = version_string + ', django {django}'.format(**version_info)


license_string = '''\
Copyright (C) 2011-2016 The Mathics Team.
This program comes with ABSOLUTELY NO WARRANTY.
This is free software, and you are welcome to redistribute it
under certain conditions.
See the documentation for the full license.'''
