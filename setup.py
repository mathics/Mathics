#!/usr/bin/env python
from distribute_setup import use_setuptools
use_setuptools()

from setuptools import setup

from distutils.extension import Extension

from mathics import settings

import sys

if sys.subversion[0] == 'PyPy':
    is_PyPy = True
else:
    is_PyPy = False

try:
    if is_PyPy:
        raise ImportError
    from Cython.Distutils import build_ext
    EXTENSIONS = {
        'core': ['expression', 'numbers', 'rules', 'pattern'],
        'builtin': ['arithmetic', 'numeric', 'patterns', 'graphics']
    }
    EXTENSIONS = [Extension('mathics.%s.%s' % (parent, module),
        ['mathics/%s/%s.py' % (parent, module)]) for parent, modules in EXTENSIONS.iteritems() for module in modules]
    CMDCLASS = {'build_ext': build_ext}
    INSTALL_REQUIRES = ['cython>=0.15.1']
except ImportError:
    EXTENSIONS = []
    CMDCLASS = {}
    INSTALL_REQUIRES = []
    
# General Requirements
INSTALL_REQUIRES += ['sympy>=0.7.2', 'mpmath>=0.15', 'django>=1.2']

# strange SandboxError with SymPy 0.6.7 in Sage (writing to ~/.sage/tmp)

#if sys.platform == "darwin":
#    INSTALL_REQUIRES += ['readline']
    
def subdirs(root, file='*.*', depth=10):
    for k in range(depth):
        yield root + '*/' * k + file
    
mathjax_files = list(subdirs('media/js/mathjax/'))

setup(
    name = "Mathics",
    cmdclass = CMDCLASS,
    ext_modules = EXTENSIONS,
    version = settings.VERSION,
    
    packages = ['mathics', 'mathics.builtin', 'mathics.optional',
        'mathics.core', 'mathics.core.spark', 'mathics.data',
        'mathics.doc', 'mathics.web', 'mathics.web.templatetags'],

    install_requires = INSTALL_REQUIRES,

    package_data = {
        'mathics.doc': ['documentation/*.mdoc', 'xml/data'],
        'mathics.web': ['media/css/*.css', 'media/img/*.gif',
            'media/js/innerdom/*.js', 'media/js/prototype/*.js', 'media/js/scriptaculous/*.js', 'media/js/three/Three.js', 'media/js/three/Detector.js',
            'media/js/*.js',
            'templates/*.html', 'templates/doc/*.html'] + mathjax_files,
        'mathics.data': ['*.csv'],
    },
    
    entry_points = {
        'console_scripts': [
            'mathics = mathics.main:main',
            'mathicsserver = mathics.server:main',
        ],
    },
    
    zip_safe = False,   # don't pack Mathics in egg file because of sqlite database, media files, etc.

    # metadata for upload to PyPI
    author = "Jan Poeschko",
    author_email = "jan@poeschko.com",
    description = "A general-purpose computer algebra system.",
    license = "GPL",
    keywords = "computer algebra system mathics mathematica sage sympy",
    url = "http://www.mathics.org/",   # project home page, if any

    # TODO: could also include long_description, download_url, classifiers, etc.
)


