#!/usr/bin/env python
from distribute_setup import use_setuptools
use_setuptools()

from setuptools import setup

from mathics import settings

INSTALL_REQUIRES = ['sympy==0.6.6', 'gmpy>=1.04', 'mpmath>=0.15']
# strange SandboxError with SymPy 0.6.7 in Sage (writing to ~/.sage/tmp)

#if sys.platform == "darwin":
#    INSTALL_REQUIRES += ['readline']
    
INSTALL_REQUIRES += ['django>=1.0']

setup(
    name = "Mathics",
    version = settings.VERSION,
    
    packages = ['mathics', 'mathics.builtin', 'mathics.optional',
        'mathics.core', 'mathics.core.spark',
        'mathics.doc', 'mathics.web', 'mathics.web.templatetags'],

    install_requires = INSTALL_REQUIRES,

    package_data = {
        'mathics.doc': ['documentation/*.mdoc', 'xml/data'],
        'mathics.web': ['media/css/*.css', 'media/img/*.gif',
            'media/js/innerdom/*.js', 'media/js/prototype/*.js', 'media/js/scriptaculous/*.js',
            'media/js/*.js',
            'templates/*.html', 'templates/doc/*.html'],
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
    author_email = "jan@mathics.net",
    description = "A general-purpose computer algebra system.",
    license = "GPL",
    keywords = "computer algebra system mathics mathematica sage sympy",
    url = "http://www.mathics.net/",   # project home page, if any

    # TODO: could also include long_description, download_url, classifiers, etc.
)


