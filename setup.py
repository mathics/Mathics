#!/usr/bin/env python
"""Setuptools based setup script for Mathics.

For the easiest installation just type the following command (you'll probably
need root privileges):

    python setup.py install

This will install the library in the default location. For instructions on
how to customize the install procedure read the output of:

    python setup.py --help install

In addition, there are some other commands:

    python setup.py clean -> will clean all trash (*.pyc and stuff)

To get a full list of avaiable commands, read the output of:

    python setup.py --help-commands

Or, if all else fails, feel free to write to the sympy list at
mathics-users@googlegroups.com and ask for help.
"""

import sys
from setuptools import setup, Command, Extension

# Ensure user has the correct Python version
if not (2, 5) <= sys.version_info[:2] <= (2, 7):
    print("Mathics supports Python 2.5 upto Python 2.7. \
Python %d.%d detected" % sys.version_info[:2])
    sys.exit(-1)

from mathics import settings

if sys.subversion[0] == 'PyPy':
    is_PyPy = True
else:
    is_PyPy = False

try:
    if is_PyPy:
        raise ImportError
    from Cython.Distutils import build_ext
except ImportError:
    EXTENSIONS = []
    CMDCLASS = {}
    INSTALL_REQUIRES = []
else:
    EXTENSIONS = {
        'core': ['expression', 'numbers', 'rules', 'pattern'],
        'builtin': ['arithmetic', 'numeric', 'patterns', 'graphics']
    }
    EXTENSIONS = [
        Extension('mathics.%s.%s' % (parent, module),
                  ['mathics/%s/%s.py' % (parent, module)])
        for parent, modules in EXTENSIONS.iteritems() for module in modules]
    CMDCLASS = {'build_ext': build_ext}
    INSTALL_REQUIRES = ['cython>=0.15.1']

# General Requirements
INSTALL_REQUIRES += ['sympy==0.7.2', 'django>=1.2', 'ply>=3.4',
                     'argparse', 'python-dateutil', 'colorama',
                     'interruptingcow']

# strange SandboxError with SymPy 0.6.7 in Sage (writing to ~/.sage/tmp)

# if sys.platform == "darwin":
#    INSTALL_REQUIRES += ['readline']


def subdirs(root, file='*.*', depth=10):
    for k in range(depth):
        yield root + '*/' * k + file


class initialize(Command):
    """
    Creates the database used by Django
    """

    description = "create the database used by django"
    user_options = []  # distutils complains if this is not here.

    def __init__(self, *args):
        self.args = args[0]  # so we can pass it to other classes
        Command.__init__(self, *args)

    def initialize_options(self):  # distutils wants this
        pass

    def finalize_options(self):    # this too
        pass

    def run(self):
        import os
        import subprocess

        database_file = settings.DATABASES['default']['NAME']
        print("Creating data directory %s" % settings.DATA_DIR)
        if not os.path.exists(settings.DATA_DIR):
            os.makedirs(settings.DATA_DIR)
        print("Creating database %s" % database_file)
        subprocess.call(
            [sys.executable, 'mathics/manage.py', 'syncdb', '--noinput'])
        os.chmod(database_file, 0o766)
        print("")
        print("Mathics initialized successfully.")

CMDCLASS['initialize'] = initialize

mathjax_files = list(subdirs('media/js/mathjax/'))

setup(
    name="Mathics",
    cmdclass=CMDCLASS,
    ext_modules=EXTENSIONS,
    version=settings.VERSION,

    packages=[
        'mathics',
        'mathics.core',
        'mathics.builtin', 'mathics.builtin.pymimesniffer', 'mathics.data',
        'mathics.doc',
        'mathics.autoload',
        'mathics.packages',
        'mathics.web', 'mathics.web.templatetags'
    ],

    install_requires=INSTALL_REQUIRES,

    package_data={
        'mathics.doc': ['documentation/*.mdoc', 'xml/data'],
        'mathics.web': [
            'media/css/*.css', 'media/img/*.gif',
            'media/js/innerdom/*.js', 'media/js/prototype/*.js',
            'media/js/scriptaculous/*.js', 'media/js/three/Three.js',
            'media/js/three/Detector.js', 'media/js/*.js', 'templates/*.html',
            'templates/doc/*.html'] + mathjax_files,
        'mathics.data': ['*.csv', 'ExampleData/*'],
        'mathics.builtin.pymimesniffer': ['mimetypes.xml'],
        'mathics.autoload': ['formats/*/Import.m', 'formats/*/Export.m'],
        'mathics.packages': ['*/*.m', '*/Kernel/init.m'],
    },

    entry_points={
        'console_scripts': [
            'mathics = mathics.main:main',
            'mathicsserver = mathics.server:main',
        ],
    },

    # don't pack Mathics in egg because of sqlite database, media files, etc.
    zip_safe=False,

    # metadata for upload to PyPI
    author="Jan Poeschko",
    author_email="jan@poeschko.com",
    description="A general-purpose computer algebra system.",
    license="GPL",
    keywords="computer algebra system mathics mathematica sage sympy",
    url="http://www.mathics.org/",   # project home page, if any

    # TODO: could also include long_description, download_url, classifiers,
    # etc.
)
