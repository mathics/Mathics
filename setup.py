#!/usr/bin/env python
# -*- coding: utf-8 -*-

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

Or, if all else fails, feel free to write to the mathics users list at
mathics-users@googlegroups.com and ask for help.
"""

import sys
import platform
import os
from setuptools import setup, Command, Extension

# Ensure user has the correct Python version
if sys.version_info < (3, 3):
    print("Mathics does not support Python %d.%d" % sys.version_info[:2])
    sys.exit(-1)

# stores __version__ in the current namespace
exec(compile(open('mathics/version.py').read(), 'mathics/version.py', 'exec'))

is_PyPy = (platform.python_implementation() == 'PyPy')

INSTALL_REQUIRES = []
DEPENDENCY_LINKS = []

try:
    if is_PyPy:
        raise ImportError
    from Cython.Distutils import build_ext
except ImportError:
    EXTENSIONS = []
    CMDCLASS = {}
else:
    EXTENSIONS = {
        'core': ['expression', 'numbers', 'rules', 'pattern'],
        'builtin': ['arithmetic', 'numeric', 'patterns', 'graphics']
    }
    EXTENSIONS = [
        Extension('mathics.%s.%s' % (parent, module),
                  ['mathics/%s/%s.py' % (parent, module)])
        for parent, modules in EXTENSIONS.items() for module in modules]
    CMDCLASS = {'build_ext': build_ext}
    INSTALL_REQUIRES += ['cython>=0.15.1']

# General Requirements
INSTALL_REQUIRES += ['sympy==1.4', 'django >= 1.8, < 1.12',
                     'mpmath>=0.19', 'python-dateutil', 'colorama']


def subdirs(root, file='*.*', depth=10):
    for k in range(depth):
        yield root + '*/' * k + file


class initialize(Command):
    """
    Manually create the Django database used by the web notebook
    """

    description = "manually create the Django database used by the web notebook"
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
        settings = {}
        exec(compile(open('mathics/settings.py').read(), 'mathics/settings.py', 'exec'), settings)

        database_file = settings['DATABASES']['default']['NAME']
        print("Creating data directory %s" % settings['DATA_DIR'])
        if not os.path.exists(settings['DATA_DIR']):
            os.makedirs(settings['DATA_DIR'])
        print("Creating database %s" % database_file)
        try:
            subprocess.check_call(
                [sys.executable, 'mathics/manage.py', 'migrate', '--noinput'])
            print("")
            print("Database created successfully.")
        except subprocess.CalledProcessError:
            print("Error: failed to create database")
            sys.exit(1)


class test(Command):
    """
    Run the unittests
    """

    description = "run the unittests"
    user_options = []

    def __init__(self, *args):
        self.args = args[0]  # so we can pass it to other classes
        Command.__init__(self, *args)

    def initialize_options(self):  # distutils wants this
        pass

    def finalize_options(self):    # this too
        pass

    def run(self):
        import unittest
        test_loader = unittest.defaultTestLoader
        test_runner = unittest.TextTestRunner(verbosity=3)
        test_suite = test_loader.discover('test/')
        test_result = test_runner.run(test_suite)

        if not test_result.wasSuccessful():
            sys.exit(1)


CMDCLASS['initialize'] = initialize
CMDCLASS['test'] = test

mathjax_files = list(subdirs('media/js/mathjax/'))

setup(
    name="Mathics",
    cmdclass=CMDCLASS,
    ext_modules=EXTENSIONS,
    version=__version__,

    packages=[
        'mathics',
        'mathics.algorithm',
        'mathics.core',
        'mathics.core.parser',
        'mathics.builtin', 'mathics.builtin.pymimesniffer', 'mathics.builtin.numpy_utils',
        'mathics.builtin.pympler', 'mathics.builtin.compile',
        'mathics.doc',
        'mathics.web', 'mathics.web.templatetags', 'mathics.web.migrations'
    ],

    install_requires=INSTALL_REQUIRES,
    dependency_links=DEPENDENCY_LINKS,

    package_data={
        'mathics': [
            'data/*.csv', 'data/ExampleData/*',
            'autoload/formats/*/Import.m', 'autoload/formats/*/Export.m',
            'packages/*/*.m', 'packages/*/Kernel/init.m'],
        'mathics.doc': ['documentation/*.mdoc', 'xml/data'],
        'mathics.web': [
            'media/css/*.css', 'media/img/*.*', 'media/fonts/*',
            'media/img/favicons/*',
            'media/js/innerdom/*.js', 'media/js/prototype/*.js',
            'media/js/scriptaculous/*.js', 'media/js/three/Three.js',
            'media/js/three/Detector.js', 'media/js/*.js', 'templates/*.html',
            'templates/doc/*.html'] + mathjax_files,
        'mathics.builtin.pymimesniffer': ['mimetypes.xml'],
    },

    entry_points={
        'console_scripts': [
            'mathics = mathics.main:main',
            'mathicsserver = mathics.server:main',
            'mathicsscript = mathics.script:main',
        ],
    },

    # don't pack Mathics in egg because of media files, etc.
    zip_safe=False,

    # metadata for upload to PyPI
    author="Angus Griffith",
    author_email="mathics@angusgriffith.com",
    description="A general-purpose computer algebra system.",
    license="GPL",
    url="https://mathics.github.io/",
    download_url="https://github.com/mathics/Mathics/tarball/v0.9",
    keywords=['Mathematica', 'Wolfram', 'Interpreter', 'Shell', 'Math', 'CAS'],
    classifiers=[
        'Intended Audience :: Developers',
        'Intended Audience :: Science/Research',
        'License :: OSI Approved :: GNU General Public License v3 (GPLv3)',
        'Programming Language :: Python',
        'Programming Language :: Python :: 3.4',
        'Programming Language :: Python :: 3.5',
        'Programming Language :: Python :: 3.6',
        'Programming Language :: Python :: Implementation :: CPython',
        'Programming Language :: Python :: Implementation :: PyPy',
        'Topic :: Scientific/Engineering',
        'Topic :: Scientific/Engineering :: Mathematics',
        'Topic :: Scientific/Engineering :: Physics',
        'Topic :: Software Development :: Interpreters',
    ],
    # TODO: could also include long_description, download_url,
)
