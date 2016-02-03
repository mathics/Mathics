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
import os
import json

from distutils import log

from setuptools import setup, Command, Extension
from setuptools.command.install import install

# Ensure user has the correct Python version
if sys.version_info[:2] != (2, 7):
    print("Mathics supports Python 2.7. \
Python %d.%d detected" % sys.version_info[:2])
    sys.exit(-1)

# stores __version__ in the current namespace
execfile('mathics/version.py')

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
SETUP_REQUIRES = [] # TODO ipython

INSTALL_REQUIRES += ['sympy==0.7.6', 'django >= 1.8, < 1.9', 'ply>=3.8',
                     'mpmath>=0.19', 'python-dateutil', 'colorama',
                     'interruptingcow'] + SETUP_REQUIRES

# if sys.platform == "darwin":
#    INSTALL_REQUIRES += ['readline']

kernel_json = {
    'argv': [sys.executable,
             '-m', 'mathics',
             '-f', '{connection_file}'],
    'display_name': 'mathics',
    'language': 'Wolfram',
    'name': 'mathics',
}

class install_with_kernelspec(install):
    def run(self):
        install.run(self)

        from ipykernel.kernelspec import write_kernel_spec
        from jupyter_client.kernelspec import KernelSpecManager

        kernel_spec_manager = KernelSpecManager()

        log.info('Writing kernel spec')
        kernel_spec_path = write_kernel_spec(overrides=kernel_json)

        log.info('Installing kernel spec')
        kernel_spec_manager.install_kernel_spec(
            kernel_spec_path,
            kernel_name=kernel_json['name'],
            user=self.user)

class test(Command):
    """
    Runs the unittests
    """

    description = "runs the unittests"
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


CMDCLASS['test'] = test
CMDCLASS['install'] = install_with_kernelspec

setup(
    name="Mathics",
    cmdclass=CMDCLASS,
    ext_modules=EXTENSIONS,
    version=__version__,

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

    setup_requires=SETUP_REQUIRES,

    package_data={
        'mathics.doc': ['documentation/*.mdoc', 'xml/data'],
        'mathics.web': [
            'media/css/*.css', 'media/img/*.gif',
            'media/js/innerdom/*.js', 'media/js/prototype/*.js',
            'media/js/scriptaculous/*.js', 'media/js/three/Three.js',
            'media/js/three/Detector.js', 'media/js/*.js', 'templates/*.html',
            'templates/doc/*.html'],
        'mathics.data': ['*.csv', 'ExampleData/*'],
        'mathics.builtin.pymimesniffer': ['mimetypes.xml'],
        'mathics.autoload': ['formats/*/Import.m', 'formats/*/Export.m'],
        'mathics.packages': ['*/*.m', '*/Kernel/init.m'],
    },

    entry_points={
        'console_scripts': [
            'mathics = mathics.main:main',
        ],
    },

    # don't pack Mathics in egg because of sqlite database, media files, etc.
    zip_safe=False,

    # metadata for upload to PyPI
    author="Jan Poeschko",
    author_email="jan@poeschko.com",
    description="A general-purpose computer algebra system.",
    license="GPL",
    keywords="computer algebra system mathics mathematica sympy wolfram",
    url="http://www.mathics.github.io/",   # project home page, if any

    # TODO: could also include long_description, download_url, classifiers,
    # etc.
)
