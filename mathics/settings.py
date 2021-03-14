#!/usr/bin/env python3
# -*- coding: utf-8 -*-


import pkg_resources
import sys
import os
from os import path


DEBUG = True

# set only to True in DEBUG mode
DEBUG_MAIL = True
PROPAGATE_EXCEPTIONS = True
DISPLAY_EXCEPTIONS = True
DEBUG_PRINT = False

LOG_QUERIES = False

# Either None (no timeout) or a positive integer.
# unix only
TIMEOUT = None

# specifies a maximum recursion depth is safe for all Python environments
# without setting a custom thread stack size.
DEFAULT_MAX_RECURSION_DEPTH = 512

# max pickle.dumps() size for storing results in DB
# historically 10000 was used on public mathics servers
MAX_STORED_SIZE = 10000

ADMINS = (
    ('Admin', 'mail@test.com'),
)
MANAGERS = ADMINS

ROOT_DIR = pkg_resources.resource_filename('mathics', '')
if sys.platform.startswith('win'):
    DATA_DIR = os.environ['APPDATA'].replace(os.sep, '/') + '/Python/Mathics/'
else:
    DATA_DIR = path.expanduser('~/.local/var/mathics/')
# if not path.exists(DATA_DIR):
#    os.makedirs(DATA_DIR)

DOC_DIR = os.path.join(ROOT_DIR, 'doc/documentation/')
DOC_TEX_DATA = os.path.join(ROOT_DIR, 'doc/tex/data')
DOC_XML_DATA = os.path.join(ROOT_DIR, 'doc/xml/data')
DOC_LATEX_FILE = os.path.join(ROOT_DIR, 'doc/tex/documentation.tex')

# Local time zone for this installation. Choices can be found here:
# http://en.wikipedia.org/wiki/List_of_tz_zones_by_name
# although not all choices may be available on all operating systems.
# If running in a Windows environment this must be set to the same as your
# system time zone.
TIME_ZONE = 'Europe/Vienna'

# Set this True if you prefer 12 hour time to be the default
TIME_12HOUR = False

# Language code for this installation. All choices can be found here:
# http://www.i18nguy.com/unicode/language-identifiers.html
LANGUAGE_CODE = 'en-us'

SITE_ID = 1

# Leave this True unless you have specific reason for not permitting
# users to access local files
ENABLE_FILES_MODULE = True

# Enabling this prints out some stats on what takes time to startup Mathics.
BENCHMARK_STARTUP = False

# If you set this to False, Django will make some optimizations so as not
# to load the internationalization machinery.
USE_I18N = True

# Absolute path to the directory that holds static files.
STATIC_ROOT = os.path.join(ROOT_DIR, 'web/media/')

# URL that handles the media served from STATIC_ROOT.
STATIC_URL = '/media/'

# Make this unique, and don't share it with anybody.
SECRET_KEY = 'uvbhuiasaeaph6Duh)r@3ex1i@et=0j4h(!p4@!r6s-=a_ev*e'

# List of callables that know how to import templates from various sources.
# TEMPLATE_LOADERS = (
#    'django.template.loaders.filesystem.load_template_source',
#    'django.template.loaders.app_directories.load_template_source',
# )


