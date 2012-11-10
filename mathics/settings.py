# -*- coding: utf8 -*-

u"""
    Mathics: a general-purpose computer algebra system
    Copyright (C) 2011 Jan Pöschko

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
"""

import pkg_resources
import sys
import os
from os import path

try:
    import django
    DJANGO_VERSION = django.VERSION
except ImportError:
    DJANGO_VERSION = (1, 4)

VERSION = '0.5.1dev'

DEBUG = True
TEMPLATE_DEBUG = DEBUG

# set only to True in DEBUG mode
DEBUG_MAIL = True
PROPAGATE_EXCEPTIONS = True
DISPLAY_EXCEPTIONS = True
DEBUG_PRINT = False

LOG_QUERIES = False

TIMEOUT = None
MAX_RECURSION_DEPTH = 200

# number of bits of precision for inexact calculations
MACHINE_PRECISION = 64

ADMINS = (
    (u'Admin', 'mail@test.com'),
)
MANAGERS = ADMINS

ROOT_DIR = pkg_resources.resource_filename('mathics', '') + '/'
if sys.platform.startswith('win'):
    DATA_DIR = '%APPDATA%/Python/Mathics/'
else:
    DATA_DIR = path.expanduser('~/.local/var/mathics/')
#if not path.exists(DATA_DIR):
#    os.makedirs(DATA_DIR)

DOC_DIR = ROOT_DIR + 'doc/documentation/'
DOC_TEX_DATA = ROOT_DIR + 'doc/tex/data'
DOC_XML_DATA = ROOT_DIR + 'doc/xml/data'
DOC_LATEX_FILE = ROOT_DIR + 'doc/tex/documentation.tex'

"""
For Django < 1.2:
DATABASE_ENGINE = 'sqlite3'
DATABASE_NAME = DATA_DIR + 'mathics.sqlite'
DATABASE_HOST = ''             # Set to empty string for localhost. Not used with sqlite3.
DATABASE_PORT = ''             # Set to empty string for default. Not used with sqlite3.
"""

DATABASES = {
    'default': {
        'ENGINE': 'django.db.backends.sqlite3',
        'NAME': DATA_DIR + 'mathics.sqlite'
    }
}

REQUIRE_LOGIN = False

SERVER_EMAIL = 'mathics@localhost'

# Local time zone for this installation. Choices can be found here:
# http://en.wikipedia.org/wiki/List_of_tz_zones_by_name
# although not all choices may be available on all operating systems.
# If running in a Windows environment this must be set to the same as your
# system time zone.
TIME_ZONE = 'Europe/Vienna'

# Language code for this installation. All choices can be found here:
# http://www.i18nguy.com/unicode/language-identifiers.html
LANGUAGE_CODE = 'en-us'

SITE_ID = 1

# If you set this to False, Django will make some optimizations so as not
# to load the internationalization machinery.
USE_I18N = True

# Absolute path to the directory that holds media.
# Example: "/home/media/media.lawrence.com/"
MEDIA_ROOT = ROOT_DIR + 'web/media/'

# URL that handles the media served from MEDIA_ROOT. Make sure to use a
# trailing slash if there is a path component (optional in other cases).
# Examples: "http://media.lawrence.com", "http://example.com/media/"
MEDIA_URL = '/media/'

# URL prefix for admin media -- CSS, JavaScript and images. Make sure to use a
# trailing slash.
# Examples: "http://foo.com/media/", "/media/".
if DJANGO_VERSION < (1, 3):
    ADMIN_MEDIA_PREFIX = '/media/admin/'

# Make this unique, and don't share it with anybody.
SECRET_KEY = 'uvbhuiasaeaph6Duh)r@3ex1i@et=0j4h(!p4@!r6s-=a_ev*e'

# List of callables that know how to import templates from various sources.
#TEMPLATE_LOADERS = (
#    'django.template.loaders.filesystem.load_template_source',
#    'django.template.loaders.app_directories.load_template_source',
#)

MIDDLEWARE_CLASSES = (
    'django.middleware.common.CommonMiddleware',
    'django.contrib.sessions.middleware.SessionMiddleware',
    'django.contrib.auth.middleware.AuthenticationMiddleware',
)

ROOT_URLCONF = 'mathics.urls'

TEMPLATE_DIRS = (
    # Put strings here, like "/home/html/django_templates" or "C:/www/django/templates".
    # Always use forward slashes, even on Windows.
    # Don't forget to use absolute paths, not relative paths.
    ROOT_DIR + 'web/templates/',
)

AUTHENTICATION_BACKENDS = (
    'mathics.web.authentication.EmailModelBackend',
)

INSTALLED_APPS = (
    'django.contrib.auth',
    'django.contrib.contenttypes',
    'django.contrib.sessions',
    'django.contrib.sites',
    'mathics.web',
)
