# -*- coding: utf-8 -*-


import pkg_resources
import sys
import os
from os import path


DEBUG = True

DEBUG_PRINT = False

# Either None (no timeout) or a positive integer.
# Unix only
TIMEOUT = None

# specifies a maximum recursion depth is safe for all Python environments
# without setting a custom thread stack size.
DEFAULT_MAX_RECURSION_DEPTH = 512

# max pickle.dumps() size for storing results in DB
# historically 10000 was used on public mathics servers
MAX_STORED_SIZE = 10000

ROOT_DIR = pkg_resources.resource_filename("mathics", "")
if sys.platform.startswith("win"):
    DATA_DIR = os.environ["APPDATA"].replace(os.sep, "/") + "/Python/Mathics/"
else:
    DATA_DIR = path.expanduser("~/.local/var/mathics/")
# if not path.exists(DATA_DIR):
#    os.makedirs(DATA_DIR)

# Location of internal document data.
# NOTE: Storing this in JSON if possible would be preferable and faster
DOC_DATA_PATH = os.path.join(DATA_DIR, "doc_data.pcl")

DOC_DIR = os.path.join(ROOT_DIR, "doc/documentation/")
DOC_LATEX_FILE = os.path.join(ROOT_DIR, "doc/tex/documentation.tex")


# Set this True if you prefer 12 hour time to be the default
TIME_12HOUR = False

# Leave this True unless you have specific reason for not permitting
# users to access local files
ENABLE_FILES_MODULE = True

# Rocky: this is probably a hack. LoadModule[] needs to handle
# whatever it is that setting this thing did.
default_pymathics_modules = []

SYSTEM_CHARACTER_ENCODING = "UTF-8" if sys.getdefaultencoding() == "utf-8" else "ASCII"
