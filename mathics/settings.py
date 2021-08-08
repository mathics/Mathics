# -*- coding: utf-8 -*-


import pkg_resources
import sys
import os
import os.path as osp
from pathlib import Path


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
    DATA_DIR = osp.join(os.environ["APPDATA"], "Python", "Mathics")
else:
    DATA_DIR = osp.join(
        os.environ.get("APPDATA", osp.expanduser("~/.local/var/mathics/"))
    )

# Location of internal document data. Currently this is in Python
# Pickle form, but storing this in JSON if possible would be preferable and faster

# We need two versions, one in the user space which is updated with
# local packages installed and is user writable.
DOC_USER_TEX_DATA_PATH = os.environ.get(
    "DOC_USER_TEX_DATA_PATH", osp.join(DATA_DIR, "doc_tex_data.pcl")
)

# We need another version as a fallback, and that is distributed with the
# package. It is note user writable and not in the user space.
DOC_SYSTEM_TEX_DATA_PATH = os.environ.get(
    "DOC_SYSTEM_TEX_DATA_PATH", osp.join(ROOT_DIR, "data", "doc_tex_data.pcl")
)

DOC_DIR = osp.join(ROOT_DIR, "doc", "documentation")
DOC_LATEX_FILE = osp.join(ROOT_DIR, "doc", "tex", "documentation.tex")

# Set this True if you prefer 12 hour time to be the default
TIME_12HOUR = False

# Leave this True unless you have specific reason for not permitting
# users to access local files.
ENABLE_FILES_MODULE = True

# Rocky: this is probably a hack. LoadModule[] needs to handle
# whatever it is that setting this thing did.
default_pymathics_modules = []

SYSTEM_CHARACTER_ENCODING = "UTF-8" if sys.getdefaultencoding() == "utf-8" else "ASCII"


def get_doc_tex_data_path(should_be_readable=False, create_parent=False) -> str:
    """Returns a string path where we can find Python Pickle data for LaTeX
    processing.

    If `should_be_readable` is True, the we will check to see whether this file is
    readable (which also means it exists). If not, we'll return the `DOC_SYSTEM_DATA_PATH`.
    """
    doc_user_tex_data_path = Path(DOC_USER_TEX_DATA_PATH)
    base_config_dir = doc_user_tex_data_path.parent
    if not base_config_dir.is_dir() and create_parent:
        Path("base_config_dir").mkdir(parents=True, exist_ok=True)

    if should_be_readable:
        return (
            DOC_USER_TEX_DATA_PATH
            if doc_user_tex_data_path.is_file()
            else DOC_SYSTEM_TEX_DATA_PATH
        )
    else:
        return DOC_USER_TEX_DATA_PATH
