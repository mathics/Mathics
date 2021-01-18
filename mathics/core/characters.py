#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import re
import yaml
import os

from mathics.settings import ROOT_DIR
from mathics.core.util import re_from_keys

####### INITIALIZATION #######

def unicode_equivalent(k: str, v: dict):
    if "unicode-equivalent" in v:
        return v["unicode-equivalent"]
    else:
        return f"\\[{k}]"

# Load the raw data
with open(os.path.join(ROOT_DIR, "data/named-characters.yml"), "r") as f:
    CHARS_DATA = yaml.load(f, Loader=yaml.FullLoader)

# Conversion from WL to the fully qualified names
WL_TO_PLAIN_DICT = {v["wl-unicode"]: f"\\[{k}]" 
                   for k, v in CHARS_DATA.items()}
WL_TO_PLAIN_RE = re_from_keys(WL_TO_PLAIN_DICT)

# Conversion from WL to unicode
WL_TO_UNICODE_DICT = {v["wl-unicode"]: unicode_equivalent(k, v)
                     for k, v in CHARS_DATA.items()
                     if "unicode-equivalent" not in v 
                     or v["unicode-equivalent"] != v["wl-unicode"]}
WL_TO_UNICODE_RE = re_from_keys(WL_TO_UNICODE_DICT)

# Conversion from unicode to WL
UNICODE_TO_WL_DICT = {v["unicode-equivalent"]: v["wl-unicode"]
                     for v in CHARS_DATA.values()
                     if "unicode-equivalent" in v and v["has-unicode-inverse"]}
UNICODE_TO_WL_RE = re_from_keys(UNICODE_TO_WL_DICT)

##############################

# Character ranges of letters
letters = "a-zA-Z\u00c0-\u00d6\u00d8-\u00f6\u00f8-\u0103\u0106\u0107\
\u010c-\u010f\u0112-\u0115\u011a-\u012d\u0131\u0141\u0142\u0147\u0148\
\u0150-\u0153\u0158-\u0161\u0164\u0165\u016e-\u0171\u017d\u017e\
\u0391-\u03a1\u03a3-\u03a9\u03b1-\u03c9\u03d1\u03d2\u03d5\u03d6\
\u03da-\u03e1\u03f0\u03f1\u03f5\u210a-\u210c\u2110-\u2113\u211b\u211c\
\u2128\u212c\u212d\u212f-\u2131\u2133-\u2138\uf6b2-\uf6b5\uf6b7\uf6b9\
\uf6ba-\uf6bc\uf6be\uf6bf\uf6c1-\uf700\uf730\uf731\uf770\uf772\uf773\
\uf776\uf779\uf77a\uf77d-\uf780\uf782-\uf78b\uf78d-\uf78f\uf790\
\uf793-\uf79a\uf79c-\uf7a2\uf7a4-\uf7bd\uf800-\uf833\ufb01\ufb02"

# Character ranges of letterlikes
letterlikes = "".join(v["wl-unicode"] for v in CHARS_DATA.values()
                      if v["is-letter-like"])

# All supported named characters
named_characters = {k: v["wl-unicode"] for k, v in CHARS_DATA.items()}

def replace_wl_with_plain_text(wl_input: str, use_unicode=True) -> str:
    """
    WL uses some non-unicode character for various things.
    Replace them with the unicode equivalent.
    """
    r = WL_TO_UNICODE_RE if use_unicode else WL_TO_PLAIN_RE
    d = WL_TO_UNICODE_DICT if use_unicode else WL_TO_PLAIN_DICT

    return r.sub(lambda m: d[m.group(0)], wl_input)

def replace_unicode_with_wl(unicode_input: str) -> str:
    """
    WL uses some non-unicode character for various things.
    Replace their unicode equivalent with them.
    """
    return UNICODE_TO_WL_RE.sub(
        lambda m: UNICODE_TO_WL_DICT[m.group(0)], unicode_input
    )

