#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import re
import yaml
import ujson
import os

from mathics.settings import ROOT_DIR

####### INITIALIZATION #######
# FIXME: Move this to an install-time script

def re_from_keys(d: dict) -> str:
    """Returns a regex that matches any of the keys of the dictionary"""

    # The keys are sorted to prevent shorter keys from obscuring longer keys 
    # when pattern matching
    return "|".join(
        sorted(map(re.escape, d.keys()), key=lambda k: (-len(k), k))
    )

def compile_tables(data: dict) -> dict:
    """
    Compiles the general table into the tables used internally by the library 
    for fast access
    """

    # Conversion from WL to the fully qualified names
    wl_to_ascii_dict = {v["wl-unicode"]: f"\\[{k}]" for k, v in data.items()}
    wl_to_ascii_re = re_from_keys(wl_to_ascii_dict)

    # Conversion from wl to unicode
    wl_to_unicode_dict = {v["wl-unicode"]: v.get("unicode-equivalent") or f"\\[{k}]"
                         for k, v in data.items()
                         if "unicode-equivalent" not in v 
                         or v["unicode-equivalent"] != v["wl-unicode"]}
    wl_to_unicode_re = re_from_keys(wl_to_unicode_dict)

    # Conversion from unicode to wl
    unicode_to_wl_dict = {v["unicode-equivalent"]: v["wl-unicode"]
                         for v in data.values()
                         if "unicode-equivalent" in v 
                         and v["has-unicode-inverse"]}
    unicode_to_wl_re = re_from_keys(unicode_to_wl_dict)

    # Character ranges of letterlikes
    letterlikes = "".join(v["wl-unicode"] for v in data.values()
                          if v["is-letter-like"])

    # All supported named characters
    named_characters = {k: v["wl-unicode"] for k, v in data.items()}

    # ESC sequence aliases
    aliased_characters = {v["esc-alias"]: v["wl-unicode"] 
                         for v in data.values() if "esc-alias" in v}

    return {
        "wl-to-ascii-dict": wl_to_ascii_dict,
        "wl-to-ascii-re": wl_to_ascii_re,
        "wl-to-unicode-dict": wl_to_unicode_dict,
        "wl-to-unicode-re": wl_to_unicode_re,
        "unicode-to-wl-dict": unicode_to_wl_dict,
        "unicode-to-wl-re": unicode_to_wl_re,
        "letterlikes": letterlikes,
        "named-characters": named_characters,
        "aliased-characters": aliased_characters,
    }

# Process the data and dump it to disk
with open(os.path.join(ROOT_DIR, "data/named-characters.yml"), "r") as i, open(os.path.join(ROOT_DIR, "data/characters.json"), "w") as o:
    # Load the YAML data
    data = yaml.load(i, Loader=yaml.FullLoader) 

    # Precompile the tables
    data = compile_tables(data)

    # Dump the proprocessed dictioanries to disk as JSON
    ujson.dump(data, o)

##############################

# Load the conversion tables from disk
with open(os.path.join(ROOT_DIR, "data/characters.json"), "r") as f:
    _data = ujson.load(f)

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

# Conversion from WL to the fully qualified names
wl_to_ascii = _data["wl-to-ascii-dict"]
_wl_to_ascii_re = re.compile(_data["wl-to-ascii-re"])

# Conversion from WL to unicode
wl_to_unicode = _data["wl-to-unicode-dict"]
_wl_to_unicode_re = re.compile(_data["wl-to-unicode-re"])

# Conversion from unicode to WL
unicode_to_wl = _data["unicode-to-wl-dict"]
_unicode_to_wl_re = re.compile(_data["unicode-to-wl-re"])

# Character ranges of letterlikes
letterlikes = _data["letterlikes"]

# All supported named characters
named_characters = _data["named-characters"]

# ESC sequence aliases
aliased_characters = _data["aliased-characters"]

def replace_wl_with_plain_text(wl_input: str, use_unicode=True) -> str:
    """
    WL uses some non-unicode character for various things.
    Replace them with the unicode equivalent.
    """
    r = _wl_to_unicode_re if use_unicode else _wl_to_ascii_re
    d = wl_to_unicode if use_unicode else wl_to_ascii

    return r.sub(lambda m: d[m.group(0)], wl_input)

def replace_unicode_with_wl(unicode_input: str) -> str:
    """
    WL uses some non-unicode character for various things.
    Replace their unicode equivalent with them.
    """
    return _unicode_to_wl_re.sub(
        lambda m: unicode_to_wl[m.group(0)], unicode_input
    )

