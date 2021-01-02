#!/usr/bin/env python3
# -*- coding: utf-8 -*-



import re
import sys
from itertools import chain

FORMAT_RE = re.compile(r'\`(\d*)\`')

WL_TO_UNICODE = {
  "Β": "Ꞵ",
  "Χ": "Ξ",
  "": "𝐷",
  "": "⌘",
  "": "𝕔",
  "": "⋱",
  "": "⨯",
  "∆": "Δ",
  "": "𝑑",
  "": "→",
  "": "ϴ",
  "ı": "I",
  "": "J",
  "": "⛶",
  "": "ℽ",
  "": "ℼ",
  "": "𝕒",
  "": "𝕓",
  "": "𝕔",
  "": "𝔸",
  "": "𝔹",
  "": "ℂ",
  "": "𝔻",
  "": "𝔼",
  "": "𝔽",
  "": "𝔾",
  "": "ℍ",
  "": "𝕀",
  "": "𝕁",
  "": "𝕂",
  "": "𝕃",
  "": "𝕄",
  "": "ℕ",
  "": "𝕆",
  "": "ℙ",
  "": "ℚ",
  "": "ℝ",
  "": "𝕊",
  "": "𝕋",
  "": "𝕌",
  "": "𝕍",
  "": "𝕎",
  "": "𝕏",
  "": "𝕐",
  "": "ℤ",
  "": "𝕕",
  "": "𝕖",
  "": "𝟠",
  "": "𝕗",
  "": "𝟝",
  "": "𝟜",
  "": "𝕘",
  "": "𝕙",
  "": "𝕚",
  "": "𝕛",
  "": "𝕜",
  "": "𝕝",
  "": "𝕞",
  "": "𝕟",
  "": "𝟡",
  "": "𝕠",
  "": "𝟙",
  "": "𝕡",
  "": "𝕢",
  "": "𝕣",
  "": "𝕤",
  "": "𝟟",
  "": "𝟞",
  "": "𝕥",
  "": "𝟛",
  "": "𝟚",
  "": "𝕦",
  "": "𝕧",
  "": "𝕨",
  "": "𝕩",
  "": "𝕪",
  "": "𝕫",
  "": "𝟘",
  "♁": "⊕",
  "": "⩵",
  "⧦": "⇔",
  "": "ⅇ",
  "": "•",
  "◼": "■",
  "": "ạ",
  "": "α̣",
  "": "ḅ",
  "": "β̣",
  "": "c̣",
  "": "Ạ",
  "": "Α̣",
  "": "Ḅ",
  "": "Β̣",
  "": "C̣",
  "": "Χ̣",
  "": "Ḍ",
  "": "Δ̣",
  "": "Ϝ̣",
  "": "Ẹ",
  "": "Ε̣",
  "": "Η̣",
  "": "F̣",
  "": "G̣",
  "": "Γ",
  "": "Ḥ",
  "": "Ị",
  "": "Ι̣",
  "": "J̣",
  "": "Ḳ",
  "": "Κ̣",
  "": "Ϟ̣",
  "": "Ḷ",
  "": "Λ̣",
  "": "Ṃ",
  "": "Μ̣",
  "": "Ṇ",
  "": "Ν̣",
  "": "Ọ",
  "": "Ω̣",
  "": "Ο̣",
  "": "P̣",
  "": "Φ̣",
  "": "Π̣",
  "": "Ψ̣",
  "": "Q̣",
  "": "Ṛ",
  "": "Ρ̣",
  "": "Ṣ",
  "": "Ϡ̣",
  "": "Σ̣",
  "": "Ϛ̣",
  "": "Ṭ",
  "": "Τ̣",
  "": "Θ",
  "": "Ụ",
  "": "Υ̣",
  "": "Ṿ",
  "": "Ẉ",
  "": "X̣",
  "": "Ξ̣",
  "": "Ỵ",
  "": "Ẓ",
  "": "Ζ̣",
  "": "χ̣",
  "": "ϒ̣",
  "": "ε̣",
  "": "ϰ̣",
  "": "φ̣",
  "": "ϖ̣",
  "": "ϱ̣",
  "": "ϑ̣",
  "": "ḍ",
  "": "δ̣",
  "": "ϝ",
  "": "ẹ",
  "": "ϵ̣",
  "": "η̣",
  "": "f̣",
  "": "ς̣",
  "": "g̣",
  "": "γ̣",
  "": "ḥ",
  "": "ị",
  "": "Ι̣",
  "": "j̣",
  "": "ḳ",
  "": "κ̣",
  "": "ϟ̣",
  "": "ḷ",
  "": "λ̣",
  "": "ṃ",
  "": "μ̣",
  "": "ṇ",
  "": "ν̣",
  "": "ọ",
  "": "ω̣",
  "": "ο̣",
  "": "p̣",
  "": "ϕ̣",
  "": "π̣",
  "": "ψ̣",
  "": "q̣",
  "": "ṛ",
  "": "ρ̣",
  "": "ṣ",
  "": "ϡ̣",
  "": "σ̣",
  "": "ϛ",
  "": "ṭ",
  "": "τ̣",
  "": "θ̣",
  "": "ụ",
  "": "υ̣",
  "": "ṿ",
  "": "ẉ",
  "": "x̣",
  "": "ξ̣",
  "": "ỵ",
  "": "ẓ",
  "": "ζ̣",
  "": "↦",
  "": "𝔞",
  "": "𝔟",
  "": "𝔠",
  "": "𝔄",
  "": "𝔅",
  "": "𝔇",
  "": "𝔈",
  "": "𝔉",
  "": "𝔊",
  "": "𝔍",
  "": "𝔎",
  "": "𝔏",
  "": "𝔐",
  "": "𝔑",
  "": "𝔒",
  "": "𝔓",
  "": "𝔔",
  "": "𝔖",
  "": "𝔗",
  "": "𝔘",
  "": "𝔙",
  "": "𝔚",
  "": "𝔛",
  "": "𝔜",
  "": "𝔡",
  "": "𝔢",
  "": "𝔣",
  "": "𝔤",
  "": "𝔥",
  "": "𝔦",
  "": "𝔧",
  "": "𝔨",
  "": "𝔩",
  "": "𝔪",
  "": "𝔫",
  "": "𝔬",
  "": "𝔭",
  "": "𝔮",
  "": "𝔯",
  "": "𝔰",
  "": "𝔱",
  "": "𝔲",
  "": "𝔳",
  "": "𝔴",
  "": "𝔵",
  "": "𝔶",
  "": "𝔷",
  "": "●",
  "": "■",
  "": "ⅈ",
  "": "ⅉ",
  "": "⟹",
  "": "|",
  "": "‖",
  "": "«",
  "": "=",
  "": "#",
  "": "⊙",
  "": "⎕",
  "": "|",
  "": "‖",
  "": "»",
  "": "→",
  "": "⧴",
  "Ϡ": "ϡ",
  "": "𝒶",
  "": "𝒷",
  "": "𝒸",
  "": "𝒜",
  "": "𝒞",
  "": "𝒟",
  "": "𝒢",
  "": "𝒥",
  "": "𝒦",
  "": "𝒩",
  "": "𝒪",
  "℘": "𝒫",
  "": "𝒬",
  "": "𝒮",
  "": "𝒯",
  "": "𝒰",
  "": "𝒱",
  "": "𝒲",
  "": "𝒳",
  "": "𝒴",
  "": "𝒵",
  "": "𝒹",
  "": "𝒾",
  "": "𝒿",
  "": "𝒻",
  "": "𝒽",
  "": "𝒾",
  "": "𝒿",
  "": "𝓀",
  "": "𝓂",
  "": "𝓃",
  "": "𝓅",
  "": "𝓆",
  "": "𝓇",
  "": "𝓈",
  "": "𝓉",
  "": "𝓊",
  "": "𝓋",
  "": "𝓌",
  "": "𝓍",
  "": "𝓎",
  "": "𝓏",
  "": "↓",
  "": "←",
  "": "→",
  "": "↑",
  "": "⋮",
  "": "⋱",
  "": "⋯",
  "": "▫",
  "": "⊗",
  "": "ᵀ",
  "": "⋯",
  "": "↔",
  "♅": "⛢",
  "": "|"
}

UNICODE_TO_WL = {
  "Ꞵ": "Β",
  "Ξ": "Χ",
  "𝐷": "",
  "⌘": "",
  "𝕔": "",
  "⨯": "",
  "Δ": "∆",
  "𝑑": "",
  "→": "",
  "ϴ": "",
  "I": "ı",
  "J": "",
  "⛶": "",
  "ℽ": "",
  "ℼ": "",
  "𝕒": "",
  "𝕓": "",
  "𝔸": "",
  "𝔹": "",
  "ℂ": "",
  "𝔻": "",
  "𝔼": "",
  "𝔽": "",
  "𝔾": "",
  "ℍ": "",
  "𝕀": "",
  "𝕁": "",
  "𝕂": "",
  "𝕃": "",
  "𝕄": "",
  "ℕ": "",
  "𝕆": "",
  "ℙ": "",
  "ℚ": "",
  "ℝ": "",
  "𝕊": "",
  "𝕋": "",
  "𝕌": "",
  "𝕍": "",
  "𝕎": "",
  "𝕏": "",
  "𝕐": "",
  "ℤ": "",
  "𝕕": "",
  "𝕖": "",
  "𝟠": "",
  "𝕗": "",
  "𝟝": "",
  "𝟜": "",
  "𝕘": "",
  "𝕙": "",
  "𝕚": "",
  "𝕛": "",
  "𝕜": "",
  "𝕝": "",
  "𝕞": "",
  "𝕟": "",
  "𝟡": "",
  "𝕠": "",
  "𝟙": "",
  "𝕡": "",
  "𝕢": "",
  "𝕣": "",
  "𝕤": "",
  "𝟟": "",
  "𝟞": "",
  "𝕥": "",
  "𝟛": "",
  "𝟚": "",
  "𝕦": "",
  "𝕧": "",
  "𝕨": "",
  "𝕩": "",
  "𝕪": "",
  "𝕫": "",
  "𝟘": "",
  "⊕": "♁",
  "⩵": "",
  "⇔": "⧦",
  "ⅇ": "",
  "•": "",
  "ạ": "",
  "α̣": "",
  "ḅ": "",
  "β̣": "",
  "c̣": "",
  "Ạ": "",
  "Α̣": "",
  "Ḅ": "",
  "Β̣": "",
  "C̣": "",
  "Χ̣": "",
  "Ḍ": "",
  "Δ̣": "",
  "Ϝ̣": "",
  "Ẹ": "",
  "Ε̣": "",
  "Η̣": "",
  "F̣": "",
  "G̣": "",
  "Γ": "",
  "Ḥ": "",
  "Ị": "",
  "Ι̣": "",
  "J̣": "",
  "Ḳ": "",
  "Κ̣": "",
  "Ϟ̣": "",
  "Ḷ": "",
  "Λ̣": "",
  "Ṃ": "",
  "Μ̣": "",
  "Ṇ": "",
  "Ν̣": "",
  "Ọ": "",
  "Ω̣": "",
  "Ο̣": "",
  "P̣": "",
  "Φ̣": "",
  "Π̣": "",
  "Ψ̣": "",
  "Q̣": "",
  "Ṛ": "",
  "Ρ̣": "",
  "Ṣ": "",
  "Ϡ̣": "",
  "Σ̣": "",
  "Ϛ̣": "",
  "Ṭ": "",
  "Τ̣": "",
  "Θ": "",
  "Ụ": "",
  "Υ̣": "",
  "Ṿ": "",
  "Ẉ": "",
  "X̣": "",
  "Ξ̣": "",
  "Ỵ": "",
  "Ẓ": "",
  "Ζ̣": "",
  "χ̣": "",
  "ϒ̣": "",
  "ε̣": "",
  "ϰ̣": "",
  "φ̣": "",
  "ϖ̣": "",
  "ϱ̣": "",
  "ϑ̣": "",
  "ḍ": "",
  "δ̣": "",
  "ϝ": "",
  "ẹ": "",
  "ϵ̣": "",
  "η̣": "",
  "f̣": "",
  "ς̣": "",
  "g̣": "",
  "γ̣": "",
  "ḥ": "",
  "ị": "",
  "j̣": "",
  "ḳ": "",
  "κ̣": "",
  "ϟ̣": "",
  "ḷ": "",
  "λ̣": "",
  "ṃ": "",
  "μ̣": "",
  "ṇ": "",
  "ν̣": "",
  "ọ": "",
  "ω̣": "",
  "ο̣": "",
  "p̣": "",
  "ϕ̣": "",
  "π̣": "",
  "ψ̣": "",
  "q̣": "",
  "ṛ": "",
  "ρ̣": "",
  "ṣ": "",
  "ϡ̣": "",
  "σ̣": "",
  "ϛ": "",
  "ṭ": "",
  "τ̣": "",
  "θ̣": "",
  "ụ": "",
  "υ̣": "",
  "ṿ": "",
  "ẉ": "",
  "x̣": "",
  "ξ̣": "",
  "ỵ": "",
  "ẓ": "",
  "ζ̣": "",
  "↦": "",
  "𝔞": "",
  "𝔟": "",
  "𝔠": "",
  "𝔄": "",
  "𝔅": "",
  "𝔇": "",
  "𝔈": "",
  "𝔉": "",
  "𝔊": "",
  "𝔍": "",
  "𝔎": "",
  "𝔏": "",
  "𝔐": "",
  "𝔑": "",
  "𝔒": "",
  "𝔓": "",
  "𝔔": "",
  "𝔖": "",
  "𝔗": "",
  "𝔘": "",
  "𝔙": "",
  "𝔚": "",
  "𝔛": "",
  "𝔜": "",
  "𝔡": "",
  "𝔢": "",
  "𝔣": "",
  "𝔤": "",
  "𝔥": "",
  "𝔦": "",
  "𝔧": "",
  "𝔨": "",
  "𝔩": "",
  "𝔪": "",
  "𝔫": "",
  "𝔬": "",
  "𝔭": "",
  "𝔮": "",
  "𝔯": "",
  "𝔰": "",
  "𝔱": "",
  "𝔲": "",
  "𝔳": "",
  "𝔴": "",
  "𝔵": "",
  "𝔶": "",
  "𝔷": "",
  "●": "",
  "ⅈ": "",
  "ⅉ": "",
  "⟹": "",
  "|": "",
  "«": "",
  "=": "",
  "#": "",
  "⊙": "",
  "⎕": "",
  "»": "",
  "⧴": "",
  "ϡ": "Ϡ",
  "𝒶": "",
  "𝒷": "",
  "𝒸": "",
  "𝒜": "",
  "𝒞": "",
  "𝒟": "",
  "𝒢": "",
  "𝒥": "",
  "𝒦": "",
  "𝒩": "",
  "𝒪": "",
  "𝒫": "℘",
  "𝒬": "",
  "𝒮": "",
  "𝒯": "",
  "𝒰": "",
  "𝒱": "",
  "𝒲": "",
  "𝒳": "",
  "𝒴": "",
  "𝒵": "",
  "𝒹": "",
  "𝒾": "",
  "𝒿": "",
  "𝒻": "",
  "𝒽": "",
  "𝓀": "",
  "𝓂": "",
  "𝓃": "",
  "𝓅": "",
  "𝓆": "",
  "𝓇": "",
  "𝓈": "",
  "𝓉": "",
  "𝓊": "",
  "𝓋": "",
  "𝓌": "",
  "𝓍": "",
  "𝓎": "",
  "𝓏": "",
  "↓": "",
  "←": "",
  "↑": "",
  "▫": "",
  "⊗": "",
  "ᵀ": "",
  "↔": "",
  "⛢": "♅"
}

WL_REPLACE_DICT = dict((re.escape(k), v) for k, v in WL_TO_UNICODE.items())
WL_REPLACE_RE = re.compile("|".join(WL_REPLACE_DICT.keys()))
UNICODE_REPLACE_DICT = dict((re.escape(k), v) for k, v in UNICODE_TO_WL.items())
UNICODE_REPLACE_RE = re.compile(
    "|".join(sorted(UNICODE_REPLACE_DICT.keys(), key=lambda k: (-len(k), k)))
)

def interpolate_string(text, get_param) -> str:
    index = [1]

    def get_item(index):
        if 1 <= index <= len(args):
            return args[index - 1]
        else:
            return ''

    if isinstance(get_param, list):
        args = get_param
        get_param = get_item

    def repl(match):
        arg = match.group(1)
        if arg == '' or arg == '0':
            arg = index[0]
        else:
            arg = int(arg)
        index[0] += 1
        param = get_param(arg)
        return param
    return FORMAT_RE.sub(repl, text)

"""
NOTE: Maybe see
http://www.cosc.canterbury.ac.nz/tad.takaoka/isaac.pdf
resp.
http://www.cosc.canterbury.ac.nz/tad.takaoka/perm.p
for a permutation generating algorithm for multisets.
"""


def permutations(items, without_duplicates=True):
    if not items:
        yield []
    # already_taken = set()
    # first yield identical permutation without recursion
    yield items
    for index in range(len(items)):
        item = items[index]
        # if item not in already_taken:
        for sub in permutations(items[:index] + items[index + 1:]):
            yield [item] + sub
            # already_taken.add(item)


def subsets(items, min, max, included=None, less_first=False):
    if max is None:
        max = len(items)
    lengths = list(range(min, max + 1))
    if not less_first:
        lengths = reversed(lengths)
    lengths = list(lengths)
    if lengths and lengths[0] == 0:
        lengths = lengths[1:] + [0]

    def decide(chosen, not_chosen, rest, count):
        if count < 0 or len(rest) < count:
            return
        if count == 0:
            yield chosen, list(chain(not_chosen, rest))
        elif len(rest) == count:
            if included is None or all(item in included for item in rest):
                yield list(chain(chosen, rest)), not_chosen
        elif rest:
            item = rest[0]
            if included is None or item in included:
                for set in decide(chosen + [item], not_chosen, rest[1:],
                                  count - 1):
                    yield set
            for set in decide(chosen, not_chosen + [item], rest[1:], count):
                yield set

    for length in lengths:
        for chosen, not_chosen in decide([], [], items, length):
            yield chosen, ([], not_chosen)


def subsets_2(items, min, max, without_duplicates=True):
    """ max may only be 1 or None (= infinity).
    Respects include property of items
    """

    if min <= max == 1:
        for index in range(len(items)):
            if items[index].include:
                yield [items[index]], ([], items[:index] + items[index + 1:])
        if min == 0:
            yield [], ([], items)
    else:
        counts = {}
        for item in items:
            if item.include:
                if item in counts:
                    counts[item] += 1
                else:
                    counts[item] = 1
        already = set()

        def decide(chosen, not_chosen, rest):
            if not rest:
                if len(chosen) >= min:
                    """if False and len(chosen) > 1 and (
                            permutate_until is None or
                            len(chosen) <= permutate_until):
                        for perm in permutations(chosen):
                            yield perm, ([], not_chosen)
                    else:"""
                    yield chosen, ([], not_chosen)
            else:
                if rest[0].include:
                    for set in decide(chosen + [rest[0]], not_chosen,
                                      rest[1:]):
                        yield set
                for set in decide(chosen, not_chosen + [rest[0]], rest[1:]):
                    yield set
        for subset in decide([], [], list(counts.keys())):
            t = tuple(subset[0])
            if t not in already:
                yield subset
                already.add(t)
            else:
                print('already taken')


def subranges(items, min_count, max, flexible_start=False, included=None,
              less_first=False):
    # TODO: take into account included

    if max is None:
        max = len(items)
    max = min(max, len(items))
    if flexible_start:
        starts = list(range(len(items) - max + 1))
    else:
        starts = (0,)
    for start in starts:
        lengths = list(range(min_count, max + 1))
        if not less_first:
            lengths = reversed(lengths)
        lengths = list(lengths)
        if lengths == [0, 1]:
            lengths = [1, 0]
        for length in lengths:
            yield (items[start:start + length],
                   (items[:start], items[start + length:]))


def unicode_superscript(value) -> str:
    def repl_char(c):
        if c == '1':
            value = 185
        elif c == '2':
            value = 178
        elif c == '3':
            value = 179
        elif '0' <= c <= '9':
            value = 8304 + (ord(c) - ord('0'))
        elif c == '-':
            value = 8315
        elif c == '(':
            value = 8317
        elif c == ')':
            value = 8318
        else:
            value = ord(c)
        return chr(value)
    return ''.join(repl_char(c) for c in value)


try:
    from inspect import signature

    def _python_function_arguments(f):
        return signature(f).parameters.keys()
except ImportError:  # py2, pypy
    from inspect import getargspec

    def _python_function_arguments(f):
        return getargspec(f).args

if sys.version_info >= (3, 4, 0):
    _cython_function_arguments = _python_function_arguments
elif sys.version_info[0] >= 3:  # py3.3
    def _cython_function_arguments(f):
        return f.__code__.co_varnames
else:  # py2
    def _cython_function_arguments(f):
        return f.func_code.co_varnames


def function_arguments(f):
    try:
        return _python_function_arguments(f)
    except (TypeError, ValueError):
        return _cython_function_arguments(f)

def robust_min(iterable):
    minimum = None
    for i in iterable:
        if minimum is None or i < minimum:
            minimum = i
    return minimum

def replace_wl_with_unicode(wl_input: str) -> str:
    """WL uses some non-unicode character for various things.
    Replace them with the unicode equivalent.
    """
    return WL_REPLACE_RE.sub(
        lambda m: WL_REPLACE_DICT[re.escape(m.group(0))], wl_input
    )

def replace_unicode_with_wl(unicode_input: str) -> str:
    """WL uses some non-unicode character for various things.
    Replace their unicode equivalent with them.
    """
    return UNICODE_REPLACE_RE.sub(
        lambda m: UNICODE_REPLACE_DICT[re.escape(m.group(0))], unicode_input
    )


