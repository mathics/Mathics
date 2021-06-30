# -*- coding: utf-8 -*-
"""
String Distances and Similarity Measures
"""

import unicodedata

from typing import Callable

from mathics.version import __version__  # noqa used in loading to check consistency.

from mathics.builtin.base import Builtin

from mathics.core.expression import (
    Expression,
    Integer,
    String,
    SymbolTrue,
)


# Levenshtein's algorithm is defined by the following construction:
# (adapted from https://de.wikipedia.org/wiki/Levenshtein-Distanz)
#
# given two strings s1, s2, we build a matrix D sized (len(s1) + 1,
# len(s2) + 1) and fill it using the following rules:
#
# (1) D(0, 0) = 0
# (2) D(i, 0) = i, 1 <= i <= len(s1)
# (3) D(0, j) = j, 1 <= j <= len(s2)
# (4) D(i, j) = minimum of
#     D(i - 1, j - 1) + 0 if s1(j) = s2(j)
#     D(i - 1, j - 1) + 1 (substitution)
#     D(i, j - 1) + 1     (insertion)
#     D(i - 1, j) + 1     (deletion)
#
# The computed distance will be in D(len(s1) + 1, len(s2) + 1).
#
# note: double brackets indicate 1-based indices below, e.g. s1[[1]]


def _one_based(l):  # makes an enumerated generator 1-based
    return ((i + 1, x) for i, x in l)


def _prev_curr(l):  # yields pairs of (x[i - 1], x[i]) for i in 1, 2, ...
    prev = None
    for curr in l:
        yield prev, curr
        prev = curr


def _levenshtein_d0(s2):  # compute D(0, ...)
    return list(range(len(s2) + 1))  # see (1), (3)


def _levenshtein_di(c1, s2, i, d_prev, sameQ, cost):  # compute one new row
    # given c1 = s1[i], s2, i, d_prev = D(i - 1, ...), compute D(i, ...)

    yield i  # start with D(i, 0) = i, see (2)
    d_curr_prev_j = i  # d_curr_prev_j stores D(i, j - 1)

    for j, c2 in _one_based(enumerate(s2)):  # c2 = s2[[j]]
        cond = 0 if sameQ(c1, c2) else cost

        d_curr_j = min(  # see (4)
            d_prev[j - 1] + cond,  # D(i - 1, j - 1) + cond; substitution
            d_curr_prev_j + 1,  # D(i, j - 1) + 1; insertion
            d_prev[j] + 1,
        )  # D(i - 1, j) + 1; deletion

        yield d_curr_j
        d_curr_prev_j = d_curr_j


def _levenshtein(s1, s2, sameQ: Callable[..., bool]):
    d_prev = _levenshtein_d0(s2)
    for i, c1 in _one_based(enumerate(s1)):  # c1 = s1[[i]]
        d_prev = list(_levenshtein_di(c1, s2, i, d_prev, sameQ, 1))
    return d_prev[-1]


def _damerau_levenshtein(s1, s2, sameQ: Callable[..., bool]):
    # _damerau_levenshtein works like _levenshtein, except for one additional
    # rule covering transposition:
    #
    # if i > 1 and j > 1 and a[i] == b[j - 1] and a[i - 1] == b[j] then
    #     D(i, j) = minimum(D(i, j), D(i - 2, j - 2) + transposition_cost)

    def row(d_prev_prev, d_prev, i, prev_c1, c1, cost):
        # given c1 = s1[i], d_prev_prev = D(i - 2), d_prev = D(i - 1),
        # prev_c1 = s1[[i - 1]], c1 = s1[[i]], compute D(i, ...)
        for j, d_curr_j in enumerate(_levenshtein_di(c1, s2, i, d_prev, sameQ, cost)):
            if i > 1 and j > 1:
                if sameQ(c1, s2[j - 2]) and sameQ(prev_c1, s2[j - 1]):  # transposition?
                    # i.e. if s1[[i]] = s2[[j-1]] and s1[[i-1]] = s2[[j]]
                    d_curr_j = min(d_curr_j, d_prev_prev[j - 2] + cost)
            yield d_curr_j

    d_prev_prev = None
    d_prev = _levenshtein_d0(s2)
    for i, (prev_c1, c1) in _one_based(enumerate(_prev_curr(s1))):
        d_curr = list(row(d_prev_prev, d_prev, i, prev_c1, c1, 1))
        d_prev_prev = d_prev
        d_prev = d_curr

    return d_prev[-1]


def _levenshtein_like_or_border_cases(s1, s2, sameQ: Callable[..., bool], compute):
    if len(s1) == len(s2) and all(sameQ(c1, c2) for c1, c2 in zip(s1, s2)):
        return 0

    if len(s1) < len(s2):
        s1, s2 = s2, s1

    if len(s2) == 0:
        return len(s1)

    return compute(s1, s2, sameQ)


class _StringDistance(Builtin):
    options = {"IgnoreCase": "False"}

    def apply(self, a, b, evaluation, options):
        "%(name)s[a_, b_, OptionsPattern[%(name)s]]"
        if isinstance(a, String) and isinstance(b, String):
            py_a = a.get_string_value()
            py_b = b.get_string_value()
            if options["System`IgnoreCase"] == SymbolTrue:
                if hasattr(str, "casefold"):

                    def normalize(c):
                        return unicodedata.normalize("NFKD", c.casefold())

                    py_a = [normalize(c) for c in py_a]
                    py_b = [normalize(c) for c in py_b]
                else:  # python2, PyPy
                    py_a = py_a.lower()
                    py_b = py_b.lower()
            return Integer(self._distance(py_a, py_b, lambda u, v: u == v))
        elif a.get_head_name() == "System`List" and b.get_head_name() == "System`List":
            return Integer(self._distance(a.leaves, b.leaves, lambda u, v: u.sameQ(v)))
        else:
            return Expression("EditDistance", a, b)


class DamerauLevenshteinDistance(_StringDistance):
    """
    <dl>
    <dt>'DamerauLevenshteinDistance[$a$, $b$]'
        <dd>returns the Damerau-Levenshtein distance of $a$ and $b$, which is defined as the minimum number of
        transpositions, insertions, deletions and substitutions needed to transform one into the other.
        In contrast to EditDistance, DamerauLevenshteinDistance counts transposition of adjacent items (e.g.
        "ab" into "ba") as one operation of change.
    </dl>

    >> DamerauLevenshteinDistance["kitten", "kitchen"]
     = 2

    >> DamerauLevenshteinDistance["abc", "ac"]
     = 1

    >> DamerauLevenshteinDistance["abc", "acb"]
     = 1

    >> DamerauLevenshteinDistance["azbc", "abxyc"]
     = 3

    The IgnoreCase option makes DamerauLevenshteinDistance ignore the case of letters:
    >> DamerauLevenshteinDistance["time", "Thyme"]
     = 3

    >> DamerauLevenshteinDistance["time", "Thyme", IgnoreCase -> True]
     = 2

    DamerauLevenshteinDistance also works on lists:
    >> DamerauLevenshteinDistance[{1, E, 2, Pi}, {1, E, Pi, 2}]
     = 1
    """

    def _distance(self, s1, s2, sameQ: Callable[..., bool]):
        return _levenshtein_like_or_border_cases(s1, s2, sameQ, _damerau_levenshtein)


class EditDistance(_StringDistance):
    """
    <dl>
    <dt>'EditDistance[$a$, $b$]'
        <dd>returns the Levenshtein distance of $a$ and $b$, which is defined as the minimum number of
        insertions, deletions and substitutions on the constituents of $a$ and $b$ needed to transform
        one into the other.
    </dl>

    >> EditDistance["kitten", "kitchen"]
     = 2

    >> EditDistance["abc", "ac"]
     = 1

    >> EditDistance["abc", "acb"]
     = 2

    >> EditDistance["azbc", "abxyc"]
     = 3

    The IgnoreCase option makes EditDistance ignore the case of letters:
    >> EditDistance["time", "Thyme"]
     = 3

    >> EditDistance["time", "Thyme", IgnoreCase -> True]
     = 2

    EditDistance also works on lists:
    >> EditDistance[{1, E, 2, Pi}, {1, E, Pi, 2}]
     = 2
    """

    def _distance(self, s1, s2, sameQ: Callable[..., bool]):
        return _levenshtein_like_or_border_cases(s1, s2, sameQ, _levenshtein)


class HammingDistance(Builtin):
    """
    <dl>
    <dt>'HammingDistance[$u$, $v$]'
      <dd>returns the Hamming distance between $u$ and $v$, i.e. the number of different elements.
      $u$ and $v$ may be lists or strings.
    </dl>

    >> HammingDistance[{1, 0, 1, 0}, {1, 0, 0, 1}]
    = 2

    >> HammingDistance["time", "dime"]
    = 1

    >> HammingDistance["TIME", "dime", IgnoreCase -> True]
    = 1
    """

    messages = {
        "idim": "`1` and `2` must be of same length.",
    }

    options = {
        "IgnoreCase": "False",
    }

    @staticmethod
    def _compute(u, v, sameQ, evaluation):
        if len(u) != len(v):
            evaluation.message("HammingDistance", "idim", u, v)
            return None
        else:
            return Integer(sum(0 if sameQ(x, y) else 1 for x, y in zip(u, v)))

    def apply_list(self, u, v, evaluation):
        "HammingDistance[u_List, v_List]"
        return HammingDistance._compute(
            u.leaves, v.leaves, lambda x, y: x.sameQ(y), evaluation
        )

    def apply_string(self, u, v, evaluation, options):
        "HammingDistance[u_String, v_String, OptionsPattern[HammingDistance]]"
        ignore_case = self.get_option(options, "IgnoreCase", evaluation)
        py_u = u.get_string_value()
        py_v = v.get_string_value()
        if ignore_case and ignore_case.is_true():
            py_u = py_u.lower()
            py_v = py_v.lower()
        return HammingDistance._compute(py_u, py_v, lambda x, y: x == y, evaluation)
