# -*- coding: utf-8 -*-
"""
Sums, Simple Statistics

These functions perform a simple arithmetic computation over a list.
"""

from mathics.version import __version__  # noqa used in loading to check consistency.

from mathics.builtin.base import Builtin


class Accumulate(Builtin):
    """
    <dl>
    <dt>'Accumulate[$list$]'
        <dd>accumulates the values of $list$, returning a new list.
    </dl>

    >> Accumulate[{1, 2, 3}]
     = {1, 3, 6}
    """

    rules = {"Accumulate[head_]": "FoldList[Plus, head]"}


class Mean(Builtin):
    """
    <dl>
    <dt>'Mean[$list$]'
      <dd>returns the statistical mean of $list$.
    </dl>

    >> Mean[{26, 64, 36}]
     = 42

    >> Mean[{1, 1, 2, 3, 5, 8}]
     = 10 / 3

    >> Mean[{a, b}]
     = (a + b) / 2
    """

    rules = {
        "Mean[list_]": "Total[list] / Length[list]",
    }


class Total(Builtin):
    """
    <dl>
    <dt>'Total[$list$]'
        <dd>adds all values in $list$.
    <dt>'Total[$list$, $n$]'
        <dd>adds all values up to level $n$.
    <dt>'Total[$list$, {$n$}]'
        <dd>totals only the values at level {$n$}.
    <dt>'Total[$list$, {$n_1$, $n_2$}]'
        <dd>totals at levels {$n_1$, $n_2$}.
    </dl>

    >> Total[{1, 2, 3}]
     = 6
    >> Total[{{1, 2, 3}, {4, 5, 6}, {7, 8 ,9}}]
     = {12, 15, 18}

    Total over rows and columns
    >> Total[{{1, 2, 3}, {4, 5, 6}, {7, 8 ,9}}, 2]
     = 45

    Total over rows instead of columns
    >> Total[{{1, 2, 3}, {4, 5, 6}, {7, 8 ,9}}, {2}]
     = {6, 15, 24}
    """

    rules = {
        "Total[head_]": "Apply[Plus, head]",
        "Total[head_, n_]": "Apply[Plus, Flatten[head, n]]",
    }
