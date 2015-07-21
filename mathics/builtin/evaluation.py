# -*- coding: utf8 -*-

import sys

from mathics.builtin.base import Predefined, Builtin
from mathics.core.expression import Integer

from mathics import settings


def set_recursionlimit(n):
    "Sets the required python recursion limit given $RecursionLimit value"
    def conversion(m):
        return 200 + 5 * m
    sys.setrecursionlimit(conversion(n))
    if sys.getrecursionlimit() != conversion(n):
        raise OverflowError


class RecursionLimit(Predefined):
    """
    >> a = a + a
     : Recursion depth of 200 exceeded.
     = $Aborted
    >> $RecursionLimit
     = 200

    >> $RecursionLimit = x;
     : Cannot set $RecursionLimit to x; value must be an integer between 20 and 512.

    >> $RecursionLimit = 512
     = 512
    >> a = a + a
     : Recursion depth of 512 exceeded.
     = $Aborted

    #> $RecursionLimit = 20
     = 20
    #> a = a + a
     : Recursion depth of 20 exceeded.
     = $Aborted

    #> $RecursionLimit = 200
     = 200

    """

    name = '$RecursionLimit'
    value = 200

    set_recursionlimit(value)

    rules = {
        '$RecursionLimit': str(value),
    }

    messages = {
        'reclim': "Recursion depth of `1` exceeded.",
        'limset': (
            "Cannot set $RecursionLimit to `1`; "
            "value must be an integer between 20 and %d.") % (
                settings.MAX_RECURSION_DEPTH),
    }

    rules = {
        '$RecursionLimit': str(value),
    }

    def evaluate(self, evaluation):
        return Integer(self.value)


class Hold(Builtin):
    """
    >> Attributes[Hold]
     = {HoldAll, Protected}
    """

    attributes = ('HoldAll',)


class HoldComplete(Builtin):
    """
    >> Attributes[HoldComplete]
     = {HoldAllComplete, Protected}
    """

    attributes = ('HoldAllComplete',)


class HoldForm(Builtin):
    """
    'HoldForm[$expr$]' maintains $expr$ in an unevaluated form, but prints as $expr$.

    >> HoldForm[1 + 2 + 3]
     = 1 + 2 + 3

    'HoldForm' has attribute 'HoldAll':
    >> Attributes[HoldForm]
     = {HoldAll, Protected}
    """

    attributes = ('HoldAll',)

    rules = {
        'MakeBoxes[HoldForm[expr_], f_]': 'MakeBoxes[expr, f]',
    }


class Evaluate(Builtin):
    """
    >> SetAttributes[f, HoldAll]
    >> f[1 + 2]
     = f[1 + 2]
    >> f[Evaluate[1 + 2]]
     = f[3]

    >> Hold[Evaluate[1 + 2]]
     = Hold[3]
    >> HoldComplete[Evaluate[1 + 2]]
     = HoldComplete[Evaluate[1 + 2]]
    >> Evaluate[Sequence[1, 2]]
     = Sequence[1, 2]
    """

    rules = {
        'Evaluate[Unevaluated[x_]]': 'Unevaluated[x]',
        'Evaluate[x___]': 'x',
    }


class Unevaluated(Builtin):
    """
    >> Length[Unevaluated[1+2+3+4]]
     = 4
    'Unevaluated' has attribute 'HoldAllComplete':
    >> Attributes[Unevaluated]
     = {HoldAllComplete, Protected}

    'Unevaluated' is maintained for arguments to non-executed functions:
    >> f[Unevaluated[x]]
     = f[Unevaluated[x]]
    Likewise, its kept in flattened arguments and sequences:
    >> Attributes[f] = {Flat};
    >> f[a, Unevaluated[f[b, c]]]
     = f[a, Unevaluated[b], Unevaluated[c]]
    >> g[a, Sequence[Unevaluated[b], Unevaluated[c]]]
     = g[a, Unevaluated[b], Unevaluated[c]]
    However, unevaluated sequences are kept:
    >> g[Unevaluated[Sequence[a, b, c]]]
     = g[Unevaluated[Sequence[a, b, c]]]

    #> Attributes[h] = Flat;
    #> h[items___] := Plus[items]
    #> h[1, Unevaluated[Sequence[Unevaluated[2], 3]], Sequence[4, Unevaluated[5]]]
     = 15
    """

    attributes = ('HoldAllComplete',)


class ReleaseHold(Builtin):
    """
    <dl>
    <dt>'ReleaseHold[$expr$]'
    <dd>removes any 'Hold', 'HoldForm', 'HoldPattern' or 'HoldComplete' head from $expr$.
    </dl>
    >> x = 3;
    >> Hold[x]
     = Hold[x]
    >> ReleaseHold[Hold[x]]
     = 3
    >> ReleaseHold[y]
     = y
    """

    rules = {
        'ReleaseHold[(Hold|HoldForm|HoldPattern|HoldComplete)[expr_]]': 'expr',
        'ReleaseHold[other_]': 'other',
    }


class Sequence(Builtin):
    """
    <dl>
    <dt>'Sequence[$x1$, $x2$, ...]'
        <dd>represents a sequence of arguments to a function.
    </dl>

    'Sequence' is automatically spliced in, except when a function has attribute 'SequenceHold'
    (like assignment functions).
    >> f[x, Sequence[a, b], y]
     = f[x, a, b, y]
    >> Attributes[Set]
     = {HoldFirst, Protected, SequenceHold}
    >> a = Sequence[b, c];
    >> a
     = Sequence[b, c]

    Apply 'Sequence' to a list to splice in arguments:
    >> list = {1, 2, 3};
    >> f[Sequence @@ list]
     = f[1, 2, 3]

    Inside 'Hold' or a function with a held argument, 'Sequence' is
    spliced in at the first level of the argument:
    >> Hold[a, Sequence[b, c], d]
     = Hold[a, b, c, d]
    If 'Sequence' appears at a deeper level, it is left unevaluated:
    >> Hold[{a, Sequence[b, c], d}]
     = Hold[{a, Sequence[b, c], d}]
    """


class Line(Builtin):
    """
    >> $Line
     = 1
    >> $Line
     = 2
    >> $Line = 12;
    >> 2 * 5
     = 10
    >> Out[13]
     = 10
    >> $Line = -1;
     : Non-negative integer expected.
    """

    name = '$Line'


class HistoryLength(Builtin):
    """
    >> $HistoryLength
     = 100
    >> $HistoryLength = 1;
    >> 42
     = 42
    >> %
     = 42
    >> %%
     = %3
    >> $HistoryLength = 0;
    >> 42
     = 42
    >> %
     = %7
    """

    name = '$HistoryLength'

    rules = {
        '$HistoryLength': '100',
    }


class In(Builtin):
    """
    >> x = 1
     = 1
    >> x = x + 1
     = 2
    >> Do[In[2], {3}]
    >> x
     = 5
    >> In[-1]
     = 5
    >> Definition[In]
     = Attributes[In] = {Protected}
     .
     . In[6] = Definition[In]
     .
     . In[5] = In[-1]
     .
     . In[4] = x
     .
     . In[3] = Do[In[2], {3}]
     .
     . In[2] = x = x + 1
     .
     . In[1] = x = 1
    """

    rules = {
        'In[k_Integer?Negative]': 'In[$Line + k]',
    }


class Out(Builtin):
    """
    <dl>
    <dt>'Out[$k$]' or '%$k$'
        <dd>gives the result of the $k$th input line.
    <dt>'%', '%%', etc.
        <dd>gives the result of the previous input line, of the line before the previous input line, etc.
    </dl>

    >> 42
     = 42
    >> %
     = 42
    >> 43;
    >> %
    >> 44
     = 44
    >> %1
     = 42
    >> %%
     = 44
    >> Hold[Out[-1]]
     = Hold[%]
    >> Hold[%4]
     = Hold[%4]
    >> Out[0]
     = Out[0]

    #> 10
     = 10
    #> Out[-1] + 1
     = 11
    #> Out[] + 1
     = 12
    """

    rules = {
        'Out[k_Integer?Negative]': 'Out[$Line + k]',
        'Out[]': 'Out[$Line - 1]',
        'MakeBoxes[Out[k_Integer?((-10 <= # < 0)&)],'
        '    f:StandardForm|TraditionalForm|InputForm|OutputForm]':
        r'StringJoin[ConstantArray["%%", -k]]',
        'MakeBoxes[Out[k_Integer?Positive],'
        '    f:StandardForm|TraditionalForm|InputForm|OutputForm]':
        r'"%%" <> ToString[k]',
    }
