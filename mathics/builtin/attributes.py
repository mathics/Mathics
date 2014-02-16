# -*- coding: utf8 -*-

r"""
Attributes

There are several builtin-attributes which have a predefined meaning in \Mathics.
However, you can set any symbol as an attribute, in contrast to \Mathematica.
"""

from mathics.builtin.base import Predefined, Builtin
from mathics.core.expression import Symbol, Expression
from mathics.builtin.assignment import get_symbol_list


class Attributes(Builtin):
    u"""
    >> Attributes[Plus]
     = {Flat, Listable, NumericFunction, OneIdentity, Orderless, Protected}
    'Attributes' always considers the head of an expression:
    >> Attributes[a + b + c]
     = {Flat, Listable, NumericFunction, OneIdentity, Orderless, Protected}

    You can assign values to 'Attributes' to set attributes:
    >> Attributes[f] = {Flat, Orderless}
     = {Flat, Orderless}
    >> f[b, f[a, c]]
     = f[a, b, c]
    Attributes must be symbols:
    >> Attributes[f] := {a + b}
     : Argument a + b at position 1 is expected to be a symbol.
     = $Failed
    Use 'Symbol' to convert strings to symbols:
    >> Attributes[f] = Symbol["Listable"]
     = Listable
    >> Attributes[f]
     = {Listable}
    """

    attributes = ('HoldAll', 'Listable')

    def apply(self, expr, evaluation):
        'Attributes[expr_]'

        name = expr.get_lookup_name()
        attributes = list(evaluation.definitions.get_attributes(name))
        attributes.sort()
        attr = [Symbol(attribute) for attribute in attributes]
        return Expression('List', *attr)


class SetAttributes(Builtin):
    """
    >> SetAttributes[f, Flat]
    >> Attributes[f]
     = {Flat}
    >> SetAttributes[{f, g}, {Flat, Orderless}]
    >> Attributes[g]
     = {Flat, Orderless}
    """

    attributes = ('HoldFirst',)

    def apply(self, symbols, attributes, evaluation):
        'SetAttributes[symbols_, attributes_]'

        symbols = get_symbol_list(symbols, lambda item: evaluation.message(
            'SetAttributes', 'sym', item, 1))
        if symbols is None:
            return
        values = get_symbol_list(attributes, lambda item: evaluation.message(
            'SetAttributes', 'sym', item, 2))
        if values is None:
            return
        for symbol in symbols:
            if 'System`Locked' in evaluation.definitions.get_attributes(symbol):
                evaluation.message('SetAttributes', 'locked', Symbol(symbol))
            else:
                for value in values:
                    evaluation.definitions.set_attribute(symbol, value)
        return Symbol('Null')


class ClearAttributes(Builtin):
    """
    >> SetAttributes[f, Flat]
    >> Attributes[f]
     = {Flat}
    >> ClearAttributes[f, Flat]
    >> Attributes[f]
     = {}
    Attributes that are not even set are simply ignored:
    >> ClearAttributes[{f}, {Flat}]
    >> Attributes[f]
     = {}
    """

    attributes = ('HoldFirst',)

    def apply(self, symbols, attributes, evaluation):
        'ClearAttributes[symbols_, attributes_]'

        symbols = get_symbol_list(symbols, lambda item: evaluation.message(
            'ClearAttributes', 'sym', item, 1))
        if symbols is None:
            return
        values = get_symbol_list(attributes, lambda item: evaluation.message(
            'ClearAttributes', 'sym', item, 2))
        if values is None:
            return
        for symbol in symbols:
            if 'System`Locked' in evaluation.definitions.get_attributes(symbol):
                evaluation.message('ClearAttributes', 'locked', Symbol(symbol))
            else:
                for value in values:
                    evaluation.definitions.clear_attribute(symbol, value)
        return Symbol('Null')


class Protect(Builtin):
    """
    >> A = {1, 2, 3};
    >> Protect[A]
    >> A[[2]] = 4;
     : Symbol A is Protected.
    >> A
     = {1, 2, 3}
    """

    attributes = ('HoldAll',)

    rules = {
        'Protect[symbols__]': 'SetAttributes[{symbols}, Protected]',
    }


class Unprotect(Builtin):

    attributes = ('HoldAll',)

    rules = {
        'Unprotect[symbols__]': 'ClearAttributes[{symbols}, Protected]',
    }


class Protected(Predefined):
    """
    Values of 'Protected' symbols cannot be modified:
    >> Attributes[p] = {Protected};
    >> p = 2;
     : Symbol p is Protected.
    >> f[p] ^= 3;
     : Tag p in f[p] is Protected.
    >> Format[p] = "text";
     : Symbol p is Protected.

    However, attributes might still be set:
    >> SetAttributes[p, Flat]
    >> Attributes[p]
     = {Flat, Protected}
    Thus, you can easily remove the attribute 'Protected':
    >> Attributes[p] = {};
    >> p = 2
     = 2
    You can also use 'Protect' or 'Unprotect', resp.
    >> Protect[p]
    >> Attributes[p]
     = {Protected}
    >> Unprotect[p]

    If a symbol is 'Protected' and 'Locked', it can never be changed again:
    >> SetAttributes[p, {Protected, Locked}]
    >> p = 2
     : Symbol p is Protected.
     = 2
    >> Unprotect[p]
     : Symbol p is locked.
    """


class ReadProtected(Predefined):
    """
    Values associated with 'ReadProtected' symbols cannot be read:
    >> ClearAll[p]
    >> p = 3;
    >> Definition[p]
     = p = 3
    >> SetAttributes[p, ReadProtected]
    >> Definition[p]
     = Attributes[p] = {ReadProtected}
    """


class Locked(Predefined):
    """
    The attributes of 'Locked' symbols cannot be modified:
    >> Attributes[lock] = {Flat, Locked};
    >> SetAttributes[lock, {}]
     : Symbol lock is locked.
    >> ClearAttributes[lock, Flat]
     : Symbol lock is locked.
    >> Attributes[lock] = {}
     : Symbol lock is locked.
     = {}
    >> Attributes[lock]
     = {Flat, Locked}

    However, their values might be modified (as long as they are not 'Protected' too):
    >> lock = 3
     = 3
    """


class Flat(Predefined):
    """
    >> SetAttributes[f, Flat]
    >> f[a, b, c] /. f[a, b] -> d
     = f[d, c]

    #> SetAttributes[{u, v}, Flat]
    #> u[x_] := {x}
    #> u[]
     = u[]
    #> u[a]
     = {a}
    #> u[a, b]
     : Recursion depth of 200 exceeded.
     = $Aborted
    #> u[a, b, c]
     : Recursion depth of 200 exceeded.
     = $Aborted
    #> v[x_] := x
    #> v[]
     = v[]
    #> v[a]
     = a
    #> v[a, b] (* in Mathematica: Iteration limit of 4096 exceeded. *)
     = v[a, b]
    #> v[a, b, c] (* in Mathematica: Iteration limit of 4096 exceeded. *)
     : Recursion depth of 200 exceeded.
     = $Aborted
    """


class Orderless(Predefined):
    """
    >> SetAttributes[f, Orderless]
    >> f[c, a, b, a + b, 3, 1.0]
     = f[1., 3, a, b, c, a + b]
    >> SetAttributes[f, Flat]
    >> f[a, b, c] /. f[a, b] -> d
     = f[c, d]
    """


class OneIdentity(Predefined):
    """
    'OneIdentity' affects pattern matching:
    >> SetAttributes[f, OneIdentity]
    >> a /. f[args___] -> {args}
     = {a}
    It does not affect evaluation:
    >> f[a]
     = f[a]
    """


class SequenceHold(Predefined):
    """
    Normally, 'Sequence' will be spliced into a function:
    >> f[Sequence[a, b]]
     = f[a, b]
    It does not for 'SequenceHold' functions:
    >> SetAttributes[f, SequenceHold]
    >> f[Sequence[a, b]]
     = f[Sequence[a, b]]

    E.g., 'Set' has attribute 'SequenceHold' to allow assignment of sequences to variables:
    >> s = Sequence[a, b];
    >> s
     = Sequence[a, b]
    >> Plus[s]
     = a + b
    """


class HoldFirst(Predefined):
    pass


class HoldRest(Predefined):
    pass


class HoldAll(Predefined):
    pass


class HoldAllComplete(Predefined):
    """
    'HoldAllComplete' even prevents upvalues from being used, and includes 'SequenceHold'.
    >> SetAttributes[f, HoldAllComplete]
    >> f[a] ^= 3;
    >> f[a]
     = f[a]
    >> f[Sequence[a, b]]
     = f[Sequence[a, b]]
    """


class NHoldAll(Predefined):
    """
    >> N[f[2, 3]]
     = f[2., 3.]
    >> SetAttributes[f, NHoldAll]
    >> N[f[2, 3]]
     = f[2, 3]
    """


class NHoldFirst(Predefined):
    pass


class NHoldRest(Predefined):
    pass


class Listable(Predefined):
    """
    >> SetAttributes[f, Listable]
    >> f[{1, 2, 3}, {4, 5, 6}]
     = {f[1, 4], f[2, 5], f[3, 6]}
    >> f[{1, 2, 3}, 4]
     = {f[1, 4], f[2, 4], f[3, 4]}
    >> {{1, 2}, {3, 4}} + {5, 6}
     = {{6, 7}, {9, 10}}
    """


class Constant(Predefined):
    pass
