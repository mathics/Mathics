# -*- coding: utf-8 -*-

r"""
Attributes

There are several builtin-attributes which have a predefined meaning in \Mathics.
However, you can set any symbol as an attribute, in contrast to \Mathematica.
"""

from mathics.version import __version__  # noqa used in loading to check consistency.

from mathics.builtin.base import Predefined, Builtin
from mathics.core.expression import Expression, Symbol, SymbolNull, String
from mathics.builtin.assignment import get_symbol_list


class Attributes(Builtin):
    """
    <dl>
    <dt>'Attributes'[$symbol$]
        <dd>returns the attributes of $symbol$.
    <dt>'Attributes'[$symbol$] = {$attr1$, $attr2$}
        <dd>sets the attributes of $symbol$, replacing any existing attributes.
    </dl>

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

    attributes = ("HoldAll", "Listable")

    def apply(self, expr, evaluation):
        "Attributes[expr_]"

        name = expr.get_lookup_name()
        attributes = list(evaluation.definitions.get_attributes(name))
        attributes.sort()
        attr = [Symbol(attribute) for attribute in attributes]
        return Expression("List", *attr)


class SetAttributes(Builtin):
    """
    <dl>
    <dt>'SetAttributes'[$symbol$, $attrib$]
        <dd>adds $attrib$ to the list of $symbol$'s attributes.
    </dl>

    >> SetAttributes[f, Flat]
    >> Attributes[f]
     = {Flat}

    Multiple attributes can be set at the same time using lists:
    >> SetAttributes[{f, g}, {Flat, Orderless}]
    >> Attributes[g]
     = {Flat, Orderless}
    """

    attributes = ("HoldFirst",)

    def apply(self, symbols, attributes, evaluation):
        "SetAttributes[symbols_, attributes_]"

        symbols = get_symbol_list(
            symbols, lambda item: evaluation.message("SetAttributes", "sym", item, 1)
        )
        if symbols is None:
            return
        values = get_symbol_list(
            attributes, lambda item: evaluation.message("SetAttributes", "sym", item, 2)
        )
        if values is None:
            return
        for symbol in symbols:
            if "System`Locked" in evaluation.definitions.get_attributes(symbol):
                evaluation.message("SetAttributes", "locked", Symbol(symbol))
            else:
                for value in values:
                    evaluation.definitions.set_attribute(symbol, value)
        return SymbolNull


class ClearAttributes(Builtin):
    """
    <dl>
    <dt>'ClearAttributes'[$symbol$, $attrib$]
        <dd>removes $attrib$ from $symbol$'s attributes.
    </dl>

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

    attributes = ("HoldFirst",)

    def apply(self, symbols, attributes, evaluation):
        "ClearAttributes[symbols_, attributes_]"

        symbols = get_symbol_list(
            symbols, lambda item: evaluation.message("ClearAttributes", "sym", item, 1)
        )
        if symbols is None:
            return
        values = get_symbol_list(
            attributes,
            lambda item: evaluation.message("ClearAttributes", "sym", item, 2),
        )
        if values is None:
            return
        for symbol in symbols:
            if "System`Locked" in evaluation.definitions.get_attributes(symbol):
                evaluation.message("ClearAttributes", "locked", Symbol(symbol))
            else:
                for value in values:
                    evaluation.definitions.clear_attribute(symbol, value)
        return SymbolNull


class Protect(Builtin):
    """
    <dl>
      <dt>'Protect'[$s1$, $s2$, ...]
      <dd>sets the attribute 'Protected' for the symbols $si$.

      <dt>'Protect'[$str1$, $str2$, ...]
      <dd>protects all symbols whose names textually match $stri$.
    </dl>

    >> A = {1, 2, 3};
    >> Protect[A]
    >> A[[2]] = 4;
     : Symbol A is Protected.
    >> A
     = {1, 2, 3}
    """

    attributes = ("HoldAll",)
    messages = {
        "ssym": "`1` is not a symbol or a string.",
    }

    def apply(self, symbols, evaluation):
        "Protect[symbols___]"
        protected = Symbol("System`Protected")
        items = []

        if isinstance(symbols, Symbol):
            symbols = [symbols]
        elif isinstance(symbols, String):
            symbols = [symbols]
        elif isinstance(symbols, Expression):
            if symbols.get_head_name() in ("System`Sequence", "System`List"):
                symbols = symbols.get_leaves()
            else:
                evaluation.message("Protect", "ssym", symbols)
                return SymbolNull

        for symbol in symbols:
            if isinstance(symbol, Symbol):
                items.append(symbol)
            else:
                pattern = symbol.get_string_value()
                if not pattern or pattern == "":
                    evaluation.message("Protect", "ssym", symbol)
                    continue

                if pattern[0] == "`":
                    pattern = evaluation.definitions.get_current_context() + pattern[1:]
                names = evaluation.definitions.get_matching_names(pattern)
                for defn in names:
                    symbol = Symbol(defn)
                    if not "System`Locked" in evaluation.definitions.get_attributes(
                        defn
                    ):
                        items.append(symbol)

        Expression("SetAttributes", Expression("List", *items), protected).evaluate(
            evaluation
        )
        return SymbolNull


class Unprotect(Builtin):
    """
    <dl>
      <dt>'Unprotect'[$s1$, $s2$, ...]
      <dd>removes the attribute 'Protected' for the symbols $si$.

      <dt>'Unprotect'[$str$]
      <dd>unprotects symbols whose names textually match $str$.
    </dl>
    """

    attributes = ("HoldAll",)
    messages = {
        "ssym": "`1` is not a symbol or a string.",
    }

    def apply(self, symbols, evaluation):
        "Unprotect[symbols___]"
        protected = Symbol("System`Protected")
        items = []
        if isinstance(symbols, Symbol):
            symbols = [symbols]
        elif isinstance(symbols, Expression):
            symbols = symbols.get_leaves()
        elif isinstance(symbols, String):
            symbols = [symbols]
        else:
            symbols = symbols.get_sequence()

        for symbol in symbols:
            if isinstance(symbol, Symbol):
                items.append(symbol)
            else:
                pattern = symbol.get_string_value()
                if not pattern or pattern == "":
                    evaluation.message("Unprotect", "ssym", symbol)
                    continue

                if pattern[0] == "`":
                    pattern = evaluation.definitions.get_current_context() + pattern[1:]
                names = evaluation.definitions.get_matching_names(pattern)
                for defn in names:
                    symbol = Symbol(defn)
                    if not "System`Locked" in evaluation.definitions.get_attributes(
                        defn
                    ):
                        items.append(symbol)

        Expression("ClearAttributes", Expression("List", *items), protected).evaluate(
            evaluation
        )
        return SymbolNull


class Protected(Predefined):
    """
    <dl>
    <dt>'Protected'
        <dd>is an attribute that prevents values on a symbol from
        being modified.
    </dl>

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
    <dl>
    <dt>'ReadProtected'
        <dd>is an attribute that prevents values on a symbol from
        being read.
    </dl>

    Values associated with 'ReadProtected' symbols cannot be seen in
    'Definition':
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
    <dl>
    <dt>'Locked'
        <dd>is an attribute that prevents attributes on a symbol from
        being modified.
    </dl>

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
    <dl>
    <dt>'Flat'
        <dd>is an attribute that specifies that nested occurrences of
        a function should be automatically flattened.
    </dl>

    A symbol with the 'Flat' attribute represents an associative
    mathematical operation:
    >> SetAttributes[f, Flat]
    >> f[a, f[b, c]]
     = f[a, b, c]

    'Flat' is taken into account in pattern matching:
    >> f[a, b, c] /. f[a, b] -> d
     = f[d, c]

    #> SetAttributes[{u, v}, Flat]
    #> u[x_] := {x}
    #> u[]
     = u[]
    #> u[a]
     = {a}
    #> u[a, b]
     : Iteration limit of 1000 exceeded.
     = $Aborted
    #> u[a, b, c]
     : Iteration limit of 1000 exceeded.
     = $Aborted
    #> v[x_] := x
    #> v[]
     = v[]
    #> v[a]
     = a
    #> v[a, b] (* in Mathematica: Iteration limit of 4096 exceeded. *)
     = v[a, b]
    #> v[a, b, c] (* in Mathematica: Iteration limit of 4096 exceeded. *)
     : Iteration limit of 1000 exceeded.
     = $Aborted
    """


class Orderless(Predefined):
    """<dl>
        <dt>'Orderless'
        <dd>is an attribute that can be assigned to a symbol $f$ to
        indicate that the elements $ei$ in expressions of the form
        $f$[$e1$, $e2$, ...] should automatically be sorted into
        canonical order. This property is accounted for in pattern
        matching.
    </dl>

    The leaves of an 'Orderless' function are automatically sorted:
    >> SetAttributes[f, Orderless]
    >> f[c, a, b, a + b, 3, 1.0]
     = f[1., 3, a, b, c, a + b]

    A symbol with the 'Orderless' attribute represents a commutative
    mathematical operation.
    >> f[a, b] == f[b, a]
     = True

    'Orderless' affects pattern matching:
    >> SetAttributes[f, Flat]
    >> f[a, b, c] /. f[a, c] -> d
     = f[b, d]

    """


class OneIdentity(Predefined):
    """
    <dl>
    <dt>'OneIdentity'
        <dd>is an attribute specifying that '$f$[$x$]' should be treated
        as equivalent to $x$ in pattern matching.
    </dl>

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
    <dl>
    <dt>'SequenceHold'
        <dd>is an attribute that prevents 'Sequence' objects from being
        spliced into a function's arguments.
    </dl>

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
    """
    <dl>
    <dt>'HoldFirst'
        <dd>is an attribute specifying that the first argument of a
        function should be left unevaluated.
    </dl>

    >> Attributes[Set]
     = {HoldFirst, Protected, SequenceHold}
    """


class HoldRest(Predefined):
    """
    <dl>
    <dt>'HoldRest'
        <dd>is an attribute specifying that all but the first argument
        of a function should be left unevaluated.
    </dl>

    >> Attributes[If]
     = {HoldRest, Protected}
    """


class HoldAll(Predefined):
    """
    <dl>
    <dt>'HoldAll'
        <dd>is an attribute specifying that all arguments of a
        function should be left unevaluated.
    </dl>

    >> Attributes[Function]
     = {HoldAll, Protected}
    """


class HoldAllComplete(Predefined):
    """
    <dl>
    <dt>'HoldAllComplete'
        <dd>is an attribute that includes the effects of 'HoldAll' and
        'SequenceHold', and also protects the function from being
        affected by the upvalues of any arguments.
    </dl>

    'HoldAllComplete' even prevents upvalues from being used, and
    includes 'SequenceHold'.
    >> SetAttributes[f, HoldAllComplete]
    >> f[a] ^= 3;
    >> f[a]
     = f[a]
    >> f[Sequence[a, b]]
     = f[Sequence[a, b]]
    """


class NHoldAll(Predefined):
    """
    <dl>
    <dt>'NHoldAll'
        <dd>is an attribute that protects all arguments of a
        function from numeric evaluation.
    </dl>

    >> N[f[2, 3]]
     = f[2., 3.]
    >> SetAttributes[f, NHoldAll]
    >> N[f[2, 3]]
     = f[2, 3]
    """


class NHoldFirst(Predefined):
    """
    <dl>
    <dt>'NHoldFirst'
        <dd>is an attribute that protects the first argument of a
        function from numeric evaluation.
    </dl>
    """


class NHoldRest(Predefined):
    """
    <dl>
    <dt>'NHoldRest'
        <dd>is an attribute that protects all but the first argument
        of a function from numeric evaluation.
    </dl>
    """


class Listable(Predefined):
    """
    <dl>
    <dt>'Listable'
        <dd>is an attribute specifying that a function should be
        automatically applied to each element of a list.
    </dl>

    >> SetAttributes[f, Listable]
    >> f[{1, 2, 3}, {4, 5, 6}]
     = {f[1, 4], f[2, 5], f[3, 6]}
    >> f[{1, 2, 3}, 4]
     = {f[1, 4], f[2, 4], f[3, 4]}
    >> {{1, 2}, {3, 4}} + {5, 6}
     = {{6, 7}, {9, 10}}
    """


class Constant(Predefined):
    """
    <dl>
    <dt>'Constant'
        <dd>is an attribute that indicates that a symbol is a constant.
    </dl>

    Mathematical constants like 'E' have attribute 'Constant':
    >> Attributes[E]
     = {Constant, Protected, ReadProtected}

    Constant symbols cannot be used as variables in 'Solve' and
    related functions:
    >> Solve[x + E == 0, E]
     : E is not a valid variable.
     = Solve[E + x == 0, E]
    """
