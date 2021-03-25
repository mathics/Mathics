# -*- coding: utf-8 -*-

"""
Functional Programming
"""

from itertools import chain

from mathics.version import __version__  # noqa used in loading to check consistency.
from mathics.builtin.base import Builtin, PostfixOperator
from mathics.core.expression import Expression


class Function(PostfixOperator):
    """
    <dl>
    <dt>'Function[$body$]'
    <dt>'$body$ &'
        <dd>represents a pure function with parameters '#1', '#2', etc.
    <dt>'Function[{$x1$, $x2$, ...}, $body$]'
        <dd>represents a pure function with parameters $x1$, $x2$, etc.
    <dt>'Function[{$x1$, $x2$, ...}, $body$, $attr$]'
        <dd>assume that the function has the attributes $attr$.
    </dl>

    >> f := # ^ 2 &
    X> f[3]
     = 9
    X> #^3& /@ {1, 2, 3}
     = {1, 8, 27}
    X> #1+#2&[4, 5]
     = 9

    You can use 'Function' with named parameters:
    >> Function[{x, y}, x * y][2, 3]
     = 6

    Parameters are renamed, when necessary, to avoid confusion:
    >> Function[{x}, Function[{y}, f[x, y]]][y]
     = Function[{y$}, f[y, y$]]
    >> Function[{y}, f[x, y]] /. x->y
     = Function[{y}, f[y, y]]
    >> Function[y, Function[x, y^x]][x][y]
     = x ^ y
    >> Function[x, Function[y, x^y]][x][y]
     = x ^ y

    Slots in inner functions are not affected by outer function application:
    >> g[#] & [h[#]] & [5]
     = g[h[5]]

    #> g[x_,y_] := x+y
    #> g[Sequence@@Slot/@Range[2]]&[1,2]
     = #1 + #2
    #> Evaluate[g[Sequence@@Slot/@Range[2]]]&[1,2]
     = 3
    """

    operator = "&"
    precedence = 90
    attributes = ("HoldAll",)

    messages = {
        "slot": "`1` should contain a positive integer.",
        "slotn": "Slot number `1` cannot be filled.",
        "fpct": "Too many parameters to be filled.",
        "iassoc": "Invalid association item `1`",
    }

    def apply_slots(self, body, args, evaluation):
        "Function[body_][args___]"

        args = list(chain([Expression("Function", body)], args.get_sequence()))
        return body.replace_slots(args, evaluation)

    def apply_named(self, vars, body, args, evaluation):
        "Function[vars_, body_][args___]"

        if vars.has_form("List", None):
            vars = vars.leaves
        else:
            vars = [vars]

        # print([v.get_head_name()=="System`Pattern" or v.is_symbol() for v in vars])
        args = args.get_sequence()
        if len(vars) > len(args):
            evaluation.message("Function", "fpct")
        else:
            # Allows to use both symbols or Blank patterns (z_Complex) to state the symbol.
            # this is not included in WL, and here does not have any impact, but it is needed for
            # translating the function to a compiled version.
            var_names = (
                var.get_name() if var.is_symbol() else var.leaves[0].get_name()
                for var in vars
            )
            vars = dict(list(zip(var_names, args[: len(vars)])))
            try:
                return body.replace_vars(vars)
            except:
                return

    # Not sure if DRY is possible here...
    def apply_named_attr(self, vars, body, attr, args, evaluation):
        "Function[vars_, body_, attr_][args___]"
        if vars.has_form("List", None):
            vars = vars.leaves
        else:
            vars = [vars]

        args = args.get_sequence()
        if len(vars) > len(args):
            evaluation.message("Function", "fpct")
        else:
            vars = dict(list(zip((var.get_name() for var in vars), args[: len(vars)])))
            try:
                return body.replace_vars(vars)
            except:
                return


class Slot(Builtin):
    """
    <dl>
    <dt>'#$n$'
        <dd>represents the $n$th argument to a pure function.
        <dt>'#'
        <dd>is short-hand for '#1'.
        <dt>'#0'
        <dd>represents the pure function itself.
    </dl>

    X> #
     = #1

    Unused arguments are simply ignored:
    >> {#1, #2, #3}&[1, 2, 3, 4, 5]
     = {1, 2, 3}

    Recursive pure functions can be written using '#0':
    >> If[#1<=1, 1, #1 #0[#1-1]]& [10]
     = 3628800

    #> # // InputForm
     = #1

    #> #0 // InputForm
     = #0
    """

    attributes = ("NHoldAll",)

    rules = {
        "Slot[]": "Slot[1]",
        "MakeBoxes[Slot[n_Integer?NonNegative],"
        "  f:StandardForm|TraditionalForm|InputForm|OutputForm]": (
            '"#" <> ToString[n]'
        ),
    }


class SlotSequence(Builtin):
    """
    <dl>
    <dt>'##'
        <dd>is the sequence of arguments supplied to a pure function.
    <dt>'##$n$'
        <dd>starts with the $n$th argument.
    </dl>

    >> Plus[##]& [1, 2, 3]
     = 6
    >> Plus[##2]& [1, 2, 3]
     = 5

    >> FullForm[##]
     = SlotSequence[1]

    #> ## // InputForm
     = ##1
    """

    attributes = ("NHoldAll",)

    rules = {
        "SlotSequence[]": "SlotSequence[1]",
        "MakeBoxes[SlotSequence[n_Integer?Positive],"
        "f:StandardForm|TraditionalForm|InputForm|OutputForm]": ('"##" <> ToString[n]'),
    }


class Composition(Builtin):
    """
    <dl>
    <dt>'Composition[$f$, $g$]'
        <dd>returns the composition of two functions $f$ and $g$.
    </dl>

    >> Composition[f, g][x]
     = f[g[x]]
    >> Composition[f, g, h][x, y, z]
     = f[g[h[x, y, z]]]
    >> Composition[]
     = Identity
    >> Composition[][x]
     = x
    >> Attributes[Composition]
     = {Flat, OneIdentity, Protected}
    >> Composition[f, Composition[g, h]]
     = Composition[f, g, h]
    """

    attributes = ("Flat", "OneIdentity")

    rules = {
        "Composition[]": "Identity",
    }

    def apply(self, functions, args, evaluation):
        "Composition[functions__][args___]"

        functions = functions.get_sequence()
        args = args.get_sequence()
        result = Expression(functions[-1], *args)
        for f in reversed(functions[:-1]):
            result = Expression(f, result)
        return result


class Identity(Builtin):
    """
    <dl>
      <dt>'Identity[$x$]'
      <dd>is the identity function, which returns $x$ unchanged.
    </dl>
    X> Identity[x]
     = x
    X> Identity[x, y]
     = Identity[x, y]
    """

    rules = {
        "Identity[x_]": "x",
    }
