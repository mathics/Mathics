# -*- coding: utf8 -*-

import time

from sympy.utilities.lambdify import lambdify

from mathics.builtin.base import Builtin
from mathics.core.expression import Expression, Real, from_sympy

COMPILED = {}


class Compile(Builtin):
    """
    <dl>
    <dt>'Compile[{$x1$, $x2$, ...}, expr]'
      <dd>compiles $expr$ as a numerical function of $xi$.
    <\dl>

    >> cf = Compile[{x, y}, Cos[x ^ 2] / (1 + x ^ 2 + y ^ 2)]
     = CompiledFunction[{x, y}, Cos[x ^ 2] / (1 + x ^ 2 + y ^ 2)]

    >> f = Function[{x, y}, Cos[x ^ 2] / (1 + x ^ 2 + y ^ 2)]
     = Function[{x, y}, Cos[x ^ 2] / (1 + x ^ 2 + y ^ 2)]

    >> f[1.5, 7.0] // AbsoluteTiming
     = {..., ...}

    >> cf[1.5, 7.0] // AbsoluteTiming
     = {..., ...}
    """

    # TODO implement MakeBoxes properly (with -CompiledCode- etc)

    def apply(self, args, expr, evaluation):
        "Compile[args_, expr_]"
        COMPILED[args, expr] = lambdify(
            (arg.to_sympy() for arg in args.leaves), expr.to_sympy())
        return Expression('CompiledFunction', args, expr)


class CompiledFunction(Builtin):
    """
    <dl>
    <dt>'CompiledFunctions[args...]'
      <dd>represents a compiled function
    </dl>
    """

    # Method used to store / lookup compiled functions based on expr, args
    # is quite different to the method used by MMA

    messages = {
        'invd': '`1` is not a valid CompiledFunction expression.'
    }

    def apply(self, cfargs, cfexpr, args, evaluation):
        "CompiledFunction[cfargs_, cfexpr_][args___]"
        try:
            cf = COMPILED[cfargs, cfexpr]
        except KeyError:
            evaluation.message('CompiledFunction', 'invd', Expression(
                Expression('CompiledFunction', cfargs, cfexpr), args))
        return from_sympy(cf(*[arg.to_sympy() for arg in args.get_sequence()]))
