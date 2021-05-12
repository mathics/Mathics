# -*- coding: utf-8 -*-

"""
Differential Equations
"""

import sympy
from mathics.builtin.base import Builtin
from mathics.core.expression import Expression
from mathics.core.convert import from_sympy

from mathics.version import __version__  # noqa used in loading to check consistency.


class DSolve(Builtin):
    """
    <dl>
    <dt>'DSolve[$eq$, $y$[$x$], $x$]'
        <dd>solves a differential equation for the function $y$[$x$].
    </dl>

    >> DSolve[y''[x] == 0, y[x], x]
     = {{y[x] -> x C[2] + C[1]}}

    >> DSolve[y''[x] == y[x], y[x], x]
     = {{y[x] -> C[1] E ^ (-x) + C[2] E ^ x}}

    >> DSolve[y''[x] == y[x], y, x]
     = {{y -> (Function[{x}, C[1] E ^ (-x) + C[2] E ^ x])}}

    DSolve can also solve basic PDE
    >> DSolve[D[f[x, y], x] / f[x, y] + 3 D[f[x, y], y] / f[x, y] == 2, f, {x, y}]
     = {{f -> (Function[{x, y}, E ^ (x / 5 + 3 y / 5) C[1][3 x - y]])}}

    >> DSolve[D[f[x, y], x] x + D[f[x, y], y] y == 2, f[x, y], {x, y}]
     = {{f[x, y] -> 2 Log[x] + C[1][y / x]}}

    >> DSolve[D[y[x, t], t] + 2 D[y[x, t], x] == 0, y[x, t], {x, t}]
     = {{y[x, t] -> C[1][-2 t + x]}}

    ## FIXME: sympy solves this as `Function[{x}, C[1] + Integrate[ArcSin[f[2 x]], x]]`
    ## #> Attributes[f] = {HoldAll};
    ## #> DSolve[f[x + x] == Sin[f'[x]], f, x]
    ##  : To avoid possible ambiguity, the arguments of the dependent variable in f[x + x] == Sin[f'[x]] should literally match the independent variables.
    ##  = DSolve[f[x + x] == Sin[f'[x]], f, x]

    ## #> Attributes[f] = {};
    ## #> DSolve[f[x + x] == Sin[f'[x]], f, x]
    ##  : To avoid possible ambiguity, the arguments of the dependent variable in f[2 x] == Sin[f'[x]] should literally match the independent variables.
    ##  = DSolve[f[2 x] == Sin[f'[x]], f, x]

    #> DSolve[f'[x] == f[x], f, x] // FullForm
     = List[List[Rule[f, Function[List[x], Times[C[1], Power[E, x]]]]]]

    #> DSolve[f'[x] == f[x], f, x] /. {C[1] -> 1}
     = {{f -> (Function[{x}, 1 E ^ x])}}

    #> DSolve[f'[x] == f[x], f, x] /. {C -> D}
     = {{f -> (Function[{x}, D[1] E ^ x])}}

    #> DSolve[f'[x] == f[x], f, x] /. {C[1] -> C[0]}
     = {{f -> (Function[{x}, C[0] E ^ x])}}

    #> DSolve[f[x] == 0, f, {}]
     : {} cannot be used as a variable.
     = DSolve[f[x] == 0, f, {}]

    ## Order of arguments shoudn't matter
    #> DSolve[D[f[x, y], x] == D[f[x, y], y], f, {x, y}]
     = {{f -> (Function[{x, y}, C[1][-x - y]])}}
    #> DSolve[D[f[x, y], x] == D[f[x, y], y], f[x, y], {x, y}]
     = {{f[x, y] -> C[1][-x - y]}}
    #> DSolve[D[f[x, y], x] == D[f[x, y], y], f[x, y], {y, x}]
     = {{f[x, y] -> C[1][-x - y]}}
    """

    # XXX sympy #11669 test
    """
    #> DSolve[\\[Gamma]'[x] == 0, \\[Gamma], x]
     : Hit sympy bug #11669.
     = ...
    """

    # TODO: GeneratedParameters option

    messages = {
        "deqn": (
            "Equation or list of equations expected instead of "
            "`1` in the first argument `1`."
        ),
        "deqx": (
            "Supplied equations are not differential equations "
            "of the given functions."
        ),
        "dsfun": "`1` cannot be used as a function.",
        "dsvar": "`1` cannot be used as a variable.",
        "litarg": (
            "To avoid possible ambiguity, the arguments of the "
            "dependent variable in `1` should literally match the "
            "independent variables."
        ),
        # FIXME: Remove these if sympy changes:
        "symsys": "SymPy can't solve systems of DEs.",
        "symimp": "SymPy can't solve this form of DE.",
        "symmua": "SymPy can't handle functions of multiple variables.",
        "sym11669": "Hit sympy bug #11669.",
    }

    def apply(self, eqn, y, x, evaluation):
        "DSolve[eqn_, y_, x_]"

        if eqn.has_form("List", None):
            # TODO: Try and solve BVPs using Solve or something analagous OR
            # add this functonality to sympy.
            evaluation.message("DSolve", "symsys")
            return

        if eqn.get_head_name() != "System`Equal":
            evaluation.message("DSolve", "deqn", eqn)
            return

        if x.is_symbol():
            syms = [x]
        elif x.has_form("List", 1, None):
            syms = sorted(x.get_leaves())
        else:
            return evaluation.message("DSolve", "dsvar", x)

        # Fixes pathalogical DSolve[y''[x] == y[x], y, x]
        try:
            y.leaves
            function_form = None
            func = y
        except AttributeError:
            func = Expression(y, *syms)
            function_form = Expression("List", *syms)

        if func.is_atom():
            evaluation.message("DSolve", "dsfun", y)
            return

        if set(func.leaves) != set(syms):
            evaluation.message("DSolve", "deqx")
            return

        f_name = func.get_head_name()

        conversion_args = {"converted_functions": set([f_name])}
        sym_func = func.to_sympy(**conversion_args)
        sym_eq = eqn.to_sympy(**conversion_args)

        # XXX when sympy adds support for higher-order PDE we will have to
        # change this to a tuple of solvefuns
        kwargs = {"solvefun": sympy.Function(str("C1"))}

        try:
            if len(syms) > 1:
                sym_result = sympy.pdsolve(sym_eq, sym_func, **kwargs)
            else:
                sym_result = sympy.dsolve(sym_eq, sym_func)
        except ValueError:
            evaluation.message("DSolve", "symimp")
            return
        except NotImplementedError:
            evaluation.message("DSolve", "symimp")
            return
        except TypeError:
            # Sympy bug #9446
            evaluation.message("DSolve", "litarg", eqn)
            return
        except AttributeError:
            evaluation.message("DSolve", "litarg", eqn)
            return
        except KeyError:
            evaluation.message("DSolve", "litarg", eqn)
            return
        else:
            if not isinstance(sym_result, list):
                sym_result = [sym_result]

        if function_form is None:
            return Expression(
                "List",
                *[
                    Expression("List", Expression("Rule", *from_sympy(soln).leaves))
                    for soln in sym_result
                ]
            )
        else:
            return Expression(
                "List",
                *[
                    Expression(
                        "List",
                        Expression(
                            "Rule",
                            y,
                            Expression(
                                "Function", function_form, *from_sympy(soln).leaves[1:]
                            ),
                        ),
                    )
                    for soln in sym_result
                ]
            )


# TODO: NDSolve


class C(Builtin):
    """
    <dl>
    <dt>'C'[$n$]
        <dd>represents the $n$th constant in a solution to a
        differential equation.
    </dl>
    """
