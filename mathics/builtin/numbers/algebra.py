# -*- coding: utf-8 -*-
"""
Algebraic Manipulation
"""


from mathics.version import __version__  # noqa used in loading to check consistency.

from mathics.builtin.base import Builtin
from mathics.core.expression import (
    Atom,
    Expression,
    Integer,
    Integer0,
    Integer1,
    RationalOneHalf,
    Number,
    Symbol,
    SymbolFalse,
    SymbolList,
    SymbolNull,
    SymbolTrue,
)
from mathics.core.convert import from_sympy, sympy_symbol_prefix
from mathics.core.rules import Pattern
from mathics.builtin.scoping import dynamic_scoping
from mathics.builtin.inference import evaluate_predicate

import sympy


def sympy_factor(expr_sympy):
    try:
        result = sympy.together(expr_sympy)
        numer, denom = result.as_numer_denom()
        if denom == 1:
            result = sympy.factor(expr_sympy)
        else:
            result = sympy.factor(numer) / sympy.factor(denom)
    except sympy.PolynomialError:
        return expr_sympy
    return result


def cancel(expr):
    if expr.has_form("Plus", None):
        return Expression("Plus", *[cancel(leaf) for leaf in expr.leaves])
    else:
        try:
            result = expr.to_sympy()
            if result is None:
                return None

            # result = sympy.powsimp(result, deep=True)
            result = sympy.cancel(result)

            # cancel factors out rationals, so we factor them again
            result = sympy_factor(result)

            return from_sympy(result)
        except sympy.PolynomialError:
            # e.g. for non-commutative expressions
            return expr


def expand(expr, numer=True, denom=False, deep=False, **kwargs):
    def _expand(expr):
        return expand(expr, numer=numer, denom=denom, deep=deep, **kwargs)

    if kwargs["modulus"] is not None and kwargs["modulus"] <= 0:
        return Integer0

    target_pat = kwargs.get("pattern", None)
    if target_pat:
        evaluation = kwargs["evaluation"]
    # A special case for trigonometric functions
    if "trig" in kwargs and kwargs["trig"]:
        if expr.has_form(
            ("Sin", "Cos", "Tan", "Cot", "Sinh", "Cosh", "Tanh", "Coth"), 1
        ):
            head = expr.get_head()
            theta = expr.leaves[0]
            if (target_pat is not None) and theta.is_free(target_pat, evaluation):
                return expr
            if deep:
                theta = _expand(theta)

            if theta.has_form("Plus", 2, None):
                x, y = theta.leaves[0], Expression("Plus", *theta.leaves[1:])
                if head == Symbol("Sin"):
                    a = Expression(
                        "Times",
                        _expand(Expression("Sin", x)),
                        _expand(Expression("Cos", y)),
                    )

                    b = Expression(
                        "Times",
                        _expand(Expression("Cos", x)),
                        _expand(Expression("Sin", y)),
                    )
                    return _expand(Expression("Plus", a, b))
                elif head == Symbol("Cos"):
                    a = Expression(
                        "Times",
                        _expand(Expression("Cos", x)),
                        _expand(Expression("Cos", y)),
                    )

                    b = Expression(
                        "Times",
                        _expand(Expression("Sin", x)),
                        _expand(Expression("Sin", y)),
                    )

                    return _expand(Expression("Plus", a, -b))
                elif head == Symbol("Sinh"):
                    a = Expression(
                        "Times",
                        _expand(Expression("Sinh", x)),
                        _expand(Expression("Cosh", y)),
                    )

                    b = Expression(
                        "Times",
                        _expand(Expression("Cosh", x)),
                        _expand(Expression("Sinh", y)),
                    )

                    return _expand(Expression("Plus", a, b))
                elif head == Symbol("Cosh"):
                    a = Expression(
                        "Times",
                        _expand(Expression("Cosh", x)),
                        _expand(Expression("Cosh", y)),
                    )

                    b = Expression(
                        "Times",
                        _expand(Expression("Sinh", x)),
                        _expand(Expression("Sinh", y)),
                    )

                    return _expand(Expression("Plus", a, b))
                elif head == Symbol("Tan"):
                    a = _expand(Expression("Sin", theta))
                    b = Expression(
                        "Power", _expand(Expression("Cos", theta)), Integer(-1)
                    )
                    return _expand(Expression("Times", a, b))
                elif head == Symbol("Cot"):
                    a = _expand(Expression("Cos", theta))
                    b = Expression(
                        "Power", _expand(Expression("Sin", theta)), Integer(-1)
                    )
                    return _expand(Expression("Times", a, b))
                elif head == Symbol("Tanh"):
                    a = _expand(Expression("Sinh", theta))
                    b = Expression(
                        "Power", _expand(Expression("Cosh", theta)), Integer(-1)
                    )
                    return _expand(Expression("Times", a, b))
                elif head == Symbol("Coth"):
                    a = _expand(Expression("Times", "Cosh", theta))
                    b = Expression(
                        "Power", _expand(Expression("Sinh", theta)), Integer(-1)
                    )
                    return _expand(Expression(a, b))

    sub_exprs = []

    def store_sub_expr(expr):
        sub_exprs.append(expr)
        result = sympy.Symbol(sympy_symbol_prefix + str(len(sub_exprs) - 1))
        return result

    def get_sub_expr(expr):
        name = expr.get_name()
        assert isinstance(expr, Symbol) and name.startswith("System`")
        i = int(name[len("System`") :])
        return sub_exprs[i]

    def convert_sympy(expr):
        "converts top-level to sympy"
        leaves = expr.get_leaves()
        if isinstance(expr, Integer):
            return sympy.Integer(expr.get_int_value())
        if target_pat is not None and not isinstance(expr, Number):
            if expr.is_free(target_pat, evaluation):
                return store_sub_expr(expr)
        if expr.has_form("Power", 2):
            # sympy won't expand `(a + b) / x` to `a / x + b / x` if denom is False
            # if denom is False we store negative powers to prevent this.
            n1 = leaves[1].get_int_value()
            if not denom and n1 is not None and n1 < 0:
                return store_sub_expr(expr)
            return sympy.Pow(*[convert_sympy(leaf) for leaf in leaves])
        elif expr.has_form("Times", 2, None):
            return sympy.Mul(*[convert_sympy(leaf) for leaf in leaves])
        elif expr.has_form("Plus", 2, None):
            return sympy.Add(*[convert_sympy(leaf) for leaf in leaves])
        else:
            return store_sub_expr(expr)

    def unconvert_subexprs(expr):
        if expr.is_atom():
            if isinstance(expr, Symbol):
                return get_sub_expr(expr)
            else:
                return expr
        else:
            return Expression(
                expr.head, *[unconvert_subexprs(leaf) for leaf in expr.get_leaves()]
            )

    sympy_expr = convert_sympy(expr)
    if deep:
        # thread over everything
        for (
            i,
            sub_expr,
        ) in enumerate(sub_exprs):
            if not sub_expr.is_atom():
                head = _expand(sub_expr.head)  # also expand head
                leaves = sub_expr.get_leaves()
                if target_pat:
                    leaves = [
                        leaf if leaf.is_free(target_pat, evaluation) else _expand(leaf)
                        for leaf in leaves
                    ]
                else:
                    leaves = [_expand(leaf) for leaf in leaves]
                sub_exprs[i] = Expression(head, *leaves)
    else:
        # thread over Lists etc.
        threaded_heads = ("List", "Rule")
        for i, sub_expr in enumerate(sub_exprs):
            for head in threaded_heads:
                if sub_expr.has_form(head, None):
                    leaves = sub_expr.get_leaves()
                    if target_pat:
                        leaves = [
                            leaf
                            if leaf.is_free(target_pat, evaluation)
                            else _expand(leaf)
                            for leaf in leaves
                        ]
                    else:
                        leaves = [_expand(leaf) for leaf in leaves]
                    sub_exprs[i] = Expression(head, *leaves)
                    break

    hints = {
        "mul": True,
        "multinomial": True,
        "power_exp": False,
        "power_base": False,
        "basic": False,
        "log": False,
    }

    hints.update(kwargs)

    if numer and denom:
        # don't expand fractions when modulus is True
        if hints["modulus"] is not None:
            hints["frac"] = True
    else:
        # setting both True doesn't expand denom
        hints["numer"] = numer
        hints["denom"] = denom

    sympy_expr = sympy_expr.expand(**hints)
    result = from_sympy(sympy_expr)
    result = unconvert_subexprs(result)
    return result


def find_all_vars(expr):
    variables = set()

    def find_vars(e, e_sympy):
        assert e_sympy is not None
        if e_sympy.is_constant():
            return
        elif e.is_symbol():
            variables.add(e)
        elif e.has_form(("Plus", "Times"), None):
            for l in e.leaves:
                l_sympy = l.to_sympy()
                if l_sympy is not None:
                    find_vars(l, l_sympy)
        elif e.has_form("Power", 2):
            (a, b) = e.leaves  # a^b
            a_sympy, b_sympy = a.to_sympy(), b.to_sympy()
            if a_sympy is None or b_sympy is None:
                return
            if not (a_sympy.is_constant()) and b_sympy.is_rational:
                find_vars(a, a_sympy)
        elif not (e.is_atom()):
            variables.add(e)

    exprs = expr.leaves if expr.has_form("List", None) else [expr]
    for e in exprs:
        e_sympy = e.to_sympy()
        if e_sympy is not None:
            find_vars(e, e_sympy)

    return variables


def find_exponents(expr, var):
    """
    Find all exponents of var in expr
    """
    f = expr.to_sympy()
    x = var.to_sympy()
    if f is None or x is None:
        return {0}

    result = set()
    for t in f.expand(power_exp=False).as_ordered_terms():
        coeff, exponent = t.as_coeff_exponent(x)
        if exponent:
            result.add(from_sympy(exponent))
        else:
            # find exponent of terms multiplied with functions: sin, cos, log, exp, ...
            # e.g: x^3 * Sin[x^2] should give 3
            muls = [
                term.as_coeff_mul(x)[1]
                if term.as_coeff_mul(x)[1]
                else (sympy.Integer(0),)
                for term in coeff.as_ordered_terms()
            ]
            expos = [term.as_coeff_exponent(x)[1] for mul in muls for term in mul]
            result.add(from_sympy(sympy.Max(*[e for e in expos])))
    return sorted(result)


class Cancel(Builtin):
    """
    <dl>
      <dt>'Cancel[$expr$]'
      <dd>cancels out common factors in numerators and denominators.
    </dl>

    >> Cancel[x / x ^ 2]
     = 1 / x
    'Cancel' threads over sums:
    >> Cancel[x / x ^ 2 + y / y ^ 2]
     = 1 / x + 1 / y

    >> Cancel[f[x] / x + x * f[x] / x ^ 2]
     = 2 f[x] / x
    """

    attributes = ("Listable", "Protected")

    def apply(self, expr, evaluation):
        "Cancel[expr_]"

        return cancel(expr)


class Simplify(Builtin):
    """
    <dl>
    <dt>'Simplify[$expr$]'
        <dd>simplifies $expr$.
    <dt>'Simplify[$expr$, $assump$]'
        <dd>simplifies $expr$ assuming $assump$ instead of $Assumptions$.
    </dl>

    >> Simplify[2*Sin[x]^2 + 2*Cos[x]^2]
     = 2
    >> Simplify[x]
     = x
    >> Simplify[f[x]]
     = f[x]

    Simplify over conditional expressions uses $Assumptions, or $assump$
    to evaluate the condition:
    # TODO: enable this once the logic for conditional expression
    # be restaured.
    # >> $Assumptions={a <= 0};
    # >> Simplify[ConditionalExpression[1, a > 0]]
    # = Undefined
    # >> Simplify[ConditionalExpression[1, a > 0], { a > 0 }]
    # = 1
    """

    rules = {
        "Simplify[list_List]": "Simplify /@ list",
        "Simplify[rule_Rule]": "Simplify /@ rule",
        "Simplify[list_List, assum_]": "Simplify[#1, assum]& /@ list",
        "Simplify[rule_Rule, assum_]": "Simplify[#1, assum]& /@ rule",
        "Simplify[0^a_, assum_]": "ConditionalExpression[0,Simplify[a>0]]",
        "Simplify[b_^a_, assum_]": "ConditionalExpression[b,Simplify[{Or[a>0, b!=0]}]]",
    }

    def apply_assuming(self, expr, assumptions, evaluation):
        "%(name)s[expr_, assumptions_]"
        assumptions = assumptions.evaluate(evaluation)
        return dynamic_scoping(
            lambda ev: self.apply(expr, ev),
            {"System`$Assumptions": assumptions},
            evaluation,
        )

    def apply(self, expr, evaluation):
        "%(name)s[expr_]"
        # Check first if we are dealing with a logic expression...
        expr = evaluate_predicate(expr, evaluation)
        if expr.is_atom():
            return expr
        # else, use sympy:
        leaves = [self.apply(leaf, evaluation) for leaf in expr._leaves]
        head = self.apply(expr.get_head(), evaluation)
        expr = Expression(head, *leaves)

        sympy_expr = expr.to_sympy()
        if sympy_expr is None:
            return
        sympy_result = sympy.simplify(sympy_expr)
        return from_sympy(sympy_result)


class FullSimplify(Simplify):
    """
    <dl>
    <dt>'FullSimplify[$expr$]'
        <dd>simplifies $expr$ using an extended set of simplification rules.
    <dt>'FullSimplify[$expr$, $assump$]'
        <dd>simplifies $expr$ assuming $assump$ instead of $Assumptions$.
    </dl>
    TODO: implement the extension. By now, this does the same than Simplify...

    >> FullSimplify[2*Sin[x]^2 + 2*Cos[x]^2]
     = 2

    """

    rules = {
        "FullSimplify[list_List]": "FullSimplify /@ list",
        "FullSimplify[rule_Rule]": "FullSimplify /@ rule",
        "FullSimplify[eq_Equal]": "FullSimplify /@ eq",
        "FullSimplify[list_List, assum_]": "FullSimplify[#1, assum]& /@ list",
        "FullSimplify[rule_Rule, assum_]": "FullSimplify[#1, assum]& /@ rule",
        "FullSimplify[eq_Equal, assum_]": "FullSimplify[#1, assum]& /@ eq",
    }


class Together(Builtin):
    """
    <dl>
    <dt>'Together[$expr$]'
        <dd>writes sums of fractions in $expr$ together.
    </dl>

    >> Together[a / c + b / c]
     = (a + b) / c
    'Together' operates on lists:
    >> Together[{x / (y+1) + x / (y+1)^2}]
     = {x (2 + y) / (1 + y) ^ 2}
    But it does not touch other functions:
    >> Together[f[a / c + b / c]]
     = f[a / c + b / c]

    #> f[x]/x+f[x]/x^2//Together
     = f[x] (1 + x) / x ^ 2
    """

    attributes = ["Listable"]

    def apply(self, expr, evaluation):
        "Together[expr_]"

        expr_sympy = expr.to_sympy()
        if expr_sympy is None:
            return None
        result = sympy.together(expr_sympy)
        result = from_sympy(result)
        result = cancel(result)
        return result


class Factor(Builtin):
    """
    <dl>
    <dt>'Factor[$expr$]'
        <dd>factors the polynomial expression $expr$.
    </dl>

    >> Factor[x ^ 2 + 2 x + 1]
     = (1 + x) ^ 2

    >> Factor[1 / (x^2+2x+1) + 1 / (x^4+2x^2+1)]
     = (2 + 2 x + 3 x ^ 2 + x ^ 4) / ((1 + x) ^ 2 (1 + x ^ 2) ^ 2)

    ## Issue659
    #> Factor[{x+x^2}]
     = {x (1 + x)}
    """

    attributes = ("Listable",)

    def apply(self, expr, evaluation):
        "Factor[expr_]"

        expr_sympy = expr.to_sympy()
        if expr_sympy is None:
            return None

        try:
            result = sympy.together(expr_sympy)
            numer, denom = result.as_numer_denom()
            if denom == 1:
                result = sympy.factor(expr_sympy)
            else:
                result = sympy.factor(numer) / sympy.factor(denom)
        except sympy.PolynomialError:
            return expr
        return from_sympy(result)


class FactorTermsList(Builtin):
    """
    <dl>
    <dt>'FactorTermsList[poly]'
        <dd>returns a list of 2 elements.
        The first element is the numerical factor in $poly$.
        The second one is the remaining of the polynomial with numerical factor removed
    <dt>'FactorTermsList[poly, {x1, x2, ...}]'
        <dd>returns a list of factors in $poly$.
        The first element is the numerical factor in $poly$.
        The next ones are factors that are independent of variables lists which
        are created by removing each variable $xi$ from right to left.
        The last one is the remaining of polynomial after dividing $poly$ to all previous factors
    </dl>

    >> FactorTermsList[2 x^2 - 2]
     = {2, -1 + x ^ 2}
    >> FactorTermsList[x^2 - 2 x + 1]
     = {1, 1 - 2 x + x ^ 2}
    #> FactorTermsList[2 x^2 - 2, x]
     = {2, 1, -1 + x ^ 2}

    >> f = 3 (-1 + 2 x) (-1 + y) (1 - a)
     = 3 (-1 + 2 x) (-1 + y) (1 - a)
    >> FactorTermsList[f]
     = {-3, -1 + a - 2 a x - a y + 2 x + y - 2 x y + 2 a x y}
    >> FactorTermsList[f, x]
     = {-3, 1 - a - y + a y, -1 + 2 x}
    """

    rules = {
        "FactorTermsList[expr_]": "FactorTermsList[expr, {}]",
        "FactorTermsList[expr_, var_]": "FactorTermsList[expr, {var}]",
    }

    messages = {
        # 'poly': '`1` is not a polynomial.',
        "ivar": "`1` is not a valid variable.",
    }

    def apply_list(self, expr, vars, evaluation):
        "FactorTermsList[expr_, vars_List]"
        if expr == Integer0:
            return Expression("List", Integer1, Integer0)
        elif isinstance(expr, Number):
            return Expression("List", expr, Integer1)

        for x in vars.leaves:
            if not (isinstance(x, Atom)):
                return evaluation.message("CoefficientList", "ivar", x)

        sympy_expr = expr.to_sympy()
        if sympy_expr is None:
            return Expression("List", Integer1, expr)
        sympy_expr = sympy.together(sympy_expr)

        sympy_vars = [
            x.to_sympy()
            for x in vars.leaves
            if isinstance(x, Symbol) and sympy_expr.is_polynomial(x.to_sympy())
        ]

        result = []
        numer, denom = sympy_expr.as_numer_denom()
        try:
            if denom == 1:
                # Get numerical part
                num_coeff, num_polys = sympy.factor_list(sympy.Poly(numer))
                result.append(num_coeff)

                # Get factors are independent of sub list of variables
                if (
                    sympy_vars
                    and isinstance(expr, Expression)
                    and any(
                        x.free_symbols.issubset(sympy_expr.free_symbols)
                        for x in sympy_vars
                    )
                ):
                    for i in reversed(range(len(sympy_vars))):
                        numer = sympy.factor(numer) / sympy.factor(num_coeff)
                        num_coeff, num_polys = sympy.factor_list(
                            sympy.Poly(numer), *[x for x in sympy_vars[: (i + 1)]]
                        )
                        result.append(sympy.expand(num_coeff))

                # Last factor
                numer = sympy.factor(numer) / sympy.factor(num_coeff)
                result.append(sympy.expand(numer))
            else:
                num_coeff, num_polys = sympy.factor_list(sympy.Poly(numer))
                den_coeff, den_polys = sympy.factor_list(sympy.Poly(denom))
                result = [
                    num_coeff / den_coeff,
                    sympy.expand(
                        sympy.factor(numer)
                        / num_coeff
                        / (sympy.factor(denom) / den_coeff)
                    ),
                ]
        except sympy.PolynomialError:  # MMA does not raise error for non poly
            result.append(sympy.expand(numer))
            # evaluation.message(self.get_name(), 'poly', expr)

        return Expression("List", *[from_sympy(i) for i in result])


class Apart(Builtin):
    """
    <dl>
    <dt>'Apart[$expr$]'
        <dd>writes $expr$ as a sum of individual fractions.
    <dt>'Apart[$expr$, $var$]'
        <dd>treats $var$ as the main variable.
    </dl>

    >> Apart[1 / (x^2 + 5x + 6)]
     = 1 / (2 + x) - 1 / (3 + x)

    When several variables are involved, the results can be different
    depending on the main variable:
    >> Apart[1 / (x^2 - y^2), x]
     = -1 / (2 y (x + y)) + 1 / (2 y (x - y))
    >> Apart[1 / (x^2 - y^2), y]
     = 1 / (2 x (x + y)) + 1 / (2 x (x - y))

    'Apart' is 'Listable':
    >> Apart[{1 / (x^2 + 5x + 6)}]
     = {1 / (2 + x) - 1 / (3 + x)}

    But it does not touch other expressions:
    >> Sin[1 / (x ^ 2 - y ^ 2)] // Apart
     = Sin[1 / (x ^ 2 - y ^ 2)]

    #> Attributes[f] = {HoldAll}; Apart[f[x + x]]
     = f[x + x]

    #> Attributes[f] = {}; Apart[f[x + x]]
     = f[2 x]
    """

    attributes = ["Listable"]
    rules = {
        "Apart[expr_]": (
            "Block[{vars = Cases[Level[expr, {-1}], _Symbol]},"
            "  If[Length[vars] > 0, Apart[expr, vars[[1]]], expr]]"
        ),
    }

    def apply(self, expr, var, evaluation):
        "Apart[expr_, var_Symbol]"

        expr_sympy = expr.to_sympy()
        var_sympy = var.to_sympy()
        if expr_sympy is None or var_sympy is None:
            return None

        try:
            result = sympy.apart(expr_sympy, var_sympy)
            result = from_sympy(result)
            return result
        except sympy.PolynomialError:
            # raised e.g. for apart(sin(1/(x**2-y**2)))
            return expr


class _Expand(Builtin):

    options = {
        "Trig": "False",
        "Modulus": "0",
    }

    messages = {
        "modn": "Value of option `1` -> `2` should be an integer.",
        "opttf": "Value of option `1` -> `2` should be True or False.",
    }

    def convert_options(self, options, evaluation):
        modulus = options["System`Modulus"]
        py_modulus = modulus.get_int_value()
        if py_modulus is None:
            return evaluation.message(
                self.get_name(), "modn", Symbol("Modulus"), modulus
            )
        if py_modulus == 0:
            py_modulus = None

        trig = options["System`Trig"]
        if trig == SymbolTrue:
            py_trig = True
        elif trig == SymbolFalse:
            py_trig = False
        else:
            return evaluation.message(self.get_name(), "opttf", Symbol("Trig"), trig)

        return {"modulus": py_modulus, "trig": py_trig}


class Expand(_Expand):
    """
    <dl>
    <dt>'Expand[$expr$]'
        <dd>expands out positive integer powers and products of sums in $expr$,
        as well as trigonometric identities.
    <dt>Expand[$expr$, $target$]
        <dd>just expands those parts involving $target$.
    </dl>

    >> Expand[(x + y) ^ 3]
     = x ^ 3 + 3 x ^ 2 y + 3 x y ^ 2 + y ^ 3
    >> Expand[(a + b) (a + c + d)]
     = a ^ 2 + a b + a c + a d + b c + b d
    >> Expand[(a + b) (a + c + d) (e + f) + e a a]
     = 2 a ^ 2 e + a ^ 2 f + a b e + a b f + a c e + a c f + a d e + a d f + b c e + b c f + b d e + b d f
    >> Expand[(a + b) ^ 2 * (c + d)]
     = a ^ 2 c + a ^ 2 d + 2 a b c + 2 a b d + b ^ 2 c + b ^ 2 d
    >> Expand[(x + y) ^ 2 + x y]
     = x ^ 2 + 3 x y + y ^ 2
    >> Expand[((a + b) (c + d)) ^ 2 + b (1 + a)]
     = a ^ 2 c ^ 2 + 2 a ^ 2 c d + a ^ 2 d ^ 2 + b + a b + 2 a b c ^ 2 + 4 a b c d + 2 a b d ^ 2 + b ^ 2 c ^ 2 + 2 b ^ 2 c d + b ^ 2 d ^ 2

    'Expand' expands items in lists and rules:
    >> Expand[{4 (x + y), 2 (x + y) -> 4 (x + y)}]
     = {4 x + 4 y, 2 x + 2 y -> 4 x + 4 y}

    'Expand' expands trigonometric identities
    >> Expand[Sin[x + y], Trig -> True]
     = Cos[x] Sin[y] + Cos[y] Sin[x]
    >> Expand[Tanh[x + y], Trig -> True]
     = Cosh[x] Sinh[y] / (Cosh[x] Cosh[y] + Sinh[x] Sinh[y]) + Cosh[y] Sinh[x] / (Cosh[x] Cosh[y] + Sinh[x] Sinh[y])

    'Expand' does not change any other expression.
    >> Expand[Sin[x (1 + y)]]
     = Sin[x (1 + y)]

    Using the second argument, the expression only
    expands those subexpressions containing $pat$:
    >> Expand[(x+a)^2+(y+a)^2+(x+y)(x+a), y]
     = a ^ 2 + 2 a y + x (a + x) + y (a + x) + y ^ 2 + (a + x) ^ 2
    'Expand' also works in Galois fields
    >> Expand[(1 + a)^12, Modulus -> 3]
     = 1 + a ^ 3 + a ^ 9 + a ^ 12

    >> Expand[(1 + a)^12, Modulus -> 4]
     = 1 + 2 a ^ 2 + 3 a ^ 4 + 3 a ^ 8 + 2 a ^ 10 + a ^ 12

    #> Expand[x, Modulus -> -1]  (* copy odd MMA behaviour *)
     = 0
    #> Expand[x, Modulus -> x]
     : Value of option Modulus -> x should be an integer.
     = Expand[x, Modulus -> x]

    #> a(b(c+d)+e) // Expand
     = a b c + a b d + a e

    #> (y^2)^(1/2)/(2x+2y)//Expand
     = Sqrt[y ^ 2] / (2 x + 2 y)


    #> 2(3+2x)^2/(5+x^2+3x)^3 // Expand
     = 24 x / (5 + 3 x + x ^ 2) ^ 3 + 8 x ^ 2 / (5 + 3 x + x ^ 2) ^ 3 + 18 / (5 + 3 x + x ^ 2) ^ 3
    """

    def apply_patt(self, expr, target, evaluation, options):
        "Expand[expr_, target_, OptionsPattern[Expand]]"

        if target.get_head_name() in ("System`Rule", "System`DelayedRule"):
            optname = target.leaves[0].get_name()
            options[optname] = target.leaves[1]
            target = None

        kwargs = self.convert_options(options, evaluation)
        if kwargs is None:
            return

        if target:
            kwargs["pattern"] = Pattern.create(target)
        kwargs["evaluation"] = evaluation
        return expand(expr, True, False, **kwargs)

    def apply(self, expr, evaluation, options):
        "Expand[expr_, OptionsPattern[Expand]]"

        kwargs = self.convert_options(options, evaluation)
        if kwargs is None:
            return

        return expand(expr, True, False, **kwargs)


class ExpandDenominator(_Expand):
    """
    <dl>
    <dt>'ExpandDenominator[$expr$]'
        <dd>expands out negative integer powers and products of sums in $expr$.
    </dl>

    >> ExpandDenominator[(a + b) ^ 2 / ((c + d)^2 (e + f))]
     = (a + b) ^ 2 / (c ^ 2 e + c ^ 2 f + 2 c d e + 2 c d f + d ^ 2 e + d ^ 2 f)

    ## Modulus option
    #> ExpandDenominator[1 / (x + y)^3, Modulus -> 3]
     = 1 / (x ^ 3 + y ^ 3)
    #> ExpandDenominator[1 / (x + y)^6, Modulus -> 4]
     = 1 / (x ^ 6 + 2 x ^ 5 y + 3 x ^ 4 y ^ 2 + 3 x ^ 2 y ^ 4 + 2 x y ^ 5 + y ^ 6)

    #> ExpandDenominator[2(3+2x)^2/(5+x^2+3x)^3]
     = 2 (3 + 2 x) ^ 2 / (125 + 225 x + 210 x ^ 2 + 117 x ^ 3 + 42 x ^ 4 + 9 x ^ 5 + x ^ 6)
    """

    def apply(self, expr, evaluation, options):
        "ExpandDenominator[expr_, OptionsPattern[ExpandDenominator]]"

        kwargs = self.convert_options(options, evaluation)
        if kwargs is None:
            return
        return expand(expr, False, True, **kwargs)


class ExpandAll(_Expand):
    """
    <dl>
    <dt>'ExpandAll[$expr$]'
        <dd>expands out negative integer powers and products of sums in $expr$.
    <dt>'ExpandAll[$expr$, $target$]'
        <dd>just expands those parts involving $target$.
    </dl>

    >> ExpandAll[(a + b) ^ 2 / (c + d)^2]
     = a ^ 2 / (c ^ 2 + 2 c d + d ^ 2) + 2 a b / (c ^ 2 + 2 c d + d ^ 2) + b ^ 2 / (c ^ 2 + 2 c d + d ^ 2)

    'ExpandAll' descends into sub expressions
    >> ExpandAll[(a + Sin[x (1 + y)])^2]
     = 2 a Sin[x + x y] + a ^ 2 + Sin[x + x y] ^ 2

    >> ExpandAll[Sin[(x+y)^2]]
     = Sin[x ^ 2 + 2 x y + y ^ 2]

    >> ExpandAll[Sin[(x+y)^2], Trig->True]
     = -Sin[x ^ 2] Sin[2 x y] Sin[y ^ 2] + Cos[x ^ 2] Cos[2 x y] Sin[y ^ 2] + Cos[x ^ 2] Cos[y ^ 2] Sin[2 x y] + Cos[2 x y] Cos[y ^ 2] Sin[x ^ 2]

    'ExpandAll' also expands heads
    >> ExpandAll[((1 + x)(1 + y))[x]]
     = (1 + x + y + x y)[x]

    'ExpandAll' can also work in finite fields
    >> ExpandAll[(1 + a) ^ 6 / (x + y)^3, Modulus -> 3]
     = (1 + 2 a ^ 3 + a ^ 6) / (x ^ 3 + y ^ 3)

    """

    def apply_patt(self, expr, target, evaluation, options):
        "ExpandAll[expr_, target_, OptionsPattern[Expand]]"
        if target.get_head_name() in ("System`Rule", "System`DelayedRule"):
            optname = target.leaves[0].get_name()
            options[optname] = target.leaves[1]
            target = None

        kwargs = self.convert_options(options, evaluation)
        if kwargs is None:
            return

        if target:
            kwargs["pattern"] = Pattern.create(target)
        kwargs["evaluation"] = evaluation
        return expand(expr, numer=True, denom=True, deep=True, **kwargs)

    def apply(self, expr, evaluation, options):
        "ExpandAll[expr_, OptionsPattern[ExpandAll]]"

        kwargs = self.convert_options(options, evaluation)
        if kwargs is None:
            return
        return expand(expr, numer=True, denom=True, deep=True, **kwargs)


class PowerExpand(Builtin):
    """
    <dl>
    <dt>'PowerExpand[$expr$]'
        <dd>expands out powers of the form '(x^y)^z' and '(x*y)^z' in $expr$.
    </dl>

    >> PowerExpand[(a ^ b) ^ c]
     = a ^ (b c)
    >> PowerExpand[(a * b) ^ c]
     = a ^ c b ^ c

    'PowerExpand' is not correct without certain assumptions:
    >> PowerExpand[(x ^ 2) ^ (1/2)]
     = x
    """

    rules = {
        "PowerExpand[(x_ ^ y_) ^ z_]": "x ^ (y * z)",
        "PowerExpand[(x_ * y_) ^ z_]": "x ^ z * y ^ z",
        "PowerExpand[Log[x_ ^ y_]]": "y * Log[x]",
        "PowerExpand[x_Plus]": "PowerExpand /@ x",
        "PowerExpand[x_Times]": "PowerExpand /@ x",
        "PowerExpand[x_Power]": "PowerExpand /@ x",
        "PowerExpand[x_List]": "PowerExpand /@ x",
        "PowerExpand[x_Rule]": "PowerExpand /@ x",
        "PowerExpand[other_]": "other",
    }


class Numerator(Builtin):
    """
    <dl>
    <dt>'Numerator[$expr$]'
        <dd>gives the numerator in $expr$.
    </dl>

    >> Numerator[a / b]
     = a
    >> Numerator[2 / 3]
     = 2
    >> Numerator[a + b]
     = a + b
    """

    attributes = ("Listable", "Protected")

    def apply(self, expr, evaluation):
        "Numerator[expr_]"

        sympy_expr = expr.to_sympy()
        if sympy_expr is None:
            return None
        numer, denom = sympy_expr.as_numer_denom()
        return from_sympy(numer)


class Denominator(Builtin):
    """
    <dl>
    <dt>'Denominator[$expr$]'
        <dd>gives the denominator in $expr$.
    </dl>

    >> Denominator[a / b]
     = b
    >> Denominator[2 / 3]
     = 3
    >> Denominator[a + b]
     = 1
    """

    attributes = ("Listable", "Protected")

    def apply(self, expr, evaluation):
        "Denominator[expr_]"

        sympy_expr = expr.to_sympy()
        if sympy_expr is None:
            return None
        numer, denom = sympy_expr.as_numer_denom()
        return from_sympy(denom)


class Variables(Builtin):
    # This builtin is incomplete. See the failing test case below.
    """
    <dl>
    <dt>'Variables[$expr$]'
        <dd>gives a list of the variables that appear in the
        polynomial $expr$.
    </dl>

    >> Variables[a x^2 + b x + c]
     = {a, b, c, x}
    >> Variables[{a + b x, c y^2 + x/2}]
     = {a, b, c, x, y}
    >> Variables[x + Sin[y]]
     = {x, Sin[y]}
    """

    """
    ## failing test case from MMA docs
    #> Variables[E^x]
     = {}
    """

    def apply(self, expr, evaluation):
        "Variables[expr_]"

        variables = find_all_vars(expr)

        variables = Expression("List", *variables)
        variables.sort()  # MMA doesn't do this
        return variables


class UpTo(Builtin):
    messages = {
        "innf": "Expected non-negative integer or infinity at position 1 in ``.",
        "argx": "UpTo expects 1 argument, `1` arguments were given.",
    }


class MinimalPolynomial(Builtin):
    """
    <dl>
    <dt>'MinimalPolynomial[s, x]'
        <dd>gives the minimal polynomial in $x$ for which the algebraic number $s$ is a root.
    </dl>

    >> MinimalPolynomial[7, x]
     = -7 + x
    >> MinimalPolynomial[Sqrt[2] + Sqrt[3], x]
     = 1 - 10 x ^ 2 + x ^ 4
    >> MinimalPolynomial[Sqrt[1 + Sqrt[3]], x]
     = -2 - 2 x ^ 2 + x ^ 4
    >> MinimalPolynomial[Sqrt[I + Sqrt[6]], x]
     = 49 - 10 x ^ 4 + x ^ 8

    #> MinimalPolynomial[7a, x]
     : 7 a is not an explicit algebraic number.
     = MinimalPolynomial[7 a, x]
    #> MinimalPolynomial[3x^3 + 2x^2 + y^2 + ab, x]
     : ab + 2 x ^ 2 + 3 x ^ 3 + y ^ 2 is not an explicit algebraic number.
     = MinimalPolynomial[ab + 2 x ^ 2 + 3 x ^ 3 + y ^ 2, x]

    ## PurePoly
    #> MinimalPolynomial[Sqrt[2 + Sqrt[3]]]
     = 1 - 4 #1 ^ 2 + #1 ^ 4
    """

    attributes = ("Listable",)

    messages = {
        "nalg": "`1` is not an explicit algebraic number.",
    }

    def apply_novar(self, s, evaluation):
        "MinimalPolynomial[s_]"
        x = Symbol("#1")
        return self.apply(s, x, evaluation)

    def apply(self, s, x, evaluation):
        "MinimalPolynomial[s_, x_]"
        variables = find_all_vars(s)
        if len(variables) > 0:
            return evaluation.message("MinimalPolynomial", "nalg", s)

        if s == SymbolNull:
            return evaluation.message("MinimalPolynomial", "nalg", s)

        sympy_s, sympy_x = s.to_sympy(), x.to_sympy()
        if sympy_s is None or sympy_x is None:
            return None
        sympy_result = sympy.minimal_polynomial(sympy_s, polys=True)(sympy_x)
        return from_sympy(sympy_result)


class PolynomialQ(Builtin):
    """
    <dl>
    <dt>'PolynomialQ[expr, var]'
        <dd>returns True if $expr$ is a polynomial in $var$, and returns False otherwise.
    <dt>'PolynomialQ[expr, {var1, ...}]'
        <dd>tests whether $expr$ is a polynomial in the $vari$.
    </dl>

    ## Form 1:
    >> PolynomialQ[x^3 - 2 x/y + 3xz, x]
     = True
    >> PolynomialQ[x^3 - 2 x/y + 3xz, y]
     = False
    >> PolynomialQ[f[a] + f[a]^2, f[a]]
     = True

    ## Form 2
    >> PolynomialQ[x^2 + axy^2 - bSin[c], {x, y}]
     = True
    >> PolynomialQ[x^2 + axy^2 - bSin[c], {a, b, c}]
     = False

    #> PolynomialQ[x, x, y]
     : PolynomialQ called with 3 arguments; 1 or 2 arguments are expected.
     = PolynomialQ[x, x, y]

    ## Always return True if argument is Null
    #> PolynomialQ[x^3 - 2 x/y + 3xz,]
     : Warning: comma encountered with no adjacent expression. The expression will be treated as Null (line 1 of "<test>").
     = True
    #> PolynomialQ[, {x, y, z}]
     : Warning: comma encountered with no adjacent expression. The expression will be treated as Null (line 1 of "<test>").
     = True
    #> PolynomialQ[, ]
     : Warning: comma encountered with no adjacent expression. The expression will be treated as Null (line 1 of "<test>").
     : Warning: comma encountered with no adjacent expression. The expression will be treated as Null (line 1 of "<test>").
     = True

    ## TODO: MMA and Sympy handle these cases differently
    ## #> PolynomialQ[x^(1/2) + 6xyz]
    ##  : No variable is not supported in PolynomialQ.
    ##  = True
    ## #> PolynomialQ[x^(1/2) + 6xyz, {}]
    ##  : No variable is not supported in PolynomialQ.
    ##  = True

    ## #> PolynomialQ[x^3 - 2 x/y + 3xz]
    ##  : No variable is not supported in PolynomialQ.
    ##  = False
    ## #> PolynomialQ[x^3 - 2 x/y + 3xz, {}]
    ##  : No variable is not supported in PolynomialQ.
    ##  = False
    """

    messages = {
        "argt": "PolynomialQ called with `1` arguments; 1 or 2 arguments are expected.",
        "novar": "No variable is not supported in PolynomialQ.",
    }

    def apply(self, expr, v, evaluation):
        "PolynomialQ[expr_, v___]"
        if expr == SymbolNull:
            return SymbolTrue

        v = v.get_sequence()
        if len(v) > 1:
            return evaluation.message("PolynomialQ", "argt", Integer(len(v) + 1))
        elif len(v) == 0:
            return evaluation.message("PolynomialQ", "novar")

        var = v[0]
        if var == SymbolNull:
            return SymbolTrue
        elif var.has_form("List", None):
            if len(var.leaves) == 0:
                return evaluation.message("PolynomialQ", "novar")
            sympy_var = [x.to_sympy() for x in var.leaves]
        else:
            sympy_var = [var.to_sympy()]

        sympy_expr = expr.to_sympy()
        sympy_result = sympy_expr.is_polynomial(*[x for x in sympy_var])
        return SymbolTrue if sympy_result else SymbolFalse


# Get a coefficient of form in an expression
def _coefficient(name, expr, form, n, evaluation):
    if expr == SymbolNull or form == SymbolNull or n == SymbolNull:
        return Integer0

    if not (isinstance(form, Symbol)) and not (isinstance(form, Expression)):
        return evaluation.message(name, "ivar", form)

    sympy_exprs = expr.to_sympy().as_ordered_terms()
    sympy_var = form.to_sympy()
    sympy_n = n.to_sympy()

    def combine_exprs(exprs):
        result = 0
        for e in exprs:
            result += e
        return result

    # expand sub expressions if they contain variables
    sympy_exprs = [
        sympy.expand(e) if sympy_var.free_symbols.issubset(e.free_symbols) else e
        for e in sympy_exprs
    ]
    sympy_expr = combine_exprs(sympy_exprs)
    sympy_result = sympy_expr.coeff(sympy_var, sympy_n)
    return from_sympy(sympy_result)


class Coefficient(Builtin):
    """
    <dl>
    <dt>'Coefficient[expr, form]'
        <dd>returns the coefficient of $form$ in the polynomial $expr$.
    <dt>'Coefficient[expr, form, n]'
        <dd>return the coefficient of $form$^$n$ in $expr$.
    </dl>

    ## Form 1: Coefficent[expr, form]
    >> Coefficient[(x + y)^4, (x^2) * (y^2)]
     = 6
    >> Coefficient[a x^2 + b y^3 + c x + d y + 5, x]
     = c
    >> Coefficient[(x + 3 y)^5, x]
     = 405 y ^ 4
    >> Coefficient[(x + 3 y)^5, x * y^4]
     = 405
    >> Coefficient[(x + 2)/(y - 3) + (x + 3)/(y - 2), x]
     = 1 / (-3 + y) + 1 / (-2 + y)
    >> Coefficient[x*Cos[x + 3] + 6*y, x]
     = Cos[3 + x]

    ## Form 2: Coefficent[expr, form, n]
    >> Coefficient[(x + 1)^3, x, 2]
     = 3
    >> Coefficient[a x^2 + b y^3 + c x + d y + 5, y, 3]
     = b

    Find the free term in a polynomial:
    >> Coefficient[(x + 2)^3 + (x + 3)^2, x, 0]
     = 17
    >> Coefficient[(x + 2)^3 + (x + 3)^2, y, 0]
     = (2 + x) ^ 3 + (3 + x) ^ 2
    >> Coefficient[a x^2 + b y^3 + c x + d y + 5, x, 0]
     = 5 + b y ^ 3 + d y

    ## Errors:
    #> Coefficient[x + y + 3]
     : Coefficient called with 1 argument; 2 or 3 arguments are expected.
     = Coefficient[3 + x + y]
    #> Coefficient[x + y + 3, 5]
     : 5 is not a valid variable.
     = Coefficient[3 + x + y, 5]

    ## This is known bug of Sympy 1.0, next Sympy version will fix it by this commit
    ## https://github.com/sympy/sympy/commit/25bf64b64d4d9a2dc563022818d29d06bc740d47
    ## #> Coefficient[x * y, z, 0]
    ##  = x y
    ##  ## Sympy 1.0 retuns 0

    ## ## TODO: Support Modulus
    ## >> Coefficient[(x + 2)^3 + (x + 3)^2, x, 0, Modulus -> 3]
    ##  = 2
    ## #> Coefficient[(x + 2)^3 + (x + 3)^2, x, 0, {Modulus -> 3, Modulus -> 2, Modulus -> 10}]
    ##  = {2, 1, 7}
    """

    messages = {
        "argtu": "Coefficient called with 1 argument; 2 or 3 arguments are expected.",
        "ivar": "`1` is not a valid variable.",
    }

    attributes = ("Listable",)

    def apply_noform(self, expr, evaluation):
        "Coefficient[expr_]"
        return evaluation.message("Coefficient", "argtu")

    def apply(self, expr, form, evaluation):
        "Coefficient[expr_, form_]"
        return _coefficient(self.__class__.__name__, expr, form, Integer1, evaluation)

    def apply_n(self, expr, form, n, evaluation):
        "Coefficient[expr_, form_, n_]"
        return _coefficient(self.__class__.__name__, expr, form, n, evaluation)


class CoefficientList(Builtin):
    """
    <dl>
    <dt>'CoefficientList[poly, var]'
        <dd>returns a list of coefficients of powers of $var$ in $poly$, starting with power 0.
    <dt>'CoefficientList[poly, {var1, var2, ...}]'
        <dd>returns an array of coefficients of the $vari$.
    </dl>

    ## Form 1 CoefficientList[poly, var]
    >> CoefficientList[(x + 3)^5, x]
     = {243, 405, 270, 90, 15, 1}
    >> CoefficientList[(x + y)^4, x]
     = {y ^ 4, 4 y ^ 3, 6 y ^ 2, 4 y, 1}
    >> CoefficientList[a x^2 + b y^3 + c x + d y + 5, x]
     = {5 + b y ^ 3 + d y, c, a}
    >> CoefficientList[(x + 2)/(y - 3) + x/(y - 2), x]
     = {2 / (-3 + y), 1 / (-3 + y) + 1 / (-2 + y)}
    >> CoefficientList[(x + y)^3, z]
     = {(x + y) ^ 3}
    #> CoefficientList[x + y, 5]
     : 5 is not a valid variable.
     = CoefficientList[x + y, 5]

    ## Form 2 CoefficientList[poly, {var1, var2, ...}]
    >> CoefficientList[a x^2 + b y^3 + c x + d y + 5, {x, y}]
     = {{5, d, 0, b}, {c, 0, 0, 0}, {a, 0, 0, 0}}
    >> CoefficientList[(x - 2 y + 3 z)^3, {x, y, z}]
     = {{{0, 0, 0, 27}, {0, 0, -54, 0}, {0, 36, 0, 0}, {-8, 0, 0, 0}}, {{0, 0, 27, 0}, {0, -36, 0, 0}, {12, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 9, 0, 0}, {-6, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{1, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}}
    #> CoefficientList[(x - 2 y)^4, {x, 2}]
     : 2 is not a valid variable.
     = CoefficientList[(x - 2 y) ^ 4, {x, 2}]
    #> CoefficientList[x / y, {x, y}]
     : x / y is not a polynomial.
     = CoefficientList[x / y, {x, y}]
    """

    messages = {
        "argtu": "CoefficientList called with 1 argument; 2 or 3 arguments are expected.",
        "ivar": "`1` is not a valid variable.",
        "poly": "`1` is not a polynomial.",
    }

    def apply_noform(self, expr, evaluation):
        "CoefficientList[expr_]"
        return evaluation.message("CoefficientList", "argtu")

    def apply(self, expr, form, evaluation):
        "CoefficientList[expr_, form_]"
        vars = [form] if not form.has_form("List", None) else [v for v in form.leaves]

        # check form is not a variable
        for v in vars:
            if not (isinstance(v, Symbol)) and not (isinstance(v, Expression)):
                return evaluation.message("CoefficientList", "ivar", v)

        # special cases for expr and form
        e_null = expr == SymbolNull
        f_null = form == SymbolNull
        if expr == Integer0:
            return Expression("List")
        elif e_null and f_null:
            return Expression(SymbolList, Integer0)
        elif e_null and not f_null:
            return Expression("List", SymbolNull)
        elif f_null:
            return Expression("List", expr)
        elif form.has_form("List", 0):
            return expr

        sympy_expr = expr.to_sympy()
        sympy_vars = [v.to_sympy() for v in vars]

        if not sympy_expr.is_polynomial(*[x for x in sympy_vars]):
            return evaluation.message("CoefficientList", "poly", expr)

        try:
            sympy_poly, sympy_opt = sympy.poly_from_expr(sympy_expr, sympy_vars)
            dimensions = [
                sympy_poly.degree(x) if x in sympy_poly.gens else 0 for x in sympy_vars
            ]

            # single & multiple variables cases
            if not form.has_form("List", None):
                return Expression(
                    "List",
                    *[
                        _coefficient(
                            self.__class__.__name__, expr, form, Integer(n), evaluation
                        )
                        for n in range(dimensions[0] + 1)
                    ]
                )
            elif form.has_form("List", 1):
                form = form.leaves[0]
                return Expression(
                    "List",
                    *[
                        _coefficient(
                            self.__class__.__name__, expr, form, Integer(n), evaluation
                        )
                        for n in range(dimensions[0] + 1)
                    ]
                )
            else:

                def _nth(poly, dims, exponents):
                    if not dims:
                        return from_sympy(poly.coeff_monomial(exponents))

                    leaves = []
                    first_dim = dims[0]
                    for i in range(first_dim + 1):
                        exponents.append(i)
                        subs = _nth(poly, dims[1:], exponents)
                        leaves.append(subs)
                        exponents.pop()
                    result = Expression("List", *leaves)
                    return result

                return _nth(sympy_poly, dimensions, [])
        except sympy.PolificationFailed:
            return evaluation.message("CoefficientList", "poly", expr)


class Exponent(Builtin):
    """
    <dl>
    <dt>'Exponent[expr, form]'
        <dd>returns the maximum power with which $form$ appears in the expanded form of $expr$.
    <dt>'Exponent[expr, form, h]'
        <dd>applies $h$ to the set of exponents with which $form$ appears in $expr$.
    </dl>

    >> Exponent[5 x^2 - 3 x + 7, x]
     = 2
    >> Exponent[(x^3 + 1)^2 + 1, x]
     = 6
    >> Exponent[x^(n + 1) + Sqrt[x] + 1, x]
     = Max[1 / 2, 1 + n]
    >> Exponent[x / y, y]
     = -1

    >> Exponent[(x^2 + 1)^3 - 1, x, Min]
     = 2

    >> Exponent[0, x]
     = -Infinity
    >> Exponent[1, x]
     = 0

    ## errors:
    #> Exponent[x^2]
     : Exponent called with 1 argument; 2 or 3 arguments are expected.
     = Exponent[x ^ 2]
    """

    attributes = ("Listable", "Protected")

    messages = {
        "argtu": "Exponent called with `1` argument; 2 or 3 arguments are expected.",
    }

    rules = {
        "Exponent[expr_, form_]": "Exponent[expr, form, Max]",
    }

    def apply_novar(self, expr, evaluation):
        "Exponent[expr_]"
        return evaluation.message("Exponent", "argtu", Integer1)

    def apply(self, expr, form, h, evaluation):
        "Exponent[expr_, form_, h_]"
        if expr == Integer0:
            return Expression("DirectedInfinity", Integer(-1))

        if not form.has_form("List", None):
            return Expression(h, *[i for i in find_exponents(expr, form)])
        else:
            exponents = [find_exponents(expr, var) for var in form.leaves]
            return Expression(
                "List", *[Expression(h, *[i for i in s]) for s in exponents]
            )


class _CoefficientHandler(Builtin):
    def coeff_power_internal(self, expr, var_exprs, filt, evaluation, form="expr"):
        from mathics.builtin.patterns import match

        if len(var_exprs) == 0:
            if form == "expr":
                return expr
            else:
                return [([], expr)]
        if len(var_exprs) == 1:
            target_pat = Pattern.create(var_exprs[0])
            var_pats = [target_pat]
        else:
            target_pat = Pattern.create(Expression("Alternatives", *var_exprs))
            var_pats = [Pattern.create(var) for var in var_exprs]

        ####### Auxiliary functions #########
        def key_powers(lst):
            key = Expression("Plus", *lst)
            key = key.evaluate(evaluation)
            if key.is_numeric():
                return key.to_python()
            return 0

        def powers_list(pf):
            powers = [Integer0 for i, p in enumerate(var_pats)]
            if pf is None:
                return powers
            if pf.is_symbol():
                for i, pat in enumerate(var_pats):
                    if match(pf, pat, evaluation):
                        powers[i] = Integer(1)
                        return powers
            if pf.has_form("Sqrt", 1):
                for i, pat in enumerate(var_pats):
                    if match(pf._leaves[0], pat, evaluation):
                        powers[i] = RationalOneHalf
                        return powers
            if pf.has_form("Power", 2):
                for i, pat in enumerate(var_pats):
                    matchval = match(pf._leaves[0], pat, evaluation)
                    if matchval:
                        powers[i] = pf._leaves[1]
                        return powers
            if pf.has_form("Times", None):
                contrib = [powers_list(factor) for factor in pf._leaves]
                for i in range(len(var_pats)):
                    powers[i] = Expression("Plus", *[c[i] for c in contrib]).evaluate(
                        evaluation
                    )
                return powers
            return powers

        def split_coeff_pow(term):
            """
            This function factorizes term in a coefficent free
            of powers of the target variables, and a factor with
            that powers.
            """
            coeffs = []
            powers = []
            # First, split factors on those which are powers of the variables
            # and the rest.
            if term.is_free(target_pat, evaluation):
                coeffs.append(term)
            elif (
                term.is_symbol()
                or term.has_form("Power", 2)
                or term.has_form("Sqrt", 1)
            ):
                powers.append(term)
            elif term.has_form("Times", None):
                for factor in term.leaves:
                    if factor.is_free(target_pat, evaluation):
                        coeffs.append(factor)
                    elif match(factor, target_pat, evaluation):
                        powers.append(factor)
                    elif (
                        factor.has_form("Power", 2) or factor.has_form("Sqrt", 1)
                    ) and match(factor._leaves[0], target_pat, evaluation):
                        powers.append(factor)
                    else:
                        coeffs.append(factor)
            else:
                coeffs.append(term)
            # Now, rebuild both factors
            if len(coeffs) == 0:
                coeffs = None
            elif len(coeffs) == 1:
                coeffs = coeffs[0]
            else:
                coeffs = Expression("Times", *coeffs)
            if len(powers) == 0:
                powers = None
            elif len(powers) == 1:
                powers = powers[0]
            else:
                powers = Expression("Times", *sorted(powers))
            return coeffs, powers

        #################  The actual begin ####################
        expr = expand(
            expr,
            numer=True,
            denom=False,
            deep=False,
            trig=False,
            modulus=None,
            target_pat=target_pat,
        )

        if expr.is_free(target_pat, evaluation):
            if filt:
                expr = Expression(filt, expr).evaluate(evaluation)
            if form == "expr":
                return expr
            else:
                return [(powers_list(None), expr)]
        elif (
            expr.is_symbol()
            or match(expr, target_pat, evaluation)
            or expr.has_form("Power", 2)
            or expr.has_form("Sqrt", 1)
        ):
            coeff = (
                Expression(filt, Integer1).evaluate(evaluation) if filt else Integer1
            )
            if form == "expr":
                if coeff is Integer1:
                    return expr
                else:
                    return Expression("Times", coeff, expr)
            else:
                if not coeff.is_free(target_pat, evaluation):
                    return []
                return [(powers_list(expr), coeff)]
        elif expr.has_form("Times", None):
            coeff, powers = split_coeff_pow(expr)
            if coeff is None:
                coeff = Integer1
            else:
                if form != "expr" and not coeff.is_free(target_pat, evaluation):
                    return []
            if filt:
                coeff = Expression(filt, coeff).evaluate(evaluation)

            if form == "expr":
                if powers is None:
                    return coeff
                else:
                    if coeff is Integer1:
                        return powers
                    else:
                        return Expression("Times", coeff, powers)
            else:
                pl = powers_list(powers)
                return [(pl, coeff)]
        elif expr.has_form("Plus", None):
            coeff_dict = {}
            powers_dict = {}
            powers_order = {}
            for term in expr._leaves:
                coeff, powers = split_coeff_pow(term)
                if (
                    form != "expr"
                    and coeff is not None
                    and not coeff.is_free(target_pat, evaluation)
                ):
                    return []
                pl = powers_list(powers)
                key = str(pl)
                if not key in powers_dict:
                    if form == "expr":
                        powers_dict[key] = powers
                    else:
                        # TODO: check if pl is a monomial...
                        powers_dict[key] = pl
                    coeff_dict[key] = []
                    powers_order[key] = key_powers(pl)

                coeff_dict[key].append(Integer1 if coeff is None else coeff)

            terms = []
            for key in sorted(
                coeff_dict, key=lambda kv: powers_order[kv], reverse=False
            ):
                val = coeff_dict[key]
                if len(val) == 0:
                    continue
                elif len(val) == 1:
                    coeff = val[0]
                else:
                    coeff = Expression("Plus", *val)
                if filt:
                    coeff = Expression(filt, coeff).evaluate(evaluation)

                powerfactor = powers_dict[key]
                if form == "expr":
                    if powerfactor:
                        terms.append(Expression("Times", coeff, powerfactor))
                    else:
                        terms.append(coeff)
                else:
                    terms.append([powerfactor, coeff])
            if form == "expr":
                return Expression("Plus", *terms)
            else:
                return terms
        else:
            # expr is not a polynomial.
            if form == "expr":
                if filt:
                    expr = Expression(filt, expr).evaluate(evaluation)
                return expr
            else:
                return []


class CoefficientArrays(_CoefficientHandler):
    """
    <dl>
    <dt>'CoefficientArrays[$polys$, $vars$]'
        <dd>returns a list of arrays of coefficients of the variables $vars$ in the polynomial  $poly$.
    </dl>

    >> CoefficientArrays[1 + x^3, x]
     = {1, {0}, {{0}}, {{{1}}}}
    >> CoefficientArrays[1 + x y+ x^3, {x, y}]
     = {1, {0, 0}, {{0, 1}, {0, 0}}, {{{1, 0}, {0, 0}}, {{0, 0}, {0, 0}}}}
    >> CoefficientArrays[{1 + x^2, x y}, {x, y}]
     = {{1, 0}, {{0, 0}, {0, 0}}, {{{1, 0}, {0, 0}}, {{0, 1}, {0, 0}}}}
    >> CoefficientArrays[(x+y+Sin[z])^3, {x,y}]
     = {Sin[z] ^ 3, {3 Sin[z] ^ 2, 3 Sin[z] ^ 2}, {{3 Sin[z], 6 Sin[z]}, {0, 3 Sin[z]}}, {{{1, 3}, {0, 3}}, {{0, 0}, {0, 1}}}}
    >> CoefficientArrays[(x + y + Sin[z])^3, {x, z}]
     : (x + y + Sin[z]) ^ 3 is not a polynomial in {x, z}
     = CoefficientArrays[(x + y + Sin[z]) ^ 3, {x, z}]
    """

    options = {
        "Symmetric": "False",
    }
    messages = {
        "poly": "`1` is not a polynomial in `2`",
    }

    def apply_list(self, polys, varlist, evaluation, options):
        "%(name)s[polys_, varlist_, OptionsPattern[]]"
        from mathics.builtin.lists import walk_parts

        if polys.has_form("List", None):
            list_polys = polys.leaves
        else:
            list_polys = [polys]

        if varlist.is_symbol():
            var_exprs = [varlist]
        elif varlist.has_form("List", None):
            var_exprs = varlist.get_leaves()
        else:
            var_exprs = [varlist]

        coeffs = [
            self.coeff_power_internal(pol, var_exprs, None, evaluation, "coeffs")
            for pol in list_polys
        ]

        dim1 = len(coeffs)
        dim2 = len(var_exprs)
        arrays = []
        if dim1 == 1:
            arrays.append(Integer(0))
        for i, component in enumerate(coeffs):
            if len(component) == 0:
                evaluation.message("CoefficientArrays", "poly", polys, varlist)
                return
            for idxcoeff in component:
                idx, coeff = idxcoeff
                order = Expression("Plus", *idx).evaluate(evaluation).get_int_value()
                if order is None:
                    evaluation.message("CoefficientArrays", "poly", polys, varlist)
                    return
                while len(arrays) <= order:
                    cur_ord = len(arrays)
                    range2 = Expression(SymbolList, Integer(dim2))
                    its2 = [range2 for k in range(cur_ord)]
                    # TODO: Use SparseArray...
                    # This constructs a tensor or range cur_ord+1
                    if dim1 > 1:
                        newtable = Expression(
                            "Table",
                            Integer(0),
                            Expression(SymbolList, Integer(dim1)),
                            *its2
                        )
                    else:
                        newtable = Expression("Table", Integer(0), *its2)
                    arrays.append(newtable.evaluate(evaluation))
                curr_array = arrays[order]
                arrayidx = [
                    Integer(n + 1)
                    for n, j in enumerate(idx)
                    for q in range(j.get_int_value())
                ]
                if dim1 > 1:
                    arrayidx = [Integer(i + 1)] + arrayidx
                if dim1 == 1 and order == 0:
                    arrays[0] = coeff
                else:
                    arrays[order] = walk_parts(
                        [curr_array], arrayidx, evaluation, coeff
                    )
        return Expression("List", *arrays)


class Collect(_CoefficientHandler):
    """
    <dl>
    <dt>'Collect[$expr$, $x$]'
    <dd> Expands $expr$ and collect together terms having the same power of $x$.
    <dt>'Collect[$expr$, {$x_1$, $x_2$, ...}]'
    <dd> Expands $expr$ and collect together terms having the same powers of
         $x_1$, $x_2$, ....
    <dt>'Collect[$expr$, {$x_1$, $x_2$, ...}, $filter$]'
    <dd> After collect the terms, applies $filter$ to each coefficient.
    </dl>

    >> Collect[(x+y)^3, y]
     =  x ^ 3 + 3 x ^ 2 y + 3 x y ^ 2 + y ^ 3
    >> Collect[2 Sin[x z] (x+2 y^2 + Sin[y] x), y]
     = 2 x Sin[x z] + 2 x Sin[x z] Sin[y] + 4 y ^ 2 Sin[x z]
    >> Collect[3 x y+2 Sin[x z] (x+2 y^2 + x) + (x+y)^3, y]
     = 4 x Sin[x z] + x ^ 3 + y (3 x + 3 x ^ 2) + y ^ 2 (3 x + 4 Sin[x z]) + y ^ 3
    >> Collect[3 x y+2 Sin[x z] (x+2 y^2 + x) + (x+y)^3, {x,y}]
     = 4 x Sin[x z] + x ^ 3 + 3 x y + 3 x ^ 2 y + 4 y ^ 2 Sin[x z] + 3 x y ^ 2 + y ^ 3
    >> Collect[3 x y+2 Sin[x z] (x+2 y^2 + x) + (x+y)^3, {x,y}, h]
     = x h[4 Sin[x z]] + x ^ 3 h[1] + x y h[3] + x ^ 2 y h[3] + y ^ 2 h[4 Sin[x z]] + x y ^ 2 h[3] + y ^ 3 h[1]
    """

    rules = {
        "Collect[expr_, varlst_]": "Collect[expr, varlst, Identity]",
    }

    def apply_var_filter(self, expr, varlst, filt, evaluation):
        """Collect[expr_, varlst_, filt_]"""
        if filt == Symbol("Identity"):
            filt = None
        if varlst.is_symbol():
            var_exprs = [varlst]
        elif varlst.has_form("List", None):
            var_exprs = varlst.get_leaves()
        else:
            var_exprs = [varlst]

        return self.coeff_power_internal(expr, var_exprs, filt, evaluation, "expr")
