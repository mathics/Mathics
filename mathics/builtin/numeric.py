# cython: language_level=3
# -*- coding: utf-8 -*-


"""
Numeric Evaluation

Support for numeric evaluation with arbitrary precision is just a
proof-of-concept.
Precision is not "guarded" through the evaluation process. Only
integer precision is supported.
However, things like 'N[Pi, 100]' should work as expected.
"""
from mathics.version import __version__  # noqa used in loading to check consistency.
import sympy
import mpmath
import numpy as np
import math
import hashlib
import zlib
from collections import namedtuple
from contextlib import contextmanager
from itertools import chain, product
from functools import lru_cache


from mathics.builtin.base import Builtin, Predefined
from mathics.core.numbers import (
    dps,
    convert_int_to_digit_list,
    machine_precision,
    machine_epsilon,
    get_precision,
    PrecisionValueError,
)
from mathics.core.expression import (
    Complex,
    Expression,
    Integer,
    MachineReal,
    Number,
    Rational,
    Real,
    String,
    Symbol,
    SymbolFalse,
    SymbolTrue,
    SymbolList,
    SymbolN,
    from_python,
)
from mathics.core.convert import from_sympy


@lru_cache(maxsize=1024)
def log_n_b(py_n, py_b) -> int:
    return int(mpmath.ceil(mpmath.log(py_n, py_b))) if py_n != 0 and py_n != 1 else 1


class N(Builtin):
    """
    <dl>
    <dt>'N[$expr$, $prec$]'
        <dd>evaluates $expr$ numerically with a precision of $prec$ digits.
    </dl>
    >> N[Pi, 50]
     = 3.1415926535897932384626433832795028841971693993751

    >> N[1/7]
     = 0.142857

    >> N[1/7, 5]
     = 0.14286

    You can manually assign numerical values to symbols.
    When you do not specify a precision, 'MachinePrecision' is taken.
    >> N[a] = 10.9
     = 10.9
    >> a
     = a

    'N' automatically threads over expressions, except when a symbol has
     attributes 'NHoldAll', 'NHoldFirst', or 'NHoldRest'.
    >> N[a + b]
     = 10.9 + b
    >> N[a, 20]
     = a
    >> N[a, 20] = 11;
    >> N[a + b, 20]
     = 11.000000000000000000 + b
    >> N[f[a, b]]
     = f[10.9, b]
    >> SetAttributes[f, NHoldAll]
    >> N[f[a, b]]
     = f[a, b]

    The precision can be a pattern:
    >> N[c, p_?(#>10&)] := p
    >> N[c, 3]
     = c
    >> N[c, 11]
     = 11.000000000

    You can also use 'UpSet' or 'TagSet' to specify values for 'N':
    >> N[d] ^= 5;
    However, the value will not be stored in 'UpValues', but
    in 'NValues' (as for 'Set'):
    >> UpValues[d]
     = {}
    >> NValues[d]
     = {HoldPattern[N[d, MachinePrecision]] :> 5}
    >> e /: N[e] = 6;
    >> N[e]
     = 6.

    Values for 'N[$expr$]' must be associated with the head of $expr$:
    >> f /: N[e[f]] = 7;
     : Tag f not found or too deep for an assigned rule.

    You can use 'Condition':
    >> N[g[x_, y_], p_] := x + y * Pi /; x + y > 3
    >> SetAttributes[g, NHoldRest]
    >> N[g[1, 1]]
     = g[1., 1]
    >> N[g[2, 2]] // InputForm
     = 8.283185307179586

    The precision of the result is no higher than the precision of the input
    >> N[Exp[0.1], 100]
     = 1.10517
    >> % // Precision
     = MachinePrecision
    >> N[Exp[1/10], 100]
     = 1.105170918075647624811707826490246668224547194737518718792863289440967966747654302989143318970748654
    >> % // Precision
     = 100.
    >> N[Exp[1.0`20], 100]
     = 2.7182818284590452354
    >> % // Precision
     = 20.

    #> p=N[Pi,100]
     = 3.141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117068
    #> ToString[p]
     = 3.141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117068
    #> 3.14159 * "a string"
     = 3.14159 a string

    #> N[Pi, Pi]
     = 3.14

    #> N[1/9, 30]
     = 0.111111111111111111111111111111
    #> Precision[%]
     = 30.

    #> N[1.5, 30]
     = 1.5
    #> Precision[%]
     = MachinePrecision
    #> N[1.5, 5]
     = 1.5
    #> Precision[%]
     = MachinePrecision

    #> {N[x], N[x, 30], N["abc"], N["abc", 30]}
     = {x, x, abc, abc}

    #> N[I, 30]
     = 1.00000000000000000000000000000 I

    #> N[1.01234567890123456789]
     = 1.01235
    #> N[1.012345678901234567890123, 20]
     = 1.0123456789012345679
    #> N[1.012345678901234567890123, 5]
     = 1.0123
    #> % // Precision
     = 5.
    #> N[1.012345678901234567890123, 50]
     = 1.01234567890123456789012
    #> % // Precision
     = 24.

    #> N[1.01234567890123456789`]
     = 1.01235
    #> N[1.01234567890123456789`, 20]
     = 1.01235
    #> % // Precision
     = MachinePrecision
    #> N[1.01234567890123456789`, 2]
     = 1.01235
    #> % // Precision
     = MachinePrecision
    """

    messages = {
        "precbd": ("Requested precision `1` is not a " + "machine-sized real number."),
        "preclg": (
            "Requested precision `1` is larger than $MaxPrecision. "
            + "Using current $MaxPrecision of `2` instead. "
            + "$MaxPrecision = Infinity specifies that any precision "
            + "should be allowed."
        ),
        "precsm": (
            "Requested precision `1` is smaller than "
            + "$MinPrecision. Using current $MinPrecision of "
            + "`2` instead."
        ),
    }

    rules = {
        "N[expr_]": "N[expr, MachinePrecision]",
    }

    def apply_with_prec(self, expr, prec, evaluation):
        "N[expr_, prec_]"

        try:
            d = get_precision(prec, evaluation)
        except PrecisionValueError:
            return

        if expr.get_head_name() in ("System`List", "System`Rule"):
            return Expression(
                expr.head,
                *[self.apply_with_prec(leaf, prec, evaluation) for leaf in expr.leaves],
            )

        # Special case for the Root builtin
        if expr.has_form("Root", 2):
            return from_sympy(sympy.N(expr.to_sympy(), d))

        if isinstance(expr, Number):
            return expr.round(d)

        name = expr.get_lookup_name()
        if name != "":
            nexpr = Expression(SymbolN, expr, prec)
            result = evaluation.definitions.get_value(
                name, "System`NValues", nexpr, evaluation
            )
            if result is not None:
                if not result.same(nexpr):
                    result = Expression(SymbolN, result, prec).evaluate(evaluation)
                return result

        if expr.is_atom():
            return expr
        else:
            attributes = expr.head.get_attributes(evaluation.definitions)
            if "System`NHoldAll" in attributes:
                eval_range = ()
            elif "System`NHoldFirst" in attributes:
                eval_range = range(1, len(expr.leaves))
            elif "System`NHoldRest" in attributes:
                if len(expr.leaves) > 0:
                    eval_range = (0,)
                else:
                    eval_range = ()
            else:
                eval_range = range(len(expr.leaves))
            head = Expression(SymbolN, expr.head, prec).evaluate(evaluation)
            leaves = expr.get_mutable_leaves()
            for index in eval_range:
                leaves[index] = Expression(SymbolN, leaves[index], prec).evaluate(
                    evaluation
                )
            return Expression(head, *leaves)


def _scipy_interface(integrator, options_map, mandatory=None, adapt_func=None):
    """
    This function provides a proxy for scipy.integrate
    functions, adapting the parameters.
    """

    def _scipy_proxy_func_filter(fun, a, b, **opts):
        native_opts = {}
        if mandatory:
            native_opts.update(mandatory)
        for opt, val in opts.items():
            native_opt = options_map.get(opt, None)
            if native_opt:
                if native_opt[1]:
                    val = native_opt[1](val)
                native_opts[native_opt[0]] = val
        return adapt_func(integrator(fun, a, b, **native_opts))

    def _scipy_proxy_func(fun, a, b, **opts):
        native_opts = {}
        if mandatory:
            native_opts.update(mandatory)
        for opt, val in opts.items():
            native_opt = options_map.get(opt, None)
            if native_opt:
                if native_opt[1]:
                    val = native_opt[1](val)
                native_opts[native_opt[0]] = val
        return integrator(fun, a, b, **native_opts)

    return _scipy_proxy_func_filter if adapt_func else _scipy_proxy_func


def _internal_adaptative_simpsons_rule(f, a, b, **opts):
    """
    1D adaptative Simpson's rule integrator
    Adapted from https://en.wikipedia.org/wiki/Adaptive_Simpson%27s_method
       by @mmatera

    TODO: handle weak divergences
    """
    wsr = 1.0 / 6.0

    tol = opts.get("tol")
    if not tol:
        tol = 1.0e-10

    maxrec = opts.get("maxrec")
    if not maxrec:
        maxrec = 150

    def _quad_simpsons_mem(f, a, fa, b, fb):
        """Evaluates the Simpson's Rule, also returning m and f(m) to reuse"""
        m = 0.5 * (a + b)
        try:
            fm = f(m)
        except ZeroDivisionError:
            fm = None

        if fm is None or np.isinf(fm):
            m = m + 1e-10
            fm = f(m)
        return (m, fm, wsr * abs(b - a) * (fa + 4.0 * fm + fb))

    def _quad_asr(f, a, fa, b, fb, eps, whole, m, fm, maxrec):
        """
        Efficient recursive implementation of adaptive Simpson's rule.
        Function values at the start, middle, end of the intervals
        are retained.
        """
        maxrec = maxrec - 1
        try:
            left = _quad_simpsons_mem(f, a, fa, m, fm)
            lm, flm, left = left
            right = _quad_simpsons_mem(f, m, fm, b, fb)
            rm, frm, right = right

            delta = left + right - whole
            err = abs(delta)
            if err <= 15 * eps or maxrec == 0:
                return (left + right + delta / 15, err)
            left = _quad_asr(f, a, fa, m, fm, 0.5 * eps, left, lm, flm, maxrec)
            right = _quad_asr(f, m, fm, b, fb, 0.5 * eps, right, rm, frm, maxrec)
            return (left[0] + right[0], left[1] + right[1])
        except Exception:
            raise

    def ensure_evaluation(f, x):
        try:
            val = f(x)
        except ZeroDivisionError:
            return None
        if np.isinf(val):
            return None
        return val

    invert_interval = False
    if a > b:
        b, a, invert_interval = a, b, True

    fa, fb = ensure_evaluation(f, a), ensure_evaluation(f, b)
    if fa is None:
        x = 10.0 * machine_epsilon if a == 0 else a * (1.0 + 10.0 * machine_epsilon)
        fa = ensure_evaluation(f, x)
        if fa is None:
            raise Exception(f"Function undefined around {a}. Cannot integrate")
    if fb is None:
        x = -10.0 * machine_epsilon if b == 0 else b * (1.0 - 10.0 * machine_epsilon)
        fb = ensure_evaluation(f, x)
        if fb is None:
            raise Exception(f"Function undefined around {b}. Cannot integrate")

    m, fm, whole = _quad_simpsons_mem(f, a, fa, b, fb)
    if invert_interval:
        return -_quad_asr(f, a, fa, b, fb, tol, whole, m, fm, maxrec)
    else:
        return _quad_asr(f, a, fa, b, fb, tol, whole, m, fm, maxrec)


def _fubini(func, ranges, **opts):
    if not ranges:
        return 0.0
    a, b = ranges[0]
    integrator = opts["integrator"]
    tol = opts.get("tol")
    if tol is None:
        opts["tol"] = 1.0e-10
        tol = 1.0e-10

    if len(ranges) > 1:

        def subintegral(*u):
            def ff(*z):
                return func(*(u + z))

            val = _fubini(ff, ranges[1:], **opts)[0]
            return val

        opts["tol"] = 4.0 * tol
        val = integrator(subintegral, a, b, **opts)
        return val
    else:
        val = integrator(func, a, b, **opts)
        return val


class NIntegrate(Builtin):
    """
    <dl>
       <dt>'NIntegrate[$expr$, $interval$]'
       <dd>returns a numeric approximation to the definite integral of $expr$ with limits $interval$ and with a precision of $prec$ digits.

        <dt>'NIntegrate[$expr$, $interval1$, $interval2$, ...]'
        <dd>returns a numeric approximation to the multiple integral of $expr$ with limits $interval1$, $interval2$ and with a precision of $prec$ digits.
    </dl>

    >> NIntegrate[Exp[-x],{x,0,Infinity},Tolerance->1*^-6]
     = 1.
    >> NIntegrate[Exp[x],{x,-Infinity, 0},Tolerance->1*^-6]
     = 1.
    >> NIntegrate[Exp[-x^2/2.],{x,-Infinity, Infinity},Tolerance->1*^-6]
     = 2.50663

    >> Table[1./NIntegrate[x^k,{x,0,1},Tolerance->1*^-6], {k,0,6}]
     : The specified method failed to return a number. Falling back into the internal evaluator.
     = {1., 2., 3., 4., 5., 6., 7.}

    >> NIntegrate[1 / z, {z, -1 - I, 1 - I, 1 + I, -1 + I, -1 - I}, Tolerance->1.*^-4]
     : Integration over a complex domain is not implemented yet
     = NIntegrate[1 / z, {z, -1 - I, 1 - I, 1 + I, -1 + I, -1 - I}, Tolerance -> 0.0001]
     ## = 6.2832 I

    Integrate singularities with weak divergences:
    >> Table[ NIntegrate[x^(1./k-1.), {x,0,1.}, Tolerance->1*^-6], {k,1,7.} ]
     = {1., 2., 3., 4., 5., 6., 7.}

    Mutiple Integrals :
    >> NIntegrate[x * y,{x, 0, 1}, {y, 0, 1}]
     = 0.25

    """

    messages = {
        "bdmtd": "The Method option should be a built-in method name.",
        "inumr": (
            "The integrand `1` has evaluated to non-numerical "
            + "values for all sampling points in the region "
            + "with boundaries `2`"
        ),
        "nlim": "`1` = `2` is not a valid limit of integration.",
        "ilim": "Invalid integration variable or limit(s) in `1`.",
        "mtdfail": (
            "The specified method failed to return a "
            + "number. Falling back into the internal "
            + "evaluator."
        ),
        "cmpint": ("Integration over a complex domain is not " + "implemented yet"),
    }

    options = {
        "Method": '"Automatic"',
        "Tolerance": "1*^-10",
        "Accuracy": "1*^-10",
        "MaxRecursion": "10",
    }

    methods = {
        "Automatic": (None, False),
    }

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.methods["Internal"] = (_internal_adaptative_simpsons_rule, False)
        try:
            from scipy.integrate import romberg, quad, nquad

            self.methods["NQuadrature"] = (
                _scipy_interface(
                    nquad, {}, {"full_output": 1}, lambda res: (res[0], res[1])
                ),
                True,
            )
            self.methods["Quadrature"] = (
                _scipy_interface(
                    quad,
                    {
                        "tol": ("epsabs", None),
                        "maxrec": ("limit", lambda maxrec: int(2 ** maxrec)),
                    },
                    {"full_output": 1},
                    lambda res: (res[0], res[1]),
                ),
                False,
            )
            self.methods["Romberg"] = (
                _scipy_interface(
                    romberg,
                    {"tol": ("tol", None), "maxrec": ("divmax", None)},
                    None,
                    lambda x: (x, np.nan),
                ),
                False,
            )
            self.methods["Automatic"] = self.methods["Quadrature"]
        except Exception:
            self.methods["Automatic"] = self.methods["Internal"]
            self.methods["Simpson"] = self.methods["Internal"]

        self.messages["bdmtd"] = (
            "The Method option should be a "
            + "built-in method name in {`"
            + "`, `".join(list(self.methods))
            + "`}. Using `Automatic`"
        )

    @staticmethod
    def decompose_domain(interval, evaluation):
        if interval.has_form("System`Sequence", 1, None):
            intervals = []
            for leaf in interval.leaves:
                inner_interval = NIntegrate.decompose_domain(leaf, evaluation)
                if inner_interval:
                    intervals.append(inner_interval)
                else:
                    evaluation.message("ilim", leaf)
                    return None
            return intervals

        if interval.has_form("System`List", 3, None):
            intervals = []
            intvar = interval.leaves[0]
            if not isinstance(intvar, Symbol):
                evaluation.message("ilim", interval)
                return None
            boundaries = [a for a in interval.leaves[1:]]
            if any([b.get_head_name() == "System`Complex" for b in boundaries]):
                intvar = Expression(
                    "List", intvar, Expression("Blank", Symbol("Complex"))
                )
            for i in range(len(boundaries) - 1):
                intervals.append((boundaries[i], boundaries[i + 1]))
            if len(intervals) > 0:
                return (intvar, intervals)

        evaluation.message("ilim", interval)
        return None

    def apply_with_func_domain(self, func, domain, evaluation, options):
        "%(name)s[func_, domain__, OptionsPattern[%(name)s]]"
        method = options["System`Method"].evaluate(evaluation)
        method_options = {}
        if method.has_form("System`List", 2):
            method = method.leaves[0]
            method_options.update(method.leaves[1].get_option_values())
        if isinstance(method, String):
            method = method.value
        elif isinstance(method, Symbol):
            method = method.get_name()
        else:
            evaluation.message("NIntegrate", "bdmtd", method)
            return
        tolerance = options["System`Tolerance"].evaluate(evaluation)
        tolerance = float(tolerance.value)
        accuracy = options["System`Accuracy"].evaluate(evaluation)
        accuracy = accuracy.value
        maxrecursion = options["System`MaxRecursion"].evaluate(evaluation)
        maxrecursion = maxrecursion.value
        nintegrate_method = self.methods.get(method, None)
        if nintegrate_method is None:
            evaluation.message("NIntegrate", "bdmtd", method)
            nintegrate_method = self.methods.get("Automatic")
        if type(nintegrate_method) is tuple:
            nintegrate_method, is_multidimensional = nintegrate_method
        else:
            is_multidimensional = False

        domain = self.decompose_domain(domain, evaluation)
        if not domain:
            return
        if not isinstance(domain, list):
            domain = [domain]

        coords = [axis[0] for axis in domain]
        # If any of the points in the integration domain is complex,
        # stop the evaluation...
        if any([c.get_head_name() == "System`List" for c in coords]):
            evaluation.message("NIntegrate", "cmpint")
            return

        intvars = Expression(SymbolList, *coords)
        integrand = Expression("Compile", intvars, func).evaluate(evaluation)

        if len(integrand.leaves) >= 3:
            integrand = integrand.leaves[2].cfunc
        else:
            evaluation.message("inumer", func, domain)
            return
        results = []
        for subdomain in product(*[axis[1] for axis in domain]):
            # On each subdomain, check if the region is bounded.
            # If not, implement a coordinate map
            func2 = integrand
            subdomain2 = []
            coordtransform = []
            nulldomain = False
            for i, r in enumerate(subdomain):
                a = r[0].evaluate(evaluation)
                b = r[1].evaluate(evaluation)
                if a == b:
                    nulldomain = True
                    break
                elif a.get_head_name() == "System`DirectedInfinity":
                    if b.get_head_name() == "System`DirectedInfinity":
                        a = a.to_python()
                        b = b.to_python()
                        le = 1 - machine_epsilon
                        if a == b:
                            nulldomain = True
                            break
                        elif a < b:
                            subdomain2.append([-le, le])
                        else:
                            subdomain2.append([le, -le])
                        coordtransform.append(
                            (np.arctanh, lambda u: 1.0 / (1.0 - u ** 2))
                        )
                    else:
                        if not b.is_numeric():
                            evaluation.message("nlim", coords[i], b)
                            return
                        z = a.leaves[0].value
                        b = b.value
                        subdomain2.append([machine_epsilon, 1.0])
                        coordtransform.append(
                            (lambda u: b - z + z / u, lambda u: -z * u ** (-2.0))
                        )
                elif b.get_head_name() == "System`DirectedInfinity":
                    if not a.is_numeric():
                        evaluation.message("nlim", coords[i], a)
                        return
                    a = a.value
                    z = b.leaves[0].value
                    subdomain2.append([machine_epsilon, 1.0])
                    coordtransform.append(
                        (lambda u: a - z + z / u, lambda u: z * u ** (-2.0))
                    )
                elif a.is_numeric() and b.is_numeric():
                    a = Expression(SymbolN, a).evaluate(evaluation).value
                    b = Expression(SymbolN, b).evaluate(evaluation).value
                    subdomain2.append([a, b])
                    coordtransform.append(None)
                else:
                    for x in (a, b):
                        if not x.is_numeric():
                            evaluation.message("nlim", coords[i], x)
                    return

            if nulldomain:
                continue

            if any(coordtransform):
                func2 = lambda *u: (
                    integrand(
                        *[
                            x[0](u[i]) if x else u[i]
                            for i, x in enumerate(coordtransform)
                        ]
                    )
                    * np.prod(
                        [jac[1](u[i]) for i, jac in enumerate(coordtransform) if jac]
                    )
                )
            opts = {
                "acur": accuracy,
                "tol": tolerance,
                "maxrec": maxrecursion,
            }
            opts.update(method_options)
            try:
                if len(subdomain2) > 1:
                    if is_multidimensional:
                        nintegrate_method(func2, subdomain2, **opts)
                    else:
                        val = _fubini(
                            func2, subdomain2, integrator=nintegrate_method, **opts
                        )
                else:
                    val = nintegrate_method(func2, *(subdomain2[0]), **opts)
            except Exception:
                val = None

            if val is None:
                evaluation.message("NIntegrate", "mtdfail")
                if len(subdomain2) > 1:
                    val = _fubini(
                        func2,
                        subdomain2,
                        integrator=_internal_adaptative_simpsons_rule,
                        **opts,
                    )
                else:
                    val = _internal_adaptative_simpsons_rule(
                        func2, *(subdomain2[0]), **opts
                    )
            results.append(val)

        result = sum([r[0] for r in results])
        # error = sum([r[1] for r in results]) -> use it when accuracy
        #                                         be implemented...
        return from_python(result)


class MachinePrecision(Predefined):
    """
    <dl>
    <dt>'MachinePrecision'
        <dd>represents the precision of machine precision numbers.
    </dl>

    >> N[MachinePrecision]
     = 15.9546
    >> N[MachinePrecision, 30]
     = 15.9545897701910033463281614204

    #> N[E, MachinePrecision]
     = 2.71828

    #> Round[MachinePrecision]
     = 16
    """

    rules = {
        "N[MachinePrecision, prec_]": ("N[Log[10, 2] * %i, prec]" % machine_precision),
    }


class MachineEpsilon_(Predefined):
    """
    <dl>
    <dt>'$MachineEpsilon'
        <dd>is the distance between '1.0' and the next
            nearest representable machine-precision number.
    </dl>

    >> $MachineEpsilon
     = 2.22045*^-16

    >> x = 1.0 + {0.4, 0.5, 0.6} $MachineEpsilon;
    >> x - 1
     = {0., 0., 2.22045*^-16}
    """

    name = "$MachineEpsilon"

    def evaluate(self, evaluation):
        return MachineReal(machine_epsilon)


class MachinePrecision_(Predefined):
    """
    <dl>
    <dt>'$MachinePrecision'
        <dd>is the number of decimal digits of precision for
            machine-precision numbers.
    </dl>

    >> $MachinePrecision
     = 15.9546
    """

    name = "$MachinePrecision"

    rules = {
        "$MachinePrecision": "N[MachinePrecision]",
    }


class Precision(Builtin):
    """
    <dl>
    <dt>'Precision[$expr$]'
        <dd>examines the number of significant digits of $expr$.
    </dl>
    This is rather a proof-of-concept than a full implementation.
    Precision of compound expression is not supported yet.
    >> Precision[1]
     = Infinity
    >> Precision[1/2]
     = Infinity
    >> Precision[0.5]
     = MachinePrecision

    #> Precision[0.0]
     = MachinePrecision
    #> Precision[0.000000000000000000000000000000000000]
     = 0.
    #> Precision[-0.0]
     = MachinePrecision
    #> Precision[-0.000000000000000000000000000000000000]
     = 0.

    #> 1.0000000000000000 // Precision
     = MachinePrecision
    #> 1.00000000000000000 // Precision
     = 17.

    #> 0.4 + 2.4 I // Precision
     = MachinePrecision
    #> Precision[2 + 3 I]
     = Infinity

    #> Precision["abc"]
     = Infinity
    """

    rules = {
        "Precision[z_?MachineNumberQ]": "MachinePrecision",
    }

    def apply(self, z, evaluation):
        "Precision[z_]"

        if not z.is_inexact():
            return Symbol("Infinity")
        elif z.to_sympy().is_zero:
            return Real(0)
        else:
            return Real(dps(z.get_precision()))


class MinPrecision(Builtin):
    """
    <dl>
    <dt>'$MinPrecision'
      <dd>represents the minimum number of digits of precision
          permitted in abitrary-precision numbers.
    </dl>

    >> $MinPrecision
     = 0

    >> $MinPrecision = 10;

    >> N[Pi, 9]
     : Requested precision 9 is smaller than $MinPrecision. Using current $MinPrecision of 10. instead.
     = 3.141592654

    #> N[Pi, 10]
     = 3.141592654

    #> $MinPrecision = x
     : Cannot set $MinPrecision to x; value must be a non-negative number.
     = x
    #> $MinPrecision = -Infinity
     : Cannot set $MinPrecision to -Infinity; value must be a non-negative number.
     = -Infinity
    #> $MinPrecision = -1
     : Cannot set $MinPrecision to -1; value must be a non-negative number.
     = -1
    #> $MinPrecision = 0;

    #> $MaxPrecision = 10;
    #> $MinPrecision = 15
     : Cannot set $MinPrecision such that $MaxPrecision < $MinPrecision.
     = 15
    #> $MinPrecision
     = 0
    #> $MaxPrecision = Infinity;
    """

    name = "$MinPrecision"
    rules = {
        "$MinPrecision": "0",
    }

    messages = {
        "precset": "Cannot set `1` to `2`; value must be a non-negative number.",
        "preccon": "Cannot set `1` such that $MaxPrecision < $MinPrecision.",
    }


class MaxPrecision(Predefined):
    """
    <dl>
    <dt>'$MaxPrecision'
      <dd>represents the maximum number of digits of precision
          permitted in abitrary-precision numbers.
    </dl>

    >> $MaxPrecision
     = Infinity

    >> $MaxPrecision = 10;

    >> N[Pi, 11]
     : Requested precision 11 is larger than $MaxPrecision. Using current $MaxPrecision of 10. instead. $MaxPrecision = Infinity specifies that any precision should be allowed.
     = 3.141592654

    #> N[Pi, 10]
     = 3.141592654

    #> $MaxPrecision = x
     : Cannot set $MaxPrecision to x; value must be a positive number or Infinity.
     = x
    #> $MaxPrecision = -Infinity
     : Cannot set $MaxPrecision to -Infinity; value must be a positive number or Infinity.
     = -Infinity
    #> $MaxPrecision = 0
     : Cannot set $MaxPrecision to 0; value must be a positive number or Infinity.
     = 0
    #> $MaxPrecision = Infinity;

    #> $MinPrecision = 15;
    #> $MaxPrecision = 10
     : Cannot set $MaxPrecision such that $MaxPrecision < $MinPrecision.
     = 10
    #> $MaxPrecision
     = Infinity
    #> $MinPrecision = 0;
    """

    name = "$MaxPrecision"

    rules = {
        "$MaxPrecision": "Infinity",
    }

    messages = {
        "precset": "Cannot set `1` to `2`; value must be a positive number or Infinity.",
        "preccon": "Cannot set `1` such that $MaxPrecision < $MinPrecision.",
    }


class Round(Builtin):
    """
    <dl>
    <dt>'Round[$expr$]'
        <dd>rounds $expr$ to the nearest integer.
    <dt>'Round[$expr$, $k$]'
        <dd>rounds $expr$ to the closest multiple of $k$.
    </dl>

    >> Round[10.6]
     = 11
    >> Round[0.06, 0.1]
     = 0.1
    >> Round[0.04, 0.1]
     = 0.

    Constants can be rounded too
    >> Round[Pi, .5]
     = 3.
    >> Round[Pi^2]
     = 10

    Round to exact value
    >> Round[2.6, 1/3]
     = 8 / 3
    >> Round[10, Pi]
     = 3 Pi

    Round complex numbers
    >> Round[6/(2 + 3 I)]
     = 1 - I
    >> Round[1 + 2 I, 2 I]
     = 2 I

    Round Negative numbers too
    >> Round[-1.4]
     = -1

    Expressions other than numbers remain unevaluated:
    >> Round[x]
     = Round[x]
    >> Round[1.5, k]
     = Round[1.5, k]
    """

    attributes = ("Listable", "NumericFunction")

    rules = {
        "Round[expr_?NumericQ]": "Round[Re[expr], 1] + I * Round[Im[expr], 1]",
        "Round[expr_Complex, k_?RealNumberQ]": (
            "Round[Re[expr], k] + I * Round[Im[expr], k]"
        ),
    }

    def apply(self, expr, k, evaluation):
        "Round[expr_?NumericQ, k_?NumericQ]"

        n = Expression("Divide", expr, k).round_to_float(
            evaluation, permit_complex=True
        )
        if n is None:
            return
        elif isinstance(n, complex):
            n = round(n.real)
        else:
            n = round(n)
        n = int(n)
        return Expression("Times", Integer(n), k)


class Rationalize(Builtin):
    """
    <dl>
    <dt>'Rationalize[$x$]'
        <dd>converts a real number $x$ to a nearby rational number.
    <dt>'Rationalize[$x$, $dx$]'
        <dd>finds the rational number within $dx$ of $x$ with the smallest denominator.
    </dl>

    >> Rationalize[2.2]
    = 11 / 5

    Not all numbers can be well approximated.
    >> Rationalize[N[Pi]]
     = 3.14159

    Find the exact rational representation of 'N[Pi]'
    >> Rationalize[N[Pi], 0]
     = 245850922 / 78256779

    #> Rationalize[1.6 + 0.8 I]
     = 8 / 5 + 4 I / 5

    #> Rationalize[N[Pi] + 0.8 I, 1*^-6]
     = 355 / 113 + 4 I / 5

    #> Rationalize[N[Pi] + 0.8 I, x]
     : Tolerance specification x must be a non-negative number.
     = Rationalize[3.14159 + 0.8 I, x]

    #> Rationalize[N[Pi] + 0.8 I, -1]
     : Tolerance specification -1 must be a non-negative number.
     = Rationalize[3.14159 + 0.8 I, -1]

    #> Rationalize[N[Pi] + 0.8 I, 0]
     = 245850922 / 78256779 + 4 I / 5

    #> Rationalize[17 / 7]
     = 17 / 7

    #> Rationalize[x]
     = x

    #> Table[Rationalize[E, 0.1^n], {n, 1, 10}]
     = {8 / 3, 19 / 7, 87 / 32, 193 / 71, 1071 / 394, 2721 / 1001, 15062 / 5541, 23225 / 8544, 49171 / 18089, 419314 / 154257}

    #> Rationalize[x, y]
     : Tolerance specification y must be a non-negative number.
     = Rationalize[x, y]
    """

    messages = {
        "tolnn": "Tolerance specification `1` must be a non-negative number.",
    }

    rules = {
        "Rationalize[z_Complex]": "Rationalize[Re[z]] + I Rationalize[Im[z]]",
        "Rationalize[z_Complex, dx_?Internal`RealValuedNumberQ]/;dx >= 0": "Rationalize[Re[z], dx] + I Rationalize[Im[z], dx]",
    }

    def apply(self, x, evaluation):
        "Rationalize[x_]"

        py_x = x.to_sympy()
        if py_x is None or (not py_x.is_number) or (not py_x.is_real):
            return x
        return from_sympy(self.find_approximant(py_x))

    @staticmethod
    def find_approximant(x):
        c = 1e-4
        it = sympy.ntheory.continued_fraction_convergents(
            sympy.ntheory.continued_fraction_iterator(x)
        )
        for i in it:
            p, q = i.as_numer_denom()
            tol = c / q ** 2
            if abs(i - x) <= tol:
                return i
            if tol < machine_epsilon:
                break
        return x

    @staticmethod
    def find_exact(x):
        p, q = x.as_numer_denom()
        it = sympy.ntheory.continued_fraction_convergents(
            sympy.ntheory.continued_fraction_iterator(x)
        )
        for i in it:
            p, q = i.as_numer_denom()
            if abs(x - i) < machine_epsilon:
                return i

    def apply_dx(self, x, dx, evaluation):
        "Rationalize[x_, dx_]"
        py_x = x.to_sympy()
        if py_x is None:
            return x
        py_dx = dx.to_sympy()
        if (
            py_dx is None
            or (not py_dx.is_number)
            or (not py_dx.is_real)
            or py_dx.is_negative
        ):
            return evaluation.message("Rationalize", "tolnn", dx)
        elif py_dx == 0:
            return from_sympy(self.find_exact(py_x))
        a = self.approx_interval_continued_fraction(py_x - py_dx, py_x + py_dx)
        sym_x = sympy.ntheory.continued_fraction_reduce(a)
        return Rational(sym_x)

    @staticmethod
    def approx_interval_continued_fraction(xmin, xmax):
        result = []
        a_gen = sympy.ntheory.continued_fraction_iterator(xmin)
        b_gen = sympy.ntheory.continued_fraction_iterator(xmax)
        while True:
            a, b = next(a_gen), next(b_gen)
            if a == b:
                result.append(a)
            else:
                result.append(min(a, b) + 1)
                break
        return result


def chop(expr, delta=10.0 ** (-10.0)):
    if isinstance(expr, Real):
        if -delta < expr.get_float_value() < delta:
            return Integer(0)
    elif isinstance(expr, Complex) and expr.is_inexact():
        real, imag = expr.real, expr.imag
        if -delta < real.get_float_value() < delta:
            real = Integer(0)
        if -delta < imag.get_float_value() < delta:
            imag = Integer(0)
        return Complex(real, imag)
    elif isinstance(expr, Expression):
        return Expression(chop(expr.head), *[chop(leaf) for leaf in expr.leaves])
    return expr


class Chop(Builtin):
    """
    <dl>
    <dt>'Chop[$expr$]'
        <dd>replaces floating point numbers close to 0 by 0.
    <dt>'Chop[$expr$, $delta$]'
        <dd>uses a tolerance of $delta$. The default tolerance is '10^-10'.
    </dl>

    >> Chop[10.0 ^ -16]
     = 0
    >> Chop[10.0 ^ -9]
     = 1.*^-9
    >> Chop[10 ^ -11 I]
     = I / 100000000000
    >> Chop[0. + 10 ^ -11 I]
     = 0
    """

    messages = {
        "tolnn": "Tolerance specification a must be a non-negative number.",
    }

    rules = {
        "Chop[expr_]": "Chop[expr, 10^-10]",
    }

    def apply(self, expr, delta, evaluation):
        "Chop[expr_, delta_:(10^-10)]"

        delta = delta.round_to_float(evaluation)
        if delta is None or delta < 0:
            return evaluation.message("Chop", "tolnn")

        return chop(expr, delta=delta)


class NumericQ(Builtin):
    """
    <dl>
    <dt>'NumericQ[$expr$]'
        <dd>tests whether $expr$ represents a numeric quantity.
    </dl>

    >> NumericQ[2]
     = True
    >> NumericQ[Sqrt[Pi]]
     = True
    >> NumberQ[Sqrt[Pi]]
     = False
    """

    def apply(self, expr, evaluation):
        "NumericQ[expr_]"

        def test(expr):
            if isinstance(expr, Expression):
                attr = evaluation.definitions.get_attributes(expr.head.get_name())
                return "System`NumericFunction" in attr and all(
                    test(leaf) for leaf in expr.leaves
                )
            else:
                return expr.is_numeric()

        return SymbolTrue if test(expr) else SymbolFalse


class RealValuedNumericQ(Builtin):
    """
    #> Internal`RealValuedNumericQ /@ {1, N[Pi], 1/2, Sin[1.], Pi, 3/4, aa,  I}
     = {True, True, True, True, True, True, False, False}
    """

    context = "Internal`"

    rules = {
        "Internal`RealValuedNumericQ[x_]": "Head[N[x]] === Real",
    }


class RealValuedNumberQ(Builtin):
    """
    #>  Internal`RealValuedNumberQ /@ {1, N[Pi], 1/2, Sin[1.], Pi, 3/4, aa, I}
     = {True, True, True, True, False, True, False, False}
    """

    context = "Internal`"

    rules = {
        "Internal`RealValuedNumberQ[x_Real]": "True",
        "Internal`RealValuedNumberQ[x_Integer]": "True",
        "Internal`RealValuedNumberQ[x_Rational]": "True",
        "Internal`RealValuedNumberQ[x_]": "False",
    }


class IntegerDigits(Builtin):
    """
    <dl>
    <dt>'IntegerDigits[$n$]'
        <dd>returns a list of the base-10 digits in the integer $n$.
    <dt>'IntegerDigits[$n$, $base$]'
        <dd>returns a list of the base-$base$ digits in $n$.
    <dt>'IntegerDigits[$n$, $base$, $length$]'
        <dd>returns a list of length $length$, truncating or padding
        with zeroes on the left as necessary.
    </dl>

    >> IntegerDigits[76543]
     = {7, 6, 5, 4, 3}

    The sign of $n$ is discarded:
    >> IntegerDigits[-76543]
     = {7, 6, 5, 4, 3}

    >> IntegerDigits[15, 16]
     = {15}
    >> IntegerDigits[1234, 16]
     = {4, 13, 2}
    >> IntegerDigits[1234, 10, 5]
     = {0, 1, 2, 3, 4}

    #> IntegerDigits[1000, 10]
     = {1, 0, 0, 0}

    #> IntegerDigits[0]
     = {0}
    """

    attributes = ("Listable",)

    messages = {
        "int": "Integer expected at position 1 in `1`",
        "ibase": "Base `1` is not an integer greater than 1.",
    }

    rules = {
        "IntegerDigits[n_]": "IntegerDigits[n, 10]",
    }

    def apply_len(self, n, base, length, evaluation):
        "IntegerDigits[n_, base_, length_]"

        if not (isinstance(length, Integer) and length.get_int_value() >= 0):
            return evaluation.message("IntegerDigits", "intnn")

        return self.apply(n, base, evaluation, nr_elements=length.get_int_value())

    def apply(self, n, base, evaluation, nr_elements=None):
        "IntegerDigits[n_, base_]"

        if not (isinstance(n, Integer)):
            return evaluation.message(
                "IntegerDigits", "int", Expression("IntegerDigits", n, base)
            )

        if not (isinstance(base, Integer) and base.get_int_value() > 1):
            return evaluation.message("IntegerDigits", "ibase", base)

        if nr_elements == 0:
            # trivial case: we don't want any digits
            return Expression(SymbolList)

        digits = convert_int_to_digit_list(n.get_int_value(), base.get_int_value())

        if nr_elements is not None:
            if len(digits) >= nr_elements:
                # Truncate, preserving the digits on the right
                digits = digits[-nr_elements:]
            else:
                # Pad with zeroes
                digits = [0] * (nr_elements - len(digits)) + digits

        return Expression(SymbolList, *digits)


def check_finite_decimal(denominator):
    # The rational number is finite decimal if the denominator has form 2^a * 5^b
    while denominator % 5 == 0:
        denominator = denominator / 5

    while denominator % 2 == 0:
        denominator = denominator / 2

    return True if denominator == 1 else False


def convert_repeating_decimal(numerator, denominator, base):
    head = [x for x in str(numerator // denominator)]
    tails = []
    subresults = [numerator % denominator]
    numerator %= denominator

    while numerator != 0:  # only rational input can go to this case
        numerator *= base
        result_digit, numerator = divmod(numerator, denominator)
        tails.append(str(result_digit))
        if numerator not in subresults:
            subresults.append(numerator)
        else:
            break

    for i in range(len(head) - 1, -1, -1):
        j = len(tails) - 1
        if head[i] != tails[j]:
            break
        else:
            del tails[j]
            tails.insert(0, head[i])
            del head[i]
            j = j - 1

    # truncate all leading 0's
    if all(elem == "0" for elem in head):
        for i in range(0, len(tails)):
            if tails[0] == "0":
                tails = tails[1:] + [str(0)]
            else:
                break
    return (head, tails)


def convert_float_base(x, base, precision=10):

    length_of_int = 0 if x == 0 else int(mpmath.log(x, base))
    # iexps = list(range(length_of_int, -1, -1))

    def convert_int(x, base, exponents):
        out = []
        for e in range(0, exponents + 1):
            d = x % base
            out.append(d)
            x = x / base
            if x == 0:
                break
        out.reverse()
        return out

    def convert_float(x, base, exponents):
        out = []
        for e in range(0, exponents):
            d = int(x * base)
            out.append(d)
            x = (x * base) - d
            if x == 0:
                break
        return out

    int_part = convert_int(int(x), base, length_of_int)
    if isinstance(x, (float, sympy.Float)):
        # fexps = list(range(-1, -int(precision + 1), -1))
        real_part = convert_float(x - int(x), base, precision + 1)
        return int_part + real_part
    elif isinstance(x, int):
        return int_part
    else:
        raise TypeError(x)


class RealDigits(Builtin):
    """
    <dl>
    <dt>'RealDigits[$n$]'
        <dd>returns the decimal representation of the real number $n$ as list of digits, together with the number of digits that are to the left of the decimal point.

    <dt>'RealDigits[$n$, $b$]'
        <dd>returns a list of base_$b$ representation of the real number $n$.

    <dt>'RealDigits[$n$, $b$, $len$]'
        <dd>returns a list of $len$ digits.

    <dt>'RealDigits[$n$, $b$, $len$, $p$]'
        <dd>return $len$ digits starting with the coefficient of $b$^$p$
    </dl>

    Return the list of digits and exponent:
    >> RealDigits[123.55555]
     = {{1, 2, 3, 5, 5, 5, 5, 5, 0, 0, 0, 0, 0, 0, 0, 0}, 3}

    >> RealDigits[0.000012355555]
     = {{1, 2, 3, 5, 5, 5, 5, 5, 0, 0, 0, 0, 0, 0, 0, 0}, -4}

    >> RealDigits[-123.55555]
     = {{1, 2, 3, 5, 5, 5, 5, 5, 0, 0, 0, 0, 0, 0, 0, 0}, 3}

    #> RealDigits[0.004]
     = {{4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, -2}

    #> RealDigits[-1.25, -1]
     : Base -1 is not a real number greater than 1.
     = RealDigits[-1.25, -1]

    Return 25 digits of in base 10:
    >> RealDigits[Pi, 10, 25]
     = {{3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7, 9, 3, 2, 3, 8, 4, 6, 2, 6, 4, 3}, 1}

    #> RealDigits[19 / 7, 10, 25]
     = {{2, 7, 1, 4, 2, 8, 5, 7, 1, 4, 2, 8, 5, 7, 1, 4, 2, 8, 5, 7, 1, 4, 2, 8, 5}, 1}

    Return an explicit recurring decimal form:
    >> RealDigits[19 / 7]
     = {{2, {7, 1, 4, 2, 8, 5}}, 1}

    #> RealDigits[100 / 21]
     = {{{4, 7, 6, 1, 9, 0}}, 1}

    #> RealDigits[1.234, 2, 15]
     = {{1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 1}, 1}

    20 digits starting with the coefficient of 10^-5:
    >> RealDigits[Pi, 10, 20, -5]
     = {{9, 2, 6, 5, 3, 5, 8, 9, 7, 9, 3, 2, 3, 8, 4, 6, 2, 6, 4, 3}, -4}

    #> RealDigits[Pi, 10, 20, 5]
     = {{0, 0, 0, 0, 0, 3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7, 9}, 6}

    The 10000th digit of  is an 8:
    >> RealDigits[Pi, 10, 1, -10000]
     = {{8}, -9999}

    #> RealDigits[Pi]
     : The number of digits to return cannot be determined.
     = RealDigits[Pi]

    #> RealDigits[20 / 3]
     = {{{6}}, 1}

    #> RealDigits[3 / 4]
     = {{7, 5}, 0}

    #> RealDigits[23 / 4]
     = {{5, 7, 5}, 1}

    #> RealDigits[3 + 4 I]
     : The value 3 + 4 I is not a real number.
     = RealDigits[3 + 4 I]

    #> RealDigits[abc]
     = RealDigits[abc]

    #> RealDigits[abc, 2]
     = RealDigits[abc, 2]

    #> RealDigits[45]
     = {{4, 5}, 2}

    #> RealDigits[{3.14, 4.5}]
     = {{{3, 1, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, 1}, {{4, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, 1}}

    #> RealDigits[123.45, 40]
     = {{3, 3, 18, 0, 0, 0, 0, 0, 0, 0}, 2}

    #> RealDigits[0.00012345, 2]
     = {{1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0}, -12}

    #> RealDigits[12345, 2, 4]
     = {{1, 1, 0, 0}, 14}

    #> RealDigits[123.45, 2, 15]
     = {{1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1}, 7}

    RealDigits gives Indeterminate if more digits than the precision are requested:
    >> RealDigits[123.45, 10, 18]
     = {{1, 2, 3, 4, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, Indeterminate, Indeterminate}, 3}

    #> RealDigits[0.000012345, 2]
     = {{1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 0, 1}, -16}

    #> RealDigits[3.14, 10, 1.5]
     : Non-negative machine-sized integer expected at position 3 in RealDigits[3.14, 10, 1.5].
     = RealDigits[3.14, 10, 1.5]

    #> RealDigits[3.14, 10, 1, 1.5]
     : Machine-sized integer expected at position 4 in RealDigits[3.14, 10, 1, 1.5].
     = RealDigits[3.14, 10, 1, 1.5]

    #> RealDigits[Pi, 10, 20, -5]
     = {{9, 2, 6, 5, 3, 5, 8, 9, 7, 9, 3, 2, 3, 8, 4, 6, 2, 6, 4, 3}, -4}

    #> RealDigits[305.0123, 10, 17, 0]
     = {{5, 0, 1, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, Indeterminate, Indeterminate, Indeterminate}, 1}

    #> RealDigits[220, 140]
     = {{1, 80}, 2}

    #> RealDigits[Sqrt[3], 10, 50]
     = {{1, 7, 3, 2, 0, 5, 0, 8, 0, 7, 5, 6, 8, 8, 7, 7, 2, 9, 3, 5, 2, 7, 4, 4, 6, 3, 4, 1, 5, 0, 5, 8, 7, 2, 3, 6, 6, 9, 4, 2, 8, 0, 5, 2, 5, 3, 8, 1, 0, 3}, 1}

    #> RealDigits[0]
     = {{0}, 1}

    #> RealDigits[1]
     = {{1}, 1}

    #> RealDigits[0, 10, 5]
     = {{0, 0, 0, 0, 0}, 0}

    #> RealDigits[11/23]
     = {{{4, 7, 8, 2, 6, 0, 8, 6, 9, 5, 6, 5, 2, 1, 7, 3, 9, 1, 3, 0, 4, 3}}, 0}

    #> RealDigits[1/97]
     = {{{1, 0, 3, 0, 9, 2, 7, 8, 3, 5, 0, 5, 1, 5, 4, 6, 3, 9, 1, 7, 5, 2, 5, 7, 7, 3, 1, 9, 5, 8, 7, 6, 2, 8, 8, 6, 5, 9, 7, 9, 3, 8, 1, 4, 4, 3, 2, 9, 8, 9, 6, 9, 0, 7, 2, 1, 6, 4, 9, 4, 8, 4, 5, 3, 6, 0, 8, 2, 4, 7, 4, 2, 2, 6, 8, 0, 4, 1, 2, 3, 7, 1, 1, 3, 4, 0, 2, 0, 6, 1, 8, 5, 5, 6, 7, 0}}, -1}

    #> RealDigits[1/97, 2]
     = {{{1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0}}, -6}

    #> RealDigits[1/197, 260, 5]
     = {{1, 83, 38, 71, 69}, 0}

    #> RealDigits[1/197, 260, 5, -6]
     = {{246, 208, 137, 67, 80}, -5}

    #> RealDigits[Pi, 260, 20]
     = {{3, 36, 211, 172, 124, 173, 210, 42, 162, 76, 23, 206, 122, 187, 23, 245, 241, 225, 254, 98}, 1}

    #> RealDigits[Pi, 260, 5]
     = {{3, 36, 211, 172, 124}, 1}

    #> RealDigits[1/3]
     = {{{3}}, 0}

    #> RealDigits[1/2, 7]
     = {{{3}}, 0}

    #> RealDigits[3/2, 7]
     = {{1, {3}}, 1}

    #> RealDigits[-3/2, 7]
     = {{1, {3}}, 1}

    #> RealDigits[3/2, 6]
     = {{1, 3}, 1}

    #> RealDigits[1, 7, 5]
     = {{1, 0, 0, 0, 0}, 1}

    #> RealDigits[I, 7]
     : The value I is not a real number.
     = RealDigits[I, 7]

    #> RealDigits[-Pi]
     : The number of digits to return cannot be determined.
     = RealDigits[-Pi]

    #> RealDigits[Round[x + y]]
     = RealDigits[Round[x + y]]

    """

    attributes = ("Listable",)

    messages = {
        "realx": "The value `1` is not a real number.",
        "ndig": "The number of digits to return cannot be determined.",
        "rbase": "Base `1` is not a real number greater than 1.",
        "intnm": "Non-negative machine-sized integer expected at position 3 in `1`.",
        "intm": "Machine-sized integer expected at position 4 in `1`.",
    }

    def apply_complex(self, n, var, evaluation):
        "%(name)s[n_Complex, var___]"
        return evaluation.message("RealDigits", "realx", n)

    def apply_rational_with_base(self, n, b, evaluation):
        "%(name)s[n_Rational, b_Integer]"
        # expr = Expression("RealDigits", n)
        py_n = abs(n.value)
        py_b = b.get_int_value()
        if check_finite_decimal(n.denominator().get_int_value()) and not py_b % 2:
            return self.apply_with_base(n, b, evaluation)
        else:
            exp = int(mpmath.ceil(mpmath.log(py_n, py_b)))
            (head, tails) = convert_repeating_decimal(
                py_n.as_numer_denom()[0], py_n.as_numer_denom()[1], py_b
            )

            leaves = []
            for x in head:
                if not x == "0":
                    leaves.append(from_python(int(x)))
            leaves.append(from_python(tails))
            list_str = Expression(SymbolList, *leaves)
        return Expression(SymbolList, list_str, exp)

    def apply_rational_without_base(self, n, evaluation):
        "%(name)s[n_Rational]"

        return self.apply_rational_with_base(n, from_python(10), evaluation)

    def apply(self, n, evaluation):
        "%(name)s[n_]"

        # Handling the testcases that throw the error message and return the ouput that doesn't include `base` argument
        if isinstance(n, Symbol) and n.name.startswith("System`"):
            return evaluation.message("RealDigits", "ndig", n)

        if n.is_numeric():
            return self.apply_with_base(n, from_python(10), evaluation)

    def apply_with_base(self, n, b, evaluation, nr_elements=None, pos=None):
        "%(name)s[n_?NumericQ, b_Integer]"

        expr = Expression("RealDigits", n)
        rational_no = (
            True if isinstance(n, Rational) else False
        )  # it is used for checking whether the input n is a rational or not
        py_b = b.get_int_value()
        if isinstance(n, (Expression, Symbol, Rational)):
            pos_len = abs(pos) + 1 if pos is not None and pos < 0 else 1
            if nr_elements is not None:
                n = Expression(
                    "N", n, int(mpmath.log(py_b ** (nr_elements + pos_len), 10)) + 1
                ).evaluate(evaluation)
            else:
                if rational_no:
                    n = Expression(SymbolN, n).evaluate(evaluation)
                else:
                    return evaluation.message("RealDigits", "ndig", expr)
        py_n = abs(n.value)

        if not py_b > 1:
            return evaluation.message("RealDigits", "rbase", py_b)

        if isinstance(py_n, complex):
            return evaluation.message("RealDigits", "realx", expr)

        if isinstance(n, Integer):
            display_len = (
                int(mpmath.floor(mpmath.log(py_n, py_b)))
                if py_n != 0 and py_n != 1
                else 1
            )
        else:
            display_len = int(
                Expression(
                    "N",
                    Expression(
                        "Round",
                        Expression(
                            "Divide",
                            Expression("Precision", py_n),
                            Expression("Log", 10, py_b),
                        ),
                    ),
                )
                .evaluate(evaluation)
                .to_python()
            )

        exp = log_n_b(py_n, py_b)

        if py_n == 0 and nr_elements is not None:
            exp = 0

        digits = []
        if not py_b == 10:
            digits = convert_float_base(py_n, py_b, display_len - exp)
            # truncate all the leading 0's
            i = 0
            while digits and digits[i] == 0:
                i += 1
            digits = digits[i:]

            if not isinstance(n, Integer):
                if len(digits) > display_len:
                    digits = digits[: display_len - 1]
        else:
            # drop any leading zeroes
            for x in str(py_n):
                if x != "." and (digits or x != "0"):
                    digits.append(x)

        if pos is not None:
            temp = exp
            exp = pos + 1
            move = temp - 1 - pos
            if move <= 0:
                digits = [0] * abs(move) + digits
            else:
                digits = digits[abs(move) :]
                display_len = display_len - move

        leaves = []
        for x in digits:
            if x == "e" or x == "E":
                break
            # Convert to Mathics' list format
            leaves.append(from_python(int(x)))

        if not rational_no:
            while len(leaves) < display_len:
                leaves.append(from_python(0))

        if nr_elements is not None:
            # display_len == nr_elements
            if len(leaves) >= nr_elements:
                # Truncate, preserving the digits on the right
                leaves = leaves[:nr_elements]
            else:
                if isinstance(n, Integer):
                    while len(leaves) < nr_elements:
                        leaves.append(from_python(0))
                else:
                    # Adding Indeterminate if the length is greater than the precision
                    while len(leaves) < nr_elements:
                        leaves.append(from_python(Symbol("Indeterminate")))
        list_str = Expression(SymbolList, *leaves)
        return Expression(SymbolList, list_str, exp)

    def apply_with_base_and_length(self, n, b, length, evaluation, pos=None):
        "%(name)s[n_?NumericQ, b_Integer, length_]"
        leaves = []
        if pos is not None:
            leaves.append(from_python(pos))
        expr = Expression("RealDigits", n, b, length, *leaves)
        if not (isinstance(length, Integer) and length.get_int_value() >= 0):
            return evaluation.message("RealDigits", "intnm", expr)

        return self.apply_with_base(
            n, b, evaluation, nr_elements=length.get_int_value(), pos=pos
        )

    def apply_with_base_length_and_precision(self, n, b, length, p, evaluation):
        "%(name)s[n_?NumericQ, b_Integer, length_, p_]"
        if not isinstance(p, Integer):
            return evaluation.message(
                "RealDigits", "intm", Expression("RealDigits", n, b, length, p)
            )

        return self.apply_with_base_and_length(
            n, b, length, evaluation, pos=p.get_int_value()
        )


class _ZLibHash:  # make zlib hashes behave as if they were from hashlib
    def __init__(self, fn):
        self._bytes = b""
        self._fn = fn

    def update(self, bytes):
        self._bytes += bytes

    def hexdigest(self):
        return format(self._fn(self._bytes), "x")


class Hash(Builtin):
    """
    <dl>
    <dt>'Hash[$expr$]'
      <dd>returns an integer hash for the given $expr$.
    <dt>'Hash[$expr$, $type$]'
      <dd>returns an integer hash of the specified $type$ for the given $expr$.</dd>
      <dd>The types supported are "MD5", "Adler32", "CRC32", "SHA", "SHA224", "SHA256", "SHA384", and "SHA512".</dd>
    <dt>'Hash[$expr$, $type$, $format$]'
      <dd>Returns the hash in the specified format.</dd>
    </dl>

    > Hash["The Adventures of Huckleberry Finn"]
    = 213425047836523694663619736686226550816

    > Hash["The Adventures of Huckleberry Finn", "SHA256"]
    = 95092649594590384288057183408609254918934351811669818342876362244564858646638

    > Hash[1/3]
    = 56073172797010645108327809727054836008

    > Hash[{a, b, {c, {d, e, f}}}]
    = 135682164776235407777080772547528225284

    > Hash[SomeHead[3.1415]]
    = 58042316473471877315442015469706095084

    >> Hash[{a, b, c}, "xyzstr"]
     = Hash[{a, b, c}, xyzstr, Integer]
    """

    rules = {
        "Hash[expr_]": 'Hash[expr, "MD5", "Integer"]',
        "Hash[expr_, type_String]": 'Hash[expr, type, "Integer"]',
    }

    attributes = ("Protected", "ReadProtected")

    # FIXME md2
    _supported_hashes = {
        "Adler32": lambda: _ZLibHash(zlib.adler32),
        "CRC32": lambda: _ZLibHash(zlib.crc32),
        "MD5": hashlib.md5,
        "SHA": hashlib.sha1,
        "SHA224": hashlib.sha224,
        "SHA256": hashlib.sha256,
        "SHA384": hashlib.sha384,
        "SHA512": hashlib.sha512,
    }

    @staticmethod
    def compute(user_hash, py_hashtype, py_format):
        hash_func = Hash._supported_hashes.get(py_hashtype)
        if hash_func is None:  # unknown hash function?
            return  # in order to return original Expression
        h = hash_func()
        user_hash(h.update)
        res = h.hexdigest()
        if py_format in ("HexString", "HexStringLittleEndian"):
            return from_python(res)
        res = int(res, 16)
        if py_format == "DecimalString":
            return from_python(str(res))
        elif py_format == "ByteArray":
            return from_python(bytearray(res))
        return from_python(res)

    def apply(self, expr, hashtype, outformat, evaluation):
        "Hash[expr_, hashtype_String, outformat_String]"
        return Hash.compute(
            expr.user_hash, hashtype.get_string_value(), outformat.get_string_value()
        )


class TypeEscalation(Exception):
    def __init__(self, mode):
        self.mode = mode


class Fold(object):
    # allows inherited classes to specify a single algorithm implementation that
    # can be called with machine precision, arbitrary precision or symbolically.

    ComputationFunctions = namedtuple("ComputationFunctions", ("sin", "cos"))

    FLOAT = 0
    MPMATH = 1
    SYMBOLIC = 2

    math = {
        FLOAT: ComputationFunctions(cos=math.cos, sin=math.sin,),
        MPMATH: ComputationFunctions(cos=mpmath.cos, sin=mpmath.sin,),
        SYMBOLIC: ComputationFunctions(
            cos=lambda x: Expression("Cos", x), sin=lambda x: Expression("Sin", x),
        ),
    }

    operands = {
        FLOAT: lambda x: None if x is None else x.round_to_float(),
        MPMATH: lambda x: None if x is None else x.to_mpmath(),
        SYMBOLIC: lambda x: x,
    }

    def _operands(self, state, steps):
        raise NotImplementedError

    def _fold(self, state, steps, math):
        raise NotImplementedError

    def _spans(self, operands):
        spans = {}
        k = 0
        j = 0

        for mode in (self.FLOAT, self.MPMATH):
            for i, operand in enumerate(operands[k:]):
                if operand[0] > mode:
                    break
                j = i + k + 1

            if k == 0 and j == 1:  # only init state? then ignore.
                j = 0

            spans[mode] = slice(k, j)
            k = j

        spans[self.SYMBOLIC] = slice(k, len(operands))

        return spans

    def fold(self, x, ll):
        # computes fold(x, ll) with the internal _fold function. will start
        # its evaluation machine precision, and will escalate to arbitrary
        # precision if or symbolical evaluation only if necessary. folded
        # items already computed are carried over to new evaluation modes.

        yield x  # initial state

        init = None
        operands = list(self._operands(x, ll))
        spans = self._spans(operands)

        for mode in (self.FLOAT, self.MPMATH, self.SYMBOLIC):
            s_operands = [y[1:] for y in operands[spans[mode]]]

            if not s_operands:
                continue

            if mode == self.MPMATH:
                from mathics.core.numbers import min_prec

                precision = min_prec(*[t for t in chain(*s_operands) if t is not None])
                working_precision = mpmath.workprec
            else:

                @contextmanager
                def working_precision(_):
                    yield

                precision = None

            if mode == self.FLOAT:

                def out(z):
                    return Real(z)

            elif mode == self.MPMATH:

                def out(z):
                    return Real(z, precision)

            else:

                def out(z):
                    return z

            as_operand = self.operands.get(mode)

            def converted_operands():
                for y in s_operands:
                    yield tuple(as_operand(t) for t in y)

            with working_precision(precision):
                c_operands = converted_operands()

                if init is not None:
                    c_init = tuple(
                        (None if t is None else as_operand(from_python(t)))
                        for t in init
                    )
                else:
                    c_init = next(c_operands)
                    init = tuple((None if t is None else out(t)) for t in c_init)

                generator = self._fold(c_init, c_operands, self.math.get(mode))

                for y in generator:
                    y = tuple(out(t) for t in y)
                    yield y
                    init = y
