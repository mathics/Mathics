# -*- coding: utf-8 -*-

"""
Calculus
"""
from mathics.version import __version__  # noqa used in loading to check consistency.

from mathics.builtin.base import Builtin, PostfixOperator, SympyFunction
from mathics.core.expression import (
    Expression,
    Symbol,
    String,
    Integer,
    Integer0,
    Integer1,
    Number,
    Rational,
    Real,
    SymbolTrue,
    SymbolFalse,
    SymbolList,
    SymbolN,
    SymbolRule,
    SymbolUndefined,
    from_python,
)
from mathics.core.convert import sympy_symbol_prefix, SympyExpression, from_sympy
from mathics.core.rules import Pattern
from mathics.core.numbers import dps
from mathics.builtin.scoping import dynamic_scoping
from mathics import Symbol

import sympy


    

_invphi = (math.sqrt(5) - 1) / 2  # 1 / phi
_invphi2 = (3 - math.sqrt(5)) / 2  # 1 / phi^2

class FindMinimumMaximum_(Builtin):
    """
    Basis clase for FindMinimum and FindMaximum
    """
    attributes = ("HoldAll",)
    options = {
        "AccuracyGoal": "Automatic",
        "Compiled":  "Automatic", 
        "EvaluationMonitor" : "None",
        "Gradient" : "Automatic", 
        "MaxIterations" : "Automatic",
        "Method" : "Automatic", 
        "PrecisionGoal" : "Automatic",
        "StepMonitor" : "None",
        "WorkingPrecision" : "MachinePrecision",
    }

    methods = {"Automatic": _minimize_internal,
               "Internal": _minimize_internal}

    messages = {
        "cvmit" : "Failed to converge to the requested accuracy or precision within `1` iterations.",
        "nrnum" : "The function value `1` is not a real number at {`2`} = {`3`}.",
        "bdmtd" : "Value of option Method -> `1` is not Automatic or in `2`",
        "srect":  "Value `1` in search specification `2` is not a number or array of numbers.",
    }
    
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.methods["Internal"] = (_internal_optimizer, False)
        try:
            from scipy.optimize import minimize


            self.methods["ConjugateGradient"] = (
                _scipy_interface(
                    minimize, {}, {"method": "CG",}, lambda res: (res.f, res.x, res.success)
                ),
                True,
            )
            self.methods["Newton"] = (
                _scipy_interface(
                    minimize, {}, {"method": "Newton-CG",}, lambda res: (res.f, res.x, res.success)
                ),
                True,
            )
            
            self.methods["NelderMead"] = (
                _scipy_interface(
                    minimize, {}, {"method": "Nealder-Mead",}, lambda res: (res.f, res.x, res.success)
                ),
                True,
            )
            self.methods["Automatic"] = self.methods["Nelder-Mead"]
        except Exception:
            self.methods["Automatic"] = self.methods["Internal"]

        self.messages["bdmtd"] = (
            "The Method option should be a "
            + "built-in method name in {`"
            + "`, `".join(list(self.methods))
            + "`}. Using `Automatic`"
        )

    def get_optimizer(self, options, evaluation):
        method = options["System`Method"]
        if isinstance(method, Symbol):
            method = method.evaluate(evaluation)
        if isinstance(method, Symbol):
            method = method.get_name()
        elif isinstance(method, String):
            method = method.value
        gradient = None
        # If the method requires the gradient, then look for the option.
        # if not available, then try to calculate it.
        if method in ("Automatic", "MathicsBuiltin"):
            gradient = options["System`Gradient"]
            if gradient == Symbol("Automatic") and method in ("System`MathicsBuiltin"):
                gradient = Expression("List", *[Expression("D", f, svar[0])   for svar in svars])
                gradient = gradient.evaluate(evaluation)
        return self.methods.get(method, None), gradient

    def get_domain(self, x, evaluation):
        if isinstance(x, symbol):
            x = x.evaluate(evaluation)

        def process_item(u):
            y = u._leaves[0]
            if not isinstance(y, Symbol):
                return None
            if len(u._leaves) == 1:
                return [(y, 1., -np.inf, np.inf)]
            if len(u._leaves) == 2:
                return [(y, u._leaves[1].to_python(), -np.inf, np.inf)]
            if len(u._leaves) == 3:
                a, b = x._leaves[1].to_python(), x._leaves[2].to_python()
                return (x, .5*(a+b), a, b)

        item = process_item(x)
        if item is not None:
            return [item]
        if x.has_form("List", None):
            items = [process_item(x)  for leaf in x._leaves]
            for i, item in enumerate(items):
                if item is None:
                    evaluation.message(self.get_name(), "srect", x._leaves[i], x)
                    return None
            return items
        return None

    def apply_findminmax(self, f, x, evaluation, options):
        """%(name)s[f_, x_, OptionsPattern[]]"""
        name = self.get_name()
        prefactor = Integer(self._sign)
        optimizer = get_optimizer(self, options, evaluation)
        if not optimizer:
            evaluation.message(name, "bdmtd", method, ", ".join(self.methods.keys()))
            return
        domain = get_domain(x, evaluation)
        if domain is None:
            return
        # Compile the function
        
        fcmp = Expression("Compile",
                          Expression(SymbolList, *[var[0] for var in domain]),
                          Expression("Times", prefactor, f)
        ).evaluate(evaluation)
        if fcmp.has_form("CompiledFunction", 3):
            print("I can not compile this function")
            return
        result = optimizer(fcmp._leaves[2].cfunc, [var[1] for var in domain])
        if result:
            return Expression(SymbolList, from_python(result[0]),
                              Expression("SymbolList", *[ Expression("Rule", var[0], val)  for var, val in zip(var, result[1])]))
        return
            



    
class FindMinimum(_FindMinimumMaximum):
    """
    <dl>
    <dt>'FindMinimum[$f$, $x$]'
        <dd>searches for a local minimum in $f$, starting from an
     automatically selected point.
    <dt>'FindMinimum[$f$, {$x$, $x_0$}]'
        <dd>searches for a local minimum in $f$, starting from the
point  $x = x_0$.
    </dl>
    >> FindMaximum[Sin[x^2], x]
     = {x ->Sqrt[Pi / 2], -1}
    """
    self._sign = 1


class FindMaximum(_FindMinimumMaximum):
    """
    <dl>
    <dt>'FindMaximum[$f$, $x$]'
        <dd>searches for a local minimum in $f$, starting from an
     automatically selected point.
    <dt>'FindMaximum[$f$, {$x$, $x_0$}]'
        <dd>searches for a local minimum in $f$, starting from the
point  $x = x_0$.
    </dl>

    >> FindMaximum[Sin[x^2], x]
     = {x ->Sqrt[Pi / 2], -1}
    """
    self._sign = -1 







def _scipy_interface(optimizer, options_map, mandatory=None, adapt_func=None):
    """
    This function provides a proxy for scipy.optimize
    functions, adapting the parameters.
    """
    def _scipy_proxy_func(fun, x0, adapt=None, **opts):
        native_opts = {}
        if mandatory:
            native_opts.update(mandatory)
        for opt, val in opts.items():
            native_opt = options_map.get(opt, None)
            if native_opt:
                if native_opt[1]:
                    val = native_opt[1](val)
                opt_name = native_opt[0]
                if opt_name in ("args", "method", "jac", "hess", "hessp", "bounds",
                                "constraints", "tol", "callback"):
                    native_opts[opt_name] = val
                else:
                    options_parm = native_opts.get(options, None)
                    if options_parm is None:
                        native_opts["options"] = {opt_name:val}
        if adapt_func:
            return adapt_func(optimizer(fun, x0, **native_opts))
        else:
            return optimizer(fun, x0, **native_opts)

    return _scipy_proxy_func


def _internal_optimizer(f, a, **opts):
    rem_it = opts["options"]["max_it"]
    tol = opts["tol"]
    x0 = a
    f0 = f(*a)
    
    def lin_optimizer(v):
        "This implements a golden section search"
        xl0 = x0
        fl0 = f0
        zl = -1.
        xll = [u + zl * w for u, w in zip(x0, v)]
        fll = f(*xll)
        if fll < fl0:
            flr, xlr = fl0, x0
            while fll < fl0:
                if rem_it==0:
                    return fll, xll
                rem_it -= 1
                fl0, xl0, zl = fll, xll, 1.5 * zl
                fll = [u + z * w for u, w in zip(x0, v)]
                fll = f(*xll)
        else:
            zr = 1
            xlr = [u + z * w for u, w in zip(x0, v)]
            flr = f(*xlr)
            while flr < fl0:
                if rem_it==0:
                    return flr, xlr
                rem_it -= 1
                fl0, xl0, zr = flr, xlr, 1.5 * zr
                flr = [u + zr * w for u, w in zip(x0, v)]
                flr = f(*xlr)
        # at this point, fll, flr>fl0.
        # Now, implement a golden section search
        h = max([abs(l-r)  for l, r in zip(xll, xlr)])
        while h < tol:
            if rem_it==0:
                return fl0, xl0
            rem_it -= 1 
            xnew = [ua + _invphi *(ub-ua) for ua, ub in zip(xll, xl0)]
            fnew = f(*xd)
            if fnew < f0:
                flr, xlr = flx, xl0
                fl0, xl0 = fnew, xnew
                h = max([abs(l-r)  for l, r in zip(xll, xlr)])
            else:
                fll, xll = fnew, xnew
        

            
            fc = f(*xc)
            fd = f(*xd)
            if fc < fd:
                xlr = xd
                xd = xc
                fd = fc
                
            else:
            
        
        

    
    return 2

