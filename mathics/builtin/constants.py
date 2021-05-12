# cython: language_level=3

"""
Mathematical Constants

Numeric, Arithmetic, or Symbolic constants like Pi, E, or Infinity.
"""

import math
import mpmath
import numpy
import sympy

from mathics.version import __version__  # noqa used in loading to check consistency.

from mathics.builtin.base import Predefined, SympyObject
from mathics.core.expression import (
    MachineReal,
    PrecisionReal,
    Symbol,
    strip_context,
)
from mathics.core.numbers import get_precision, PrecisionValueError, machine_precision


def mp_constant(fn: str, d=None) -> mpmath.mpf:
    """
    Return the mpmath constant _fn_ with integer precision _d_.
    """
    if d is None:
        return getattr(mpmath, fn)()
    else:
        # TODO: In some functions like Pi, you can
        # ask for a certain number of digits, but the
        # accuracy will be less than that. Figure out
        # what's up and compensate somehow.
        mpmath.mp.dps = int_d = int(d * 3.321928)
        return getattr(mpmath, fn)(prec=int_d)


def mp_convert_constant(obj, **kwargs):
    if isinstance(obj, mpmath.ctx_mp_python._constant):
        prec = kwargs.get("prec", None)
        if prec is not None:
            return sympy.Float(obj(prec=prec))
        return sympy.Float(obj)
    return obj


def numpy_constant(name: str, d=None) -> float:
    if d:
        # by mmatera: Here I have a question:
        # 0.0123`2 should be rounded to
        # 0.01 or to 0.0123?
        # (absolute versus relative accuracy)
        val = getattr(numpy, name)
        val = numpy.round(val, d)
        return val
    else:
        return getattr(numpy, name)


def sympy_constant(fn, d=None):
    return getattr(sympy, fn).evalf(n=d)


class _Constant_Common(Predefined):

    attributes = ("Constant", "Protected", "ReadProtected")
    nargs = 0
    options = {"Method": "Automatic"}

    def apply_N(self, precision, evaluation, options={}):
        "N[%(name)s, precision_?NumericQ, OptionsPattern[%(name)s]]"

        preference = self.get_option(options, "Method", evaluation).get_string_value()
        if preference == "Automatic":
            return self.get_constant(precision, evaluation)
        else:
            return self.get_constant(precision, evaluation, preference)

    def apply_N2(self, evaluation, options={}):
        "N[%(name)s, OptionsPattern[%(name)s]]"
        return self.apply_N(None, evaluation, options)

    def is_constant(self) -> bool:
        return True

    def get_constant(self, precision, evaluation, preference=None):
        # first, determine the precision
        machine_d = int(0.30103 * machine_precision)
        d = None
        if precision:
            try:
                d = get_precision(precision, evaluation)
            except PrecisionValueError:
                pass

        if d is None:
            d = machine_d

        # If preference not especified, determine it
        # from the precision.
        if preference is None:
            if d <= machine_d:
                preference = "numpy"
            else:
                preference = "mpmath"
        # If preference is not valid, send a message and return.
        if not (preference in ("sympy", "numpy", "mpmath")):
            evaluation.message(f'{preference} not in ("sympy", "numpy", "mpmath")')
            return
        # Try to determine the numeric value
        value = None
        if preference == "mpmath" and not hasattr(self, "mpmath_name"):
            preference = "numpy"
        elif preference == "sympy" and not hasattr(self, "sympy_name"):
            preference = "numpy"

        if preference == "numpy" and not hasattr(self, "numpy_name"):
            if hasattr(self, "sympy_name"):
                preference = "sympy"
            elif hasattr(self, "mpmath_name"):
                preference = "mpmath"
            else:
                preference = ""
        if preference == "numpy":
            value = numpy_constant(self.numpy_name)
            if d == machine_d:
                return MachineReal(value)
        if preference == "sympy":
            value = sympy_constant(self.sympy_name, d + 2)
        if preference == "mpmath":
            value = mp_constant(self.mpmath_name, d * 2)
        if value:
            return PrecisionReal(sympy.Float(str(value), d))
        # If the value is not available, return none
        # and keep it unevaluated.
        return


class MPMathConstant(_Constant_Common):
    """Representation of a constant in mpmath, e.g. Pi, E, I, etc."""

    # Subclasses should define this.
    mpmath_name = None

    mathics_to_mpmath = {}

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        if self.mpmath_name is None:
            self.mpmath_name = strip_context(self.get_name()).lower()
        self.mathics_to_mpmath[self.__class__.__name__] = self.mpmath_name

    def to_mpmath(self, args):
        if self.mpmath_name is None or len(args) != 0:
            return None
        return getattr(mpmath, self.mpmath_name)


class NumpyConstant(_Constant_Common):
    """Representation of a constant in numpy, e.g. Pi, E, etc."""

    # Subclasses should define this.
    numpy_name = None

    mathics_to_numpy = {}

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        if self.numpy_name is None:
            self.numpy_name = strip_context(self.get_name()).lower()
        self.mathics_to_numpy[self.__class__.__name__] = self.numpy_name

    def to_numpy(self, args):
        if self.numpy_name is None or len(args) != 0:
            return None
        return self.get_constant()


class SympyConstant(_Constant_Common, SympyObject):
    """Representation of a constant in Sympy, e.g. Pi, E, I, Catalan, etc."""

    # Subclasses should define this.
    sympy_name = None

    def to_sympy(self, expr=None, **kwargs):
        if expr is None or expr.is_atom():
            result = getattr(sympy, self.sympy_name)
            if kwargs.get("evaluate", False):
                result = mp_convert_constant(result, **kwargs)
            return result
        else:
            # there is no "native" SymPy expression for e.g. E[x]
            return None


class Catalan(MPMathConstant, SympyConstant):
    """
    <dl>
    <dt>'Catalan'
        <dd>is Catalan's constant with numerical value \u2243 0.915966.
    </dl>

    >> Catalan // N
     = 0.915965594177219

    >> N[Catalan, 20]
     = 0.91596559417721901505
    """

    mpmath_name = "catalan"
    # numpy_name = "catalan"  ## This is not defined in numpy
    sympy_name = "Catalan"


class ComplexInfinity(SympyConstant):
    """
    <dl>
    <dt>'ComplexInfinity'
        <dd>represents an infinite complex quantity of undetermined direction.
    </dl>

    >> 1 / ComplexInfinity
     = 0
    >> ComplexInfinity * Infinity
     = ComplexInfinity
    >> FullForm[ComplexInfinity]
     = DirectedInfinity[]

    ## Issue689
    #> ComplexInfinity + ComplexInfinity
     : Indeterminate expression ComplexInfinity + ComplexInfinity encountered.
     = Indeterminate
    #> ComplexInfinity + Infinity
     : Indeterminate expression ComplexInfinity + Infinity encountered.
     = Indeterminate
    """

    sympy_name = "zoo"

    rules = {
        "ComplexInfinity": "DirectedInfinity[]",
    }


class Degree(MPMathConstant, NumpyConstant, SympyConstant):
    """
    <dl>
      <dt>'Degree'
      <dd>is the number of radians in one degree. It has a numerical value of \u03c0 / 180.
    </dl>
    >> Cos[60 Degree]
     = 1 / 2

    Degree has the value of Pi / 180
    >> Degree == Pi / 180
     = True

    #> Cos[Degree[x]]
     = Cos[Degree[x]]

    ## Issue 274
    #> \\[Degree] == ° == Degree
     = True

    #> N[Degree]
     = 0.0174533
    #> N[Degree, 30]
     = 0.0174532925199432957692369076849
    """

    mpmath_name = "degree"

    def to_sympy(self, expr=None, **kwargs):
        if expr == Symbol("System`Degree"):
            # return mpmath.degree
            return sympy.pi / 180

    def to_numpy(self, expr=None, **kwargs):
        if expr == Symbol("System`Degree"):
            # return mpmath.degree
            return numpy.pi / 180

    def apply_N(self, precision, evaluation, options={}):
        "N[Degree, precision_, OptionsPattern[%(name)s]]"
        try:
            if precision:
                d = get_precision(precision, evaluation)
            else:
                d = get_precision(Symbol("System`MachinePrecision"), evaluation)
        except PrecisionValueError:
            return

        # FIXME: There are all sorts of interactions between in the trig functions,
        # that are expected to work out right. Until we have convertion between
        # mpmath and sympy worked out so that values can be made the to the same
        # precision and compared. we have to not use mpmath right now.
        # return self.get_constant(precision, evaluation, preference="mpmath")

        if d is None:
            return MachineReal(math.pi / 180)
        else:
            return PrecisionReal((sympy.pi / 180).n(d))


class E(MPMathConstant, NumpyConstant, SympyConstant):
    """
        <dl>
        <dt>'E'</dt>
            <dd>is the constant \u2147 with numerical value \u2243 2.71828.
        </dl>

        >> N[E]
         = 2.71828
        >> N[E, 50]
         = 2.7182818284590452353602874713526624977572470937000

        #> 5. E
         = 13.5914
    """

    mpmath_name = "e"
    numpy_name = "e"
    sympy_name = "E"

    def apply_N(self, precision, evaluation, options={}):
        "N[E, precision_, OptionsPattern[%(name)s]]"
        return self.get_constant(precision, evaluation)


class EulerGamma(MPMathConstant, NumpyConstant, SympyConstant):
    """
    <dl>
      <dt>'EulerGamma'</dt>
      <dd>is Euler's constant \u03b3 with numerial value \u2243 0.577216.
    </dl>

    >> EulerGamma // N
     = 0.577216

    >> N[EulerGamma, 40]
     = 0.5772156649015328606065120900824024310422
    """

    mpmath_name = "euler"
    numpy_name = "euler_gamma"
    sympy_name = "EulerGamma"


class Glaisher(MPMathConstant):
    """
    <dl>
      <dt>'Glaisher'</dt>
      <dd>is Glaisher's constant, with numerical value \u2243 1.28243.
    </dl>

    >> N[Glaisher]
     = 1.28242712910062
    >> N[Glaisher, 50]
     = 1.2824271291006226368753425688697917277676889273250
     # 1.2824271291006219541941391071304678916931152343750
    """

    mpmath_name = "glaisher"


class GoldenRatio(MPMathConstant, SympyConstant):
    """
    <dl>
      <dt>'GoldenRatio'
      <dd>is the golden ratio, \u03D5 = (1+Sqrt[5])/2.
    </dl>

    >> GoldenRatio // N
     = 1.61803398874989
    >> N[GoldenRatio, 40]
     = 1.618033988749894848204586834365638117720
    """

    sympy_name = "GoldenRatio"
    mpmath_name = "phi"


class Indeterminate(SympyConstant):
    """
    <dl>
    <dt>'Indeterminate'</dt>
        <dd>represents an indeterminate result.
    </dl>

    >> 0^0
     : Indeterminate expression 0 ^ 0 encountered.
     = Indeterminate

    >> Tan[Indeterminate]
     = Indeterminate
    """

    sympy_name = "nan"


class Infinity(SympyConstant):
    """
    <dl>
    <dt>'Infinity'
        <dd>represents an infinite real quantity.
    </dl>

    >> 1 / Infinity
     = 0
    >> Infinity + 100
     = Infinity

    Use 'Infinity' in sum and limit calculations:
    >> Sum[1/x^2, {x, 1, Infinity}]
     = Pi ^ 2 / 6

    #> FullForm[Infinity]
     = DirectedInfinity[1]
    #> (2 + 3.5*I) / Infinity
     = 0.
    #> Infinity + Infinity
     = Infinity
    #> Infinity / Infinity
     : Indeterminate expression 0 Infinity encountered.
     = Indeterminate
    """

    sympy_name = "oo"
    numpy_name = "Inf"
    mpmath_name = "inf"
    python_equivalent = math.inf

    rules = {
        "Infinity": "DirectedInfinity[1]",
        "MakeBoxes[Infinity, f:StandardForm|TraditionalForm]": ('"\\[Infinity]"'),
    }


class Khinchin(MPMathConstant):
    """
    <dl>
      <dt>'Khinchin'</dt>
      <dd>is Khinchin's constant, with numerical value \u2243 2.68545.
    </dl>

    >> N[Khinchin]
     = 2.68545200106531
    >> N[Khinchin, 50]
     = 2.6854520010653064453097148354817956938203822939945
     # = 2.6854520010653075701156922150403261184692382812500
    """

    mpmath_name = "khinchin"


class Pi(MPMathConstant, SympyConstant):
    """
    <dl>
      <dt>'Pi'</dt>
      <dd>is the constant \u03c0.
    </dl>

    >> N[Pi]
     = 3.14159

    Force using the value given from numpy to compute Pi.
    >> N[Pi, Method->"numpy"]
     = 3.14159

    Force using the value given from sympy to compute Pi to 3 places,
    two places after the decimal point.

    Note that sympy is the default method.
    >> N[Pi, 3, Method->"sympy"]
     = 3.14

     >> N[Pi, 50]
     = 3.1415926535897932384626433832795028841971693993751
    >> Attributes[Pi]
     = {Constant, Protected, ReadProtected}
    """

    sympy_name = "pi"
    mpmath_name = "pi"
    numpy_name = "pi"
