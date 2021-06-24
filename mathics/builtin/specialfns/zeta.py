# -*- coding: utf-8 -*-

"""
Exponential Integral and Special Functions
"""

import mpmath

from mathics.version import __version__  # noqa used in loading to check consistency.

from mathics.builtin.arithmetic import _MPMathFunction
from mathics.core.expression import from_mpmath


class LerchPhi(_MPMathFunction):
    """
    <dl>
    <dt>'LerchPhi[z,s,a]'
        <dd>gives the Lerch transcendent Φ(z,s,a).
    </dl>

    >> LerchPhi[2, 3, -1.5]
     = 19.3893 - 2.1346 I

    >> LerchPhi[1, 2, 1/4]
     = 17.1973
    """

    # attributes = ("Listable", "NumericFunction") # inherited

    mpmath_name = "lerchphi"
    sympy_name = "lerchphi"

    def apply(self, z, s, a, evaluation):
        "%(name)s[z_, s_, a_]"

        py_z = z.to_python()
        py_s = s.to_python()
        py_a = a.to_python()
        try:
            return from_mpmath(mpmath.lerchphi(py_z, py_s, py_a))
        except:
            pass
            # return sympy.expand_func(sympy.lerchphi(py_z, py_s, py_a))


class Zeta(_MPMathFunction):
    """
    <dl>
    <dt>'Zeta[$z$]'
      <dd>returns the Riemann zeta function of $z$.
    </dl>

    >> Zeta[2]
     = Pi ^ 2 / 6

    >> Zeta[-2.5 + I]
     = 0.0235936 + 0.0014078 I
    """

    sympy_name = "zeta"
    mpmath_name = "zeta"
