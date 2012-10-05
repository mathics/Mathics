# -*- coding: utf8 -*-

u"""
    Mathics: a general-purpose computer algebra system
    Copyright (C) 2011 Jan Pöschko

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
"""

from __future__ import with_statement

import sympy
import mpmath
from math import log

from mathics.core.util import unicode_superscript

def get_type(value):
    if isinstance(value, sympy.Integer):
        return 'z'
    elif isinstance(value, sympy.Rational):
        return 'q'
    elif isinstance(value, sympy.Float) or isinstance(value, mpmath.mpf):
        return 'f'
    elif isinstance(value, mpcomplex):
        return 'c'
    else:
        return None
    
def same(v1, v2):
    return get_type(v1) == get_type(v2) and v1 == v2

def is_0(value):
    return get_type(value) == 'z' and value == 0

def sympy2mpmath(value):
    if isinstance(value, mpcomplex):
        return value.to_mpmath()
    else:
        if get_type(value) != 'f':
            value = sympy.Float(value)
        #with mpmath.workprec(value.getprec()): #TODO
        value = str(sympy.Float(value))
        if value and value[0] == '-':
            return -mpmath.mpf(value[1:])
        else:
            return mpmath.mpf(value)
            
class SpecialValueError(Exception):
    def __init__(self, name):
        self.name = name

def mpmath2sympy(value):
    if isinstance(value, mpmath.mpc):
        return mpcomplex(value)
    else:
        value = str(value)
        if value in ('+inf', '-inf'):
            raise SpecialValueError('ComplexInfinity')
        return sympy.Float(value)
    
C = log(10, 2) # ~ 3.3219280948873626
    
def dps(prec):
    return max(1, int(round(int(prec) / C - 1)))

def prec(dps):
    return max(1, int(round((int(dps) + 1) * C)))

def format_float(value, pretty=True, parenthesize_plus=False):
    s = str(value)
    s = s.split('e')
    if len(s) == 2:
        man, exp = s
        if pretty:
            return u'%s\u00d710%s' % (format_float(man), unicode_superscript(exp))
        else:
            result = u'%s*10^%s' % (format_float(man), exp)
            if parenthesize_plus:
                result = '(%s)' % result
            return result
    else:
        return s[0]
    
def mul(x, y):
    return x * y
    
def add(x, y):
    return x + y
        
def min_prec(*args):
    result = None
    #TODO
    #for arg in args:
    #    prec = arg.get_precision()
    #    if result is None or (prec is not None and prec < result):
    #        result = prec
    return result

def create_complex(real, imag):
    if is_0(imag):
        return real
    else:
        return mpcomplex(real, imag)
    
def pickle_mp(value):
    return (get_type(value), str(value))

def unpickle_mp(value):
    type, value = value
    if type == 'z':
        return sympy.Integer(value)
    elif type == 'q':
        return sympy.Rational(value)
    elif type == 'f':
        return sympy.Float(value)
    else:
        return value

class mpcomplex(object):
    def __init__(self, real, imag=None):
        if isinstance(real, mpcomplex):
            self.real, self.imag = real.real, real.imag
        elif isinstance(real, mpmath.mpc):
            self.real = real.real
            self.imag = real.imag
        else:
            self.real = real
            if imag is None: imag = sympy.Integer(0)
            self.imag = imag
        
    def __getstate__(self):
        return {'real': pickle_mp(self.real), 'imag': pickle_mp(self.imag)}
    
    def __setstate__(self, dict):
        self.real = unpickle_mp(dict['real'])
        self.imag = unpickle_mp(dict['imag'])
        
    def __str__(self):
        return '%s+%s*I' % (self.real, self.imag)

    def __add__(self, other):
        if isinstance(other, mpcomplex):
            return create_complex(self.real + other.real, self.imag + other.imag)
        else:
            return create_complex(self.real + other, self.imag)

    def __radd__(self, other):
        return self.__add__(other)
    
    def __mul__(self, other):
        if isinstance(other, mpcomplex):
            real = self.real * other.real - self.imag * other.imag
            imag = self.real * other.imag + self.imag * other.real
            return create_complex(real, imag)
        else:
            if is_0(other):
                return sympy.Integer(0)
            else:
                if is_0(self.real):
                    real = sympy.Integer(0)
                else:
                    real = self.real * other
                if is_0(self.imag):
                    imag = sympy.Integer(0)
                else:
                    imag = self.imag * other
                return create_complex(real, imag)
            
    def __rmul__(self, other):
        return self.__mul__(other)
    
    def __rdiv__(self, other):
        r = self.abs()
        if r == 0:
            raise ZeroDivisionError
        return other * create_complex(self.real / r, - self.imag / r)
    
    def __eq__(self, other):
        return isinstance(other, mpcomplex) and other.real == self.real and other.imag == self.imag
    
    def to_mpmath(self):
        return mpmath.mpc(self.real, self.imag)
    
    def __pow__(self, other):
        if get_type(other) == 'z':
            # TODO: make this faster!
            if other == 0:
                return sympy.Integer(1)
            else:
                result = sympy.Integer(1)
                for i in range(abs(other)):
                    result = result * self
                if other < 0:
                    result = sympy.Integer(1) / result
                return result
        else:
            other = mpcomplex(other)
            sc = self.to_mpmath()
            oc = other.to_mpmath()
            result = sc ** oc
            return create_complex(result.real, result.imag)
            
    def __rpow__(self, other):
        return mpcomplex.__pow__(mpcomplex(other), self)
    
    def __neg__(self):
        return mpcomplex(-self.real, -self.imag)
    
    def abs(self):
        return self.real * self.real + self.imag * self.imag
                    
