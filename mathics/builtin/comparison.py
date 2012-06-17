# -*- coding: utf8 -*-

from gmpy import mpz, mpf

from mathics.builtin.base import Builtin, Predefined, BinaryOperator, PrefixOperator, Test
from mathics.core.expression import Expression, Number, Integer, Rational, Real, Symbol, Complex, String
from mathics.core.numbers import real_power, get_type

class SameQ(BinaryOperator):
    """
    >> a===a
     = True
    >> 1===1
     = True
    >> 1===1.
     = False
    """
    
    operator = '==='
    precedence = 290
    
    def apply(self, lhs, rhs, evaluation):
        'lhs_ === rhs_'
        
        if lhs.same(rhs):
            return Symbol('True')
        else:
            return Symbol('False')

class UnsameQ(BinaryOperator):
    """
    >> a=!=a
     = False
    >> 1=!=1.
     = True
    """
    
    operator = '=!='
    precedence = 290
    
    def apply(self, lhs, rhs, evaluation):
        'lhs_ =!= rhs_'
        
        if lhs.same(rhs):
            return Symbol('False')
        else:
            return Symbol('True')
        
operators = {
    'Less': (-1,),
    'LessEqual': (-1, 0),
    'Equal': (0,),
    'GreaterEqual': (0, 1),
    'Greater': (1,),
    'Unequal': (-1, 1),
}
        
class _InequalityOperator(BinaryOperator):
    precedence = 290
    grouping = 'NonAssociative'
    
    def parse(self, args):
        names = operators.keys()
        
        def inequality_leaves(expression):
            if expression.parenthesized:
                return [expression]
            name = expression.get_head_name()
            leaves = expression.get_leaves()
            if name == 'Inequality':
                return leaves
            elif name in names:
                result = []
                for leaf in leaves[:-1]:
                    result.extend([leaf, Symbol(name)])
                result.extend(leaves[-1:])
                return result
            else:
                return [expression]
               
        left = args[0]
        right = args[2]
        name = self.get_name()
        
        left_leaves = inequality_leaves(left)
        right_leaves = inequality_leaves(right)
        leaves = left_leaves + [Symbol(name)] + right_leaves
        ops = set(leaves[1::2])
        if len(ops) == 1:
            return Expression(ops.pop(), *leaves[0::2])
        else:
            return Expression('Inequality', *leaves)
            
    def apply(self, items, evaluation):
        '%(name)s[items__?RealNumberQ]'
        
        prev = None
        items = items.get_sequence()
        wanted = operators[self.get_name()]
        for item in items:
            if prev is not None and do_cmp(prev, item) not in wanted:
                return Symbol('False')
            prev = item
        return Symbol('True')
            
class Inequality(Builtin):
    """
    'Inequality' is the head of expressions involving different inequality operators (at least temporarily).
    Thus, it is possible to write chains of inequalities.
    >> a<b<=c
     = a < b && b <= c
    >> Inequality[a, Greater, b, LessEqual, c]
     = a > b && b <= c
    >> 1<2<=3
     = True
    >> 1<2>0
     = True
    >> 1<2<-1
     = False
    """
    
    messages = {
        'ineq': "Inequality called with `` arguments; the number of arguments is expected to be an odd number >= 3.",
    }
    
    def apply(self, items, evaluation):
        'Inequality[items___]'
        
        items = items.get_sequence()
        count = len(items)
        if count == 1:
            return Symbol('True')
        elif count % 2 == 0:
            evaluation.message('Inequality', 'ineq', count)
        elif count == 3:
            name = items[1].get_name()
            if name in operators.keys():
                return Expression(name, items[0], items[2]) 
        else:
            groups = [Expression('Inequality', *items[index-1:index+2]) for index in range(1, count-1, 2)]
            return Expression('And', *groups)
            
def do_cmp(x1, x2):
    real1, real2 = x1.get_real_value(), x2.get_real_value()
    inf1 = inf2 = None
    if x1.has_form('DirectedInfinity', 1): inf1 = x1.leaves[0].get_int_value()
    if x2.has_form('DirectedInfinity', 1): inf2 = x2.leaves[0].get_int_value()
    
    if real1 is not None and get_type(real1) != 'f': real1 = mpf(real1)
    if real2 is not None and get_type(real2) != 'f': real2 = mpf(real2)    
    # Bus error when not converting to mpf
    
    if real1 is not None and real2 is not None:
        return cmp(x1, x2)
    elif inf1 is not None and inf2 is not None:
        return cmp(inf1, inf2)
    elif inf1 is not None and real2 is not None:
        return inf1
    elif real1 is not None and inf2 is not None:
        return -inf2
    else:
        return None
            
def do_compare(l1, l2):
    if l1.same(l2):
        return True
    elif isinstance(l1, (Number, String)) and isinstance(l2, (Number, String)):
        return False
    elif l1.has_form('List', None) and l2.has_form('List', None):
        if len(l1.leaves) != len(l2.leaves):
            return False
        for item1, item2 in zip(l1.leaves, l2.leaves):
            result = do_compare(item1, item2)
            if not result:
                return result
        return True
    else:
        return None
    
class Equal(_InequalityOperator):
    """
    >> a==a
     = True
    >> a==b
     = a == b
    >> 1==1.
     = True
     
    Lists are compared based on their elements:
    >> {{1}, {2}} == {{1}, {2}}
     = True
    >> {1, 2} == {1, 2, 3}
     = False
     
    Real values are considered equal if they only differ in their last digits:
    >> 0.739085133215160642 == 0.739085133215160641
     = True
    >> 0.73908513321516064200000000 == 0.73908513321516064100000000
     = False
     
    #> {1, 2, 3} < {1, 2, 3}
     = {1, 2, 3} < {1, 2, 3}
    """
    operator = '=='
    grouping = 'None'
    
    def apply_other(self, x, y, evaluation):
        'Equal[x_?(!RealNumberQ[#]&), y_?(!RealNumberQ[#]&)]'
            
        result = do_compare(x, y)
        if result is not None:
            if result:
                return Symbol('True')
            else:
                return Symbol('False')
        
class Unequal(_InequalityOperator):
    """
    >> 1 != 1.
     = False
     
    Lists are compared based on their elements:
    >> {1} != {2}
     = True
    >> {1, 2} != {1, 2}
     = False
    >> {a} != {a}
     = False
    >> "a" != "b"
     = True
    >> "a" != "a"
     = False
    """
    
    operator = '!='
    
    def apply_other(self, x, y, evaluation):
        'Unequal[x_?(!RealNumberQ[#]&), y_?(!RealNumberQ[#]&)]'
        
        result = do_compare(x, y)
        if result is not None:
            if result:
                return Symbol('False')
            else:
                return Symbol('True')
        
        if x.is_same(y):
            return Symbol('False')
        
class Less(_InequalityOperator):
    operator = '<'
        
class LessEqual(_InequalityOperator):
    operator = '<='

class Greater(_InequalityOperator):
    """
    >> a > b > c //FullForm
     = Greater[a, b, c]
    >> Greater[3, 2, 1]
     = True
    """
    
    operator = '>'

class GreaterEqual(_InequalityOperator):
    operator = '>='
    
class Positive(Test):
    def test(self, expr):
        return isinstance(expr, (Integer, Rational, Real)) and expr.value > 0
        
class Negative(Test):
    """
    >> Negative[-3]
     = True
    >> Negative[10/7]
     = False
    >> Negative[1+2I]
     = False
    >> Negative[a+b]
     = False
    """
    
    def test(self, expr):
        return isinstance(expr, (Integer, Rational, Real)) and expr.value < 0
    
class NonNegative(Test):    
    def test(self, expr):
        return isinstance(expr, (Integer, Rational, Real)) and expr.value >= 0
    
class NonPositive(Test):    
    def test(self, expr):
        return isinstance(expr, (Integer, Rational, Real)) and expr.value <= 0
    
def expr_max(items):
    result = Expression('DirectedInfinity', -1)
    for item in items:
        c = do_cmp(item, result)
        if c > 0:
            result = item
    return result
    
def expr_min(items):
    result = Expression('DirectedInfinity', 1)
    for item in items:
        c = do_cmp(item, result)
        if c < 0:
            result = item
    return result
    
class Max(Builtin):
    """
    >> Max[{1,2},3,{-3,3.5,-Infinity},{{1/2}}]
     = 3.5
    >> Max[]
     = -Infinity
    """
    
    attributes = ('Flat', 'NumericFunction', 'OneIdentity', 'Orderless')
    
    def apply(self, items, evaluation):
        'Max[items___]'
        
        items = items.flatten(Symbol('List')).get_sequence()
        result = Expression('DirectedInfinity', -1)
        for item in items:
            if item.has_form('List', None):
                leaves = item.leaves
            else:
                leaves = [item]
            for leaf in leaves:
                c = do_cmp(leaf, result)
                if c > 0:
                    result = leaf
        return result
    
class Min(Builtin):
    """
    >> Min[]
     = Infinity
    """
    
    attributes = ('Flat', 'NumericFunction', 'OneIdentity', 'Orderless')
    
    def apply(self, items, evaluation):
        'Min[items___]'
        
        items = items.flatten(Symbol('List')).get_sequence()
        result = Expression('DirectedInfinity', 1)
        for item in items:
            if item.has_form('List', None):
                leaves = item.leaves
            else:
                leaves = [item]
            for leaf in leaves:
                c = do_cmp(leaf, result)
                if c < 0:
                    result = leaf
        return result
            
