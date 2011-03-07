# -*- coding: utf8 -*-

"""
Tensor functions
"""

from mathics.builtin.base import Builtin, BinaryOperator, Test
from mathics.core.expression import Expression, Symbol, Integer
from mathics.core.rules import Pattern

from mathics.builtin.lists import get_part

class ArrayQ(Builtin):
    """
    <dl>
    <dt>'ArrayQ[$expr$]'
        <dd>tests whether $expr$ is a full array.
    <dt>'ArrayQ[$expr$, $pattern$]'
        <dd>also tests whether the array depth of $expr$ matches $pattern$.
    <dt>'ArrayQ[$expr$, $pattern$, $test$]'</dt>
        <dd>furthermore tests whether $test$ yields 'True' for all elements of $expr$.
        'ArrayQ[$expr$]' is equivalent to 'ArrayQ[$expr$, _, True&]'.
    </dl>
    >> ArrayQ[a]
     = False
    >> ArrayQ[{a}]
     = True
    >> ArrayQ[{{{a}},{{b,c}}}]
     = False
    >> ArrayQ[{{a, b}, {c, d}}, 2, SymbolQ]
     = True
    """
    
    rules = {
        'ArrayQ[expr_]': 'ArrayQ[expr, _, True&]',
        'ArrayQ[expr_, pattern_]': 'ArrayQ[expr, pattern, True&]',
    }
    
    def apply(self, expr, pattern, test, evaluation):
        'ArrayQ[expr_, pattern_, test_]'
        
        pattern = Pattern.create(pattern)
        
        dims = [len(expr.get_leaves())] # to ensure an atom is not an array
        
        def check(level, expr):
            if not expr.has_form('List', None):
                test_expr = Expression(test, expr)
                if test_expr.evaluate(evaluation) != Symbol('True'):
                    return False
                level_dim = None
            else:
                level_dim = len(expr.leaves)
            
            if len(dims) > level:
                if dims[level] != level_dim:
                    return False
            else:
                dims.append(level_dim)
            if level_dim is not None:
                for leaf in expr.leaves:
                    if not check(level + 1, leaf):
                        return False
            return True
        
        if not check(0, expr):
            return Symbol('False')
        
        depth = len(dims) - 1 # None doesn't count
        if not pattern.does_match(Integer(depth), evaluation):
            return Symbol('False')
        return Symbol('True')

class VectorQ(Builtin):
    """
    >> VectorQ[{a, b, c}]
     = True
    """
    
    rules = {
        'VectorQ[expr_]': 'ArrayQ[expr, 1]',
        'VectorQ[expr_, test_]': 'ArrayQ[expr, 1, test]',
    }
    
class NotListQ(Test):
    def test(self, expr):
        return expr.get_head_name() != 'List'

class MatrixQ(Builtin):
    """
    >> MatrixQ[{{1, 3}, {4.0, 3/2}}, NumberQ]
     = True
    """
    
    rules = {
        'MatrixQ[expr_]': 'ArrayQ[expr, 2]',
        'MatrixQ[expr_, test_]': 'ArrayQ[expr, 2, test]',
    }
    
def get_dimensions(expr, head=None):
    if expr.is_atom():
        return []
    else:
        if head is not None and not expr.head.same(head):
            return []
        sub_dim = None
        for leaf in expr.leaves:
            sub = get_dimensions(leaf, expr.head)
            if sub_dim is None:
                sub_dim = sub
            else:
                if sub_dim != sub:
                    sub = []
                    break
        return [len(expr.leaves)] + sub
    
class Dimensions(Builtin):
    """
    >> Dimensions[{a, b, c}]
     = {3}
    >> Dimensions[{{a, b}, {c, d}, {e, f}}]
     = {3, 2}
     
    Ragged arrays are not taken into account:
    >> Dimensions[{{a, b}, {b, c}, {c, d, e}}]
     = {3}
     
    The expression can have any head:
    >> Dimensions[f[f[a, b, c]]]
     = {1, 3}
    """
    
    def apply(self, expr, evaluation):
        'Dimensions[expr_]'
        
        return Expression('List', *(Integer(dim) for dim in get_dimensions(expr)))
    
class ArrayDepth(Builtin):
    """
    >> ArrayDepth[{{a,b},{c,d}}]
     = 2
    >> ArrayDepth[x]
     = 0
    """
    
    rules = {
        'ArrayDepth[list_]': 'Length[Dimensions[list]]',
    }
    
class Dot(BinaryOperator):
    """
    Scalar product of vectors:
    >> {a, b, c} . {x, y, z}
     = a x + b y + c z
    Product of matrices and vectors:
    >> {{a, b}, {c, d}} . {x, y}
     = {a x + b y, c x + d y}
    Matrix product:
    >> {{a, b}, {c, d}} . {{r, s}, {t, u}}
     = {{a r + b t, a s + b u}, {c r + d t, c s + d u}}
    """
    
    operator = '.'
    precedence = 490
    attributes = ('Flat', 'OneIdentity')
    
    rules = {
        'Dot[a_, b_]': 'Inner[Times, a, b, Plus]',
    }
    
class Inner(Builtin):
    """
    >> Inner[f, {a, b}, {x, y}, g]
     = g[f[a, x], f[b, y]]
    
    The inner product of two boolean matrices:
    >> Inner[And, {{False, False}, {False, True}}, {{True, False}, {True, True}}, Or]
     = {{False, False}, {True, True}}
     
    Inner works with tensors of any depth:
    >> Inner[f, {{{a, b}}, {{c, d}}}, {{1}, {2}}, g]
     = {{{g[f[a, 1], f[b, 2]]}}, {{g[f[c, 1], f[d, 2]]}}}
    """
    
    rules = {
        'Inner[f_, list1_, list2_]': 'Inner[f, list1, list2, Plus]',
    }
    
    messages = {
        'incom': "Length `1` of dimension `2` in `3` is incommensurate with length `4` of dimension 1 in `5.",
    }
    
    def apply(self, f, list1, list2, g, evaluation):
        'Inner[f_, list1_, list2_, g_]'
        
        m = get_dimensions(list1)
        n = get_dimensions(list2)
        
        if not m or not n:
            evaluation.message('Inner', 'normal')
            return
        if list1.head != list2.head:
            evaluation.message('Inner', 'heads', list1.head, list2.head)
            return
        if m[-1] != n[0]:
            evaluation.message('Inner', 'incom', m[-1], len(m), list1, n[0], list2)
            return
        
        head = list1.head
        inner_dim = n[0]
        
        def rec(i_cur, j_cur, i_rest, j_rest):
            evaluation.check_stopped()
            if i_rest:
                new = Expression(head)
                for i in range(1, i_rest[0] + 1):
                    new.leaves.append(rec(i_cur + [i], j_cur, i_rest[1:], j_rest))
                return new
            elif j_rest:
                new = Expression(head)
                for j in range(1, j_rest[0] + 1):
                    new.leaves.append(rec(i_cur, j_cur + [j], i_rest, j_rest[1:]))
                return new
            else:
                def summand(i):
                    return Expression(f, get_part(list1, i_cur + [i]), get_part(list2, [i] + j_cur))
                part = Expression(g, *(summand(i) for i in range(1, inner_dim + 1)))
                #cur_expr.leaves.append(part)
                return part
        
        return rec([], [], m[:-1], n[1:])
    
class Outer(Builtin):
    """
    >> Outer[f, {a, b}, {1, 2, 3}]
     = {{f[a, 1], f[a, 2], f[a, 3]}, {f[b, 1], f[b, 2], f[b, 3]}}
     
    Outer product of two matrices:
    >> Outer[Times, {{a, b}, {c, d}}, {{1, 2}, {3, 4}}]
     = {{{{a, 2 a}, {3 a, 4 a}}, {{b, 2 b}, {3 b, 4 b}}}, {{{c, 2 c}, {3 c, 4 c}}, {{d, 2 d}, {3 d, 4 d}}}}
     
    'Outer' of multiple lists:
    >> Outer[f, {a, b}, {x, y, z}, {1, 2}]
     = {{{f[a, x, 1], f[a, x, 2]}, {f[a, y, 1], f[a, y, 2]}, {f[a, z, 1], f[a, z, 2]}}, {{f[b, x, 1], f[b, x, 2]}, {f[b, y, 1], f[b, y, 2]}, {f[b, z, 1], f[b, z, 2]}}}
     
    Arrays can be ragged:
    >> Outer[Times, {{1, 2}}, {{a, b}, {c, d, e}}]
     = {{{{a, b}, {c, d, e}}, {{2 a, 2 b}, {2 c, 2 d, 2 e}}}}
     
    Word combinations:
    >> Outer[StringJoin, {"", "re", "un"}, {"cover", "draw", "wind"}, {"", "ing", "s"}] // InputForm
     = {{{"cover", "covering", "covers"}, {"draw", "drawing", "draws"}, {"wind", "winding", "winds"}}, {{"recover", "recovering", "recovers"}, {"redraw", "redrawing", "redraws"}, {"rewind", "rewinding", "rewinds"}}, {{"uncover", "uncovering", "uncovers"}, {"undraw", "undrawing", "undraws"}, {"unwind", "unwinding", "unwinds"}}}
    
    Compositions of trigonometric functions:
    >> trigs = Outer[Composition, {Sin, Cos, Tan}, {ArcSin, ArcCos, ArcTan}]
     = {{Composition[Sin, ArcSin], Composition[Sin, ArcCos], Composition[Sin, ArcTan]}, {Composition[Cos, ArcSin], Composition[Cos, ArcCos], Composition[Cos, ArcTan]}, {Composition[Tan, ArcSin], Composition[Tan, ArcCos], Composition[Tan, ArcTan]}}
    Evaluate at 0:
    >> Map[#[0] &, trigs, {2}]
     = {{0, 1, 0}, {1, 0, 1}, {0, ComplexInfinity, 0}}
    """
    
    def apply(self, f, lists, evaluation):
        'Outer[f_, lists__]'
        
        lists = lists.get_sequence()
        head = None
        for list in lists:
            if list.is_atom():
                evaluation.message('Outer', 'normal')
                return
            if head is None:
                head = list.head
            elif not list.head.same(head):
                evaluation.message('Outer', 'heads', head, list.head)
                return
            
        def rec(item, rest_lists, current):
            evaluation.check_stopped()
            if item.is_atom() or not item.head.same(head):
                if rest_lists:
                    return rec(rest_lists[0], rest_lists[1:], current + [item])
                else:
                    return Expression(f, *(current + [item]))
            else:
                leaves = []
                for leaf in item.leaves:
                    leaves.append(rec(leaf, rest_lists, current))
                return Expression(head, *leaves)
            
        return rec(lists[0], lists[1:], [])
        
class Transpose(Builtin):
    """
    <dl>
    <dt>'Tranpose[$m$]'
        <dd>transposes rows and columns in the matrix $m$.
    </dl>
    
    >> Transpose[{{1, 2, 3}, {4, 5, 6}}]
     = {{1, 4}, {2, 5}, {3, 6}}
    >> MatrixForm[%]
     = 1   4
     .
     . 2   5
     .
     . 3   6
     
    #> Transpose[x]
     = Transpose[x]
    """
    
    def apply(self, m, evaluation):
        'Transpose[m_?MatrixQ]'
        
        result = []
        for row_index, row in enumerate(m.leaves):
            for col_index, item in enumerate(row.leaves):
                if row_index == 0:
                    result.append([item])
                else:
                    result[col_index].append(item)
        return Expression('List', *[Expression('List', *row) for row in result])
    
class DiagonalMatrix(Builtin):
    """
    <dl>
    <dt>'DiagonalMatrix[$list$]'
        <dd>gives a matrix with the values in $list$ on its diagonal and zeroes elsewhere.
    </dl>
    
    >> DiagonalMatrix[{1, 2, 3}]
     = {{1, 0, 0}, {0, 2, 0}, {0, 0, 3}}
    >> MatrixForm[%]
     = 1   0   0
     .
     . 0   2   0
     .
     . 0   0   3
     
    #> DiagonalMatrix[a + b]
     = DiagonalMatrix[a + b]
    """
    
    def apply(self, list, evaluation):
        'DiagonalMatrix[list_List]'
        
        result = []
        n = len(list.leaves)
        for index, item in enumerate(list.leaves):
            row = [Integer(0)] * n
            row[index] = item
            result.append(Expression('List', *row))
        return Expression('List', *result)
    
class IdentityMatrix(Builtin):
    """
    <dl>
    <dt>'IdentityMatrix[$n$]'
        <dd>gives the identity matrix with $n$ rows and columns.
    </dl>
    
    >> IdentityMatrix[3]
     = {{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}
    """
    
    rules = {
        'IdentityMatrix[n_Integer]': 'DiagonalMatrix[Table[1, {n}]]',
    }
        