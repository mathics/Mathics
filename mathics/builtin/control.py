# -*- coding: utf8 -*-

"""
Control statements
"""

from mathics.builtin.base import Builtin, Predefined, BinaryOperator
from mathics.core.expression import Expression, Symbol, from_python
from mathics.core.evaluation import AbortInterrupt, ReturnInterrupt, BreakInterrupt, ContinueInterrupt

from mathics.builtin.lists import _IterationFunction
from mathics.builtin.patterns import match

class CompoundExpression(BinaryOperator):
    """
    <dl>
    <dt>'CompoundExpression[$e1$, $e2$, ...]' or '$e1$; $e2$; ...' 
        <dd>evaluates its arguments in turn, returning the last result.
    </dl>
    >> a; b; c; d
     = d
    If the last argument is omitted, 'Null' is taken:
    >> a;

    ## Parser Tests
    #> FullForm[Hold[a ;]]
     = Hold[CompoundExpression[a, Null]]
    #> FullForm[Hold[a ; b]]
     = Hold[CompoundExpression[a, b]]
    #> FullForm[Hold[a ; b ;]]
     = Hold[CompoundExpression[a, b, Null]]
    #> FullForm[Hold[a ; b ; c]]
     = Hold[CompoundExpression[a, b, c]]
    #> FullForm[Hold[a ; ; c]]
     = Hold[CompoundExpression[a, Null, c]]
    #> FullForm[Hold[a ; ;]]
     = Hold[CompoundExpression[a, Null, Null]]
    #> FullForm[Hold[; a]]
     : Parse error at or near token ;.
    #> FullForm[Hold[; a ;]]
     : Parse error at or near token ;.
    """
    
    operator = ';'
    precedence = 10
    attributes = ('HoldAll', 'ReadProtected')
    
    def apply(self, expr, evaluation):
        'CompoundExpression[expr___]'
        
        items = expr.get_sequence()
        result = Symbol('Null')
        for expr in items:
            result = expr.evaluate(evaluation)
        return result

class If(Builtin):
    """
    <dl>
    <dt>'If[$cond$, $pos$, $neg$]'
        <dd>returns $pos$ if $cond$ evaluates to 'True', and $neg$ if it evaluates to 'False'.
    <dt>'If[$cond$, $pos$, $neg$, $other$]'
        <dd>returns $other$ if $cond$ evaluates to neither 'True' nor 'False'.
    <dt>'If[$cond$, $pos$]'
        <dd>returns 'Null' if $cond$ evaluates to 'False'.
    </dl>
    >> If[1<2, a, b]
     = a
    If the second branch is not specified, 'Null' is taken:
    >> If[1<2, a]
     = a
    >> If[False, a] //FullForm
     = Null
    
    You might use comments (inside '(*' and '*)') to make the branches of 'If' more readable:
    >> If[a, (*then*) b, (*else*) c];
    """
    
    attributes = ('HoldRest',)
    
    def apply_2(self, condition, t, evaluation):
        'If[condition_, t_]'
        
        name = condition.get_name()
        if name == 'True':
            return t.evaluate(evaluation)
        elif name == 'False':
            return Symbol('Null')
    
    def apply_3(self, condition, t, f, evaluation):
        'If[condition_, t_, f_]'
        
        name = condition.get_name()
        if name == 'True':
            return t.evaluate(evaluation)
        elif name == 'False':
            return f.evaluate(evaluation)
    
    def apply_4(self, condition, t, f, u, evaluation):
        'If[condition_, t_, f_, u_]'
        
        name = condition.get_name()
        if name == 'True':
            return t.evaluate(evaluation)
        elif name == 'False':
            return f.evaluate(evaluation)
        else:
            return u.evaluate(evaluation)
        
class Switch(Builtin):
    """
    <dl>
    <dt>'Switch[$expr$, $pattern1$, $value1$, $pattern2$, $value2$, ...]'
        <dd>yields the first $value$ for which $expr matches the corresponding $pattern$.
    </dl>
    
    >> Switch[2, 1, x, 2, y, 3, z]
     = y
    >> Switch[5, 1, x, 2, y]
     = Switch[5, 1, x, 2, y]
    >> Switch[5, 1, x, 2, y, _, z]
     = z
    >> Switch[2, 1]
     : Switch called with 2 arguments. Switch must be called with an odd number of arguments.
     = Switch[2, 1]

    #> a; Switch[b, b]
     : Switch called with 2 arguments. Switch must be called with an odd number of arguments.
     = Switch[b, b]
    """
    
    attributes = ('HoldRest',)
    
    messages = {
        'argct': "Switch called with `2` arguments. Switch must be called with an odd number of arguments.",
    }
    
    def apply(self, expr, rules, evaluation):
        'Switch[expr_, rules___]'
        
        rules = rules.get_sequence()
        if len(rules) % 2 != 0:
            evaluation.message('Switch', 'argct', 'Switch', len(rules) + 1)
            return
        for pattern, value in zip(rules[::2], rules[1::2]):
            if match(expr, pattern, evaluation):
                return value.evaluate(evaluation) 
        # return unevaluated Switch when no pattern matches
        
class Which(Builtin):
    """
    <dl>
    <dt>'Which[$cond1$, $expr1$, $cond2$, $expr2$, ...]'
        <dd>yields $expr1$ if $cond1$ evaluates to 'True', $expr2$ if $cond2$ evaluates to 'True', etc.
    </dl>
    >> n = 5;
    >> Which[n == 3, x, n == 5, y]
     = y
    >> f[x_] := Which[x < 0, -x, x == 0, 0, x > 0, x]
    >> f[-3]
     = 3
    If no test yields 'True', 'Which' returns 'Null':
    >> Which[False, a]
    
    'Which' must be called with an even number of arguments:
    >> Which[a, b, c]
     : Which called with 3 arguments.
     = Which[a, b, c]
    """
    
    attributes = ('HoldAll',)
    
    def apply(self, items, evaluation):
        'Which[items___]'
        
        items = items.get_sequence()
        if len(items) == 1:
            evaluation.message('Which', 'argctu', 'Which')
            return
        elif len(items) % 2 == 1:
            evaluation.message('Which', 'argct', 'Which', len(items))
            return
        for test, item in zip(items[::2], items[1::2]):
            if test.evaluate(evaluation) == Symbol('True'):
                return item.evaluate(evaluation)
        return Symbol('Null')
        
class Do(_IterationFunction):
    """
    <dl>
    <dt>'Do[$expr$, {$max$}]'
        <dd>evaluates $expr$ $max$ times.
    <dt>'Do[$expr$, {$i$, $max$}]'
        <dd>evaluates $expr$ $max$ times, substituting $i$ in $expr$ with values from 1 to $max$.
    <dt>'Do[$expr$, {$i$, $min$, $max$}]'
        <dd>starts with '$i$ = $max$'.
    <dt>'Do[$expr$, {$i$, $min$, $max$, $step$}]'
        <dd>uses a step size of $step$.
    <dt>'Do[$expr$, {$i$, {$i1$, $i2$, ...}}]'
        <dd>uses values $i1$, $i2$, ... for $i$.
    <dt>'Do[$expr$, {$i$, $imin$, $imax$}, {$j$, $jmin$, $jmax$}, ...]'
        <dd>evaluates $expr$ for each $j$ from $jmin$ to $jmax$, for each $i$ from $imin$ to $imax$, etc.
    </dl>
    >> Do[Print[i], {i, 2, 4}]
     | 2
     | 3
     | 4
    >> Do[Print[{i, j}], {i,1,2}, {j,3,5}]
     | {1, 3}
     | {1, 4}
     | {1, 5}
     | {2, 3}
     | {2, 4}
     | {2, 5}
    You can use 'Break[]' and 'Continue[]' inside 'Do':
    >> Do[If[i > 10, Break[], If[Mod[i, 2] == 0, Continue[]]; Print[i]], {i, 5, 20}]
     | 5
     | 7
     | 9
     
    #> Do[Print["hi"],{1+1}]
     | hi
     | hi
    """
    
    allow_loopcontrol = True
    
    def get_result(self, items):
        return Symbol('Null')
        
class For(Builtin):
    """
    <dl>
    <dt>'For[$start$, $test$, $incr$, $body$]'
        <dd>evaluates $start$, and then iteratively $body$ and $incr$ as long as $test$ evaluates to 'True'.
    <dt>'For[$start$, $test$, $incr$]'
        <dd>evaluates only $incr$ and no $body$.
    <dt>'For[$start$, $test$]'
        <dd>runs the loop without any body.
    </dl>
    Compute the factorial of 10 using 'For':
    >> n := 1
    >> For[i=1, i<=10, i=i+1, n = n * i]
    >> n
     = 3628800
    >> n == 10!
     = True
    """
    
    attributes = ('HoldRest',)
    rules = {
        'For[start_, test_, incr_]': 'For[start, test, incr, Null]',
    }
    
    def apply(self, start, test, incr, body, evaluation):
        'For[start_, test_, incr_, body_]'
        
        while test.evaluate(evaluation) == Symbol('True'):
            evaluation.check_stopped()
            try:
                try:
                    body.evaluate(evaluation)
                except ContinueInterrupt:
                    pass
                try:
                    incr.evaluate(evaluation)
                except ContinueInterrupt:   # critical, most likely leads to an infinite loop
                    pass
            except BreakInterrupt:
                break
        return Symbol('Null')
    
class While(Builtin):
    """
    <dl>
    <dt>'While[$test$, $body$]'
        <dd>evaluates $body$ as long as $test$ evaluates to 'True'.
    <dt>'While[$test$]'
        <dd>runs the loop without any body.
    </dl>
    Compute the GCD of two numbers:
    >> {a, b} = {27, 6};
    >> While[b != 0, {a, b} = {b, Mod[a, b]}];
    >> a
     = 3
    """
    
    attributes = ('HoldAll',)
    rules = {
        'While[test_]': 'While[test, Null]',
    }
    
    def apply(self, test, body, evaluation):
        'While[test_, body_]'
        
        while test.evaluate(evaluation) == Symbol('True'):
            try:
                evaluation.check_stopped()
                body.evaluate(evaluation)
            except ContinueInterrupt:
                pass
            except BreakInterrupt:
                break
        return Symbol('Null')
    
class Nest(Builtin):
    """
    <dl>
    <dt>'Nest[$f$, $expr$, $n$]'
        <dd>starting with $expr$, iteratively applies $f$ $n$ times and returns the final result.
    </dl>
    
    >> Nest[f, x, 3]
     = f[f[f[x]]]
    >> Nest[(1+#) ^ 2 &, x, 2]
     = (1 + (1 + x) ^ 2) ^ 2
    """
    
    def apply(self, f, expr, n, evaluation):
        'Nest[f_, expr_, n_Integer]'
        
        n = n.get_int_value()
        if n is None or n < 0:
            return
        result = expr
        for k in range(n):
            result = Expression(f, result).evaluate(evaluation)
        return result

class NestList(Builtin):
    """
    <dl>
    <dt>'NestList[$f$, $expr$, $n$]'
        <dd>starting with $expr$, iteratively applies $f$ $n$ times and returns a list of all intermediate results.
    </dl>
    
    >> NestList[f, x, 3]
     = {x, f[x], f[f[x]], f[f[f[x]]]}
    >> NestList[2 # &, 1, 8]
     = {1, 2, 4, 8, 16, 32, 64, 128, 256}

    ## TODO: improve this example when RandomChoice, PointSize, Axes->False are implemented
    Chaos game rendition of the Sierpinski triangle:
    >> vertices = {{0,0}, {1,0}, {.5, .5 Sqrt[3]}};
    >> points = NestList[.5(vertices[[ RandomInteger[{1,3}] ]] + #) &, {0.,0.}, 2000];
    >> Graphics[Point[points], ImageSize->Small]
     = -Graphics-
    """
    
    def apply(self, f, expr, n, evaluation):
        'NestList[f_, expr_, n_Integer]'
        
        n = n.get_int_value()
        if n is None or n < 0:
            return

        interm = expr
        result = [interm]

        for k in range(n):
            interm = Expression(f, interm).evaluate(evaluation)
            result.append(interm)

        return from_python(result)

class NestWhile(Builtin):
    """
    <dl>
    <dt>'NestWhile[$f$, $expr$, $test$]'
        <dd>applies a function $f$ repeatedly on an expression $expr$, until
        applying $test$ on the result no longer yields 'True'.
    <dt>'NestWhile[$f$, $expr$, $test$, $m$]'
        <dd>supplies the last $m$ results to $test$ (default value: 1).
    <dt>'NestWhile[$f$, $expr$, $test$, All]'
        <dd>supplies all results gained so far to $test$.
    </dl>
    
    Divide by 2 until the result is no longer an integer:
    >> NestWhile[#/2&, 10000, IntegerQ]
     = 625 / 2
    """
    
    rules = {
        'NestWhile[f_, expr_, test_]': 'NestWhile[f, expr, test, 1]',
    }
    
    def apply(self, f, expr, test, m, evaluation):
        'NestWhile[f_, expr_, test_, Pattern[m,_Integer|All]]'
        
        results = [expr]
        while True:
            if m.get_name() == 'All':
                test_leaves = results
            else:
                test_leaves = results[-m.value:]
            test_expr = Expression(test, *test_leaves)
            test_result = test_expr.evaluate(evaluation)
            if test_result == Symbol('True'):
                next = Expression(f, results[-1])
                results.append(next.evaluate(evaluation))
            else:
                break
        return results[-1]
    
class FixedPoint(Builtin):
    """
    <dl>
    <dt>'FixedPoint[$f$, $expr$]'
        <dd>starting with $expr$, iteratively applies $f$ until the result no longer changes.
    <dt>'FixedPoint[$f$, $expr$, $n$]'
        <dd>performs at most $n$ iterations.
    </dl>
    
    >> FixedPoint[Cos, 1.0]
     = 0.739085
     
    >> FixedPoint[#+1 &, 1, 20]
     = 21
     
    #> FixedPoint[f, x, 0]
     = x
    #> FixedPoint[f, x, -1]
     : Non-negative integer expected.
     = FixedPoint[f, x, -1]
    #> FixedPoint[Cos, 1.0, Infinity]
     = 0.739085
    """
    
    def apply(self, f, expr, n, evaluation):
        'FixedPoint[f_, expr_, n_:DirectedInfinity[1]]'
        
        if n == Expression('DirectedInfinity', 1):
            count = None
        else:
            count = n.get_int_value()
            if count is None or count < 0:
                evaluation.message('FixedPoint', 'intnn')
                return
        result = expr
        index = 0
        while count is None or index < count:
            evaluation.check_stopped()
            new_result = Expression(f, result).evaluate(evaluation)
            #print '%d: %s' % (index, new_result)
            if new_result == result:
                result = new_result
                break
            result = new_result
            index += 1
            
        return result

class FixedPointList(Builtin):
    """
    <dl>
    <dt>'FixedPointList[$f$, $expr$]'
        <dd>starting with $expr$, iteratively applies $f$ until the result no longer changes, and returns a list of all intermediate results.
    <dt>'FixedPointList[$f$, $expr$, $n$]'
        <dd>performs at most $n$ iterations.
    </dl>
    
    >> FixedPointList[Cos, 1.0, 4]
     = {1., 0.540302, 0.857553, 0.65429, 0.79348}

    Observe the convergence of Newton's method for approximating square roots:
    >> newton[n_] := FixedPointList[.5(# + n/#) &, 1.];
    >> newton[9]
     = {1., 5., 3.4, 3.02353, 3.00009, 3., 3., 3.}
    
    Plot the "hailstone" sequence of a number:
    >> collatz[1] := 1;
    >> collatz[x_ ? EvenQ] := x / 2;
    >> collatz[x_] := 3 x + 1;
    >> list = FixedPointList[collatz, 14]
     = {14, 7, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1, 1}
    >> ListLinePlot[list]
     = -Graphics-

    #> FixedPointList[f, x, 0]
     = {x}
    #> FixedPointList[f, x, -1]
     : Non-negative integer expected.
     = FixedPointList[f, x, -1]
    #> Last[FixedPointList[Cos, 1.0, Infinity]]
     = 0.739085
    """
    
    def apply(self, f, expr, n, evaluation):
        'FixedPointList[f_, expr_, n_:DirectedInfinity[1]]'
        
        if n == Expression('DirectedInfinity', 1):
            count = None
        else:
            count = n.get_int_value()
            if count is None or count < 0:
                evaluation.message('FixedPoint', 'intnn')
                return

        interm = expr
        result = [interm]

        index = 0
        while count is None or index < count:
            evaluation.check_stopped()

            new_result = Expression(f, interm).evaluate(evaluation)
            result.append(new_result)
            if new_result == interm:
                break

            interm = new_result
            index += 1
            
        return from_python(result)
    
class Abort(Builtin):
    """
    <dl>
    <dt>'Abort[]'
        <dd>aborts an evaluation completely and returns '$Aborted'.
    </dl>
    >> Print["a"]; Abort[]; Print["b"]
     | a
     = $Aborted
    """
    
    def apply(self, evaluation):
        'Abort[]'
        
        raise AbortInterrupt

#class Return(Builtin):
#    pass

class Break(Builtin):
    """
    <dl>
    <dt>'Break[]'
        <dd>exits a 'For', 'While', or 'Do' loop.
    </dl>
    >> n = 0;
    >> While[True, If[n>10, Break[]]; n=n+1]
    >> n
     = 11
    """
    
    messages = {
        'nofwd': "No enclosing For, While, or Do found for Break[].",
    }
    
    def apply(self, evaluation):
        'Break[]'
        
        raise BreakInterrupt

class Continue(Builtin):
    """
    <dl>
    <dt>'Continue[]'
        <dd>continues with the next iteration in a 'For', 'While', or 'Do' loop.
    </dl>
    >> For[i=1, i<=8, i=i+1, If[Mod[i,2] == 0, Continue[]]; Print[i]]
     | 1
     | 3
     | 5
     | 7
    """
    
    messages = {
        'nofwd': "No enclosing For, While, or Do found for Continue[].",
    }
    
    def apply(self, evaluation):
        'Continue[]'
        
        raise ContinueInterrupt
