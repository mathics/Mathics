# -*- coding: utf-8 -*-

from mathics.version import __version__  # noqa used in loading to check consistency.
from mathics.builtin.base import BinaryOperator, Predefined, PrefixOperator, Builtin
from mathics.builtin.lists import InvalidLevelspecError, python_levelspec, walk_levels
from mathics.core.expression import Expression, Symbol


class Or(BinaryOperator):
    """
    <dl>
    <dt>'Or[$expr1$, $expr2$, ...]'
    <dt>'$expr1$ || $expr2$ || ...'
        <dd>evaluates each expression in turn, returning 'True'
        as soon as an expression evaluates to 'True'. If all
        expressions evaluate to 'False', 'Or' returns 'False'.
    </dl>

    >> False || True
     = True

    If an expression does not evaluate to 'True' or 'False', 'Or'
    returns a result in symbolic form:
    >> a || False || b
     = a || b
    """

    operator = "||"
    precedence = 215
    attributes = ("Flat", "HoldAll", "OneIdentity")

    def apply(self, args, evaluation):
        "Or[args___]"

        args = args.get_sequence()
        leaves = []
        for arg in args:
            result = arg.evaluate(evaluation)
            if result.is_true():
                return Symbol("True")
            elif result != Symbol("False"):
                leaves.append(result)
        if leaves:
            if len(leaves) == 1:
                return leaves[0]
            else:
                return Expression("Or", *leaves)
        else:
            return Symbol("False")


class And(BinaryOperator):
    """
    <dl>
    <dt>'And[$expr1$, $expr2$, ...]'
    <dt>'$expr1$ && $expr2$ && ...'
        <dd>evaluates each expression in turn, returning 'False'
        as soon as an expression evaluates to 'False'. If all
        expressions evaluate to 'True', 'And' returns 'True'.
    </dl>

    >> True && True && False
     = False

    If an expression does not evaluate to 'True' or 'False', 'And'
    returns a result in symbolic form:
    >> a && b && True && c
     = a && b && c
    """

    operator = "&&"
    precedence = 215
    attributes = ("Flat", "HoldAll", "OneIdentity")

    def apply(self, args, evaluation):
        "And[args___]"

        args = args.get_sequence()
        leaves = []
        for arg in args:
            result = arg.evaluate(evaluation)
            if result == Symbol("False"):
                return Symbol("False")
            elif not result.is_true():
                leaves.append(result)
        if leaves:
            if len(leaves) == 1:
                return leaves[0]
            else:
                return Expression("And", *leaves)
        else:
            return Symbol("True")


class Not(PrefixOperator):
    """
    <dl>
    <dt>'Not[$expr$]'
    <dt>'!$expr$'
        <dd>negates the logical expression $expr$.
    </dl>

    >> !True
     = False
    >> !False
     = True
    >> !b
     = !b
    """

    operator = "!"
    precedence = 230

    rules = {
        "Not[True]": "False",
        "Not[False]": "True",
        "Not[Not[expr_]]": "expr",
    }


class Implies(BinaryOperator):
    """
    <dl>
    <dt>'Implies[$expr1$, $expr2$]'
    <dt>'$expr1$ \uF523 $expr2$'
        <dd>evaluates each expression in turn, returning 'True'
        as soon as the first expression evaluates to 'False'. If the
        first expression evaluates to 'True', 'Implies' returns the
        second expression.
    </dl>

    >> Implies[False, a]
     = True
    >> Implies[True, a]
     = a

    If an expression does not evaluate to 'True' or 'False', 'Implies'
    returns a result in symbolic form:
    >> Implies[a, Implies[b, Implies[True, c]]]
     = a \uF523 b \uF523 c
    """

    operator = "\uF523"
    precedence = 200
    grouping = "Right"

    def apply(self, x, y, evaluation):
        "Implies[x_, y_]"

        result0 = x.evaluate(evaluation)
        if result0 == Symbol("False"):
            return Symbol("True")
        elif result0.is_true():
            return y.evaluate(evaluation)
        else:
            return Expression("Implies", result0, y.evaluate(evaluation))


class Equivalent(BinaryOperator):
    """
     <dl>
     <dt>'Equivalent[$expr1$, $expr2$, ...]'
     <dt>'$expr1$ \u29E6 $expr2$ \u29E6 ...'
         <dd>is equivalent to
         ($expr1$ && $expr2$ && ...) || (!$expr1$ && !$expr2$ && ...)
     </dl>

     >> Equivalent[True, True, False]
      = False

     If all expressions do not evaluate to 'True' or 'False', 'Equivalent'
     returns a result in symbolic form:
     >> Equivalent[a, b, c]
      = a \u29E6 b \u29E6 c
      Otherwise, 'Equivalent' returns a result in DNF
     >> Equivalent[a, b, True, c]
      = a && b && c
     #> Equivalent[]
      = True
     #> Equivalent[a]
      = True
     """

    operator = "\u29E6"
    precedence = 205
    attributes = "Orderless"

    def apply(self, args, evaluation):
        "Equivalent[args___]"

        args = args.get_sequence()
        argc = len(args)
        if argc == 0 or argc == 1:
            return Symbol("True")
        flag = False
        for arg in args:
            result = arg.evaluate(evaluation)
            if result == Symbol("False") or result.is_true():
                flag = not flag
                break
        if flag:
            return Expression(
                "Or",
                Expression("And", *args),
                Expression("And", *[Expression("Not", arg) for arg in args]),
            ).evaluate(evaluation)
        else:
            return Expression("Equivalent", *args)


class Xor(BinaryOperator):
    """
    <dl>
    <dt>'Xor[$expr1$, $expr2$, ...]'
    <dt>'$expr1$ \u22BB $expr2$ \u22BB ...'
        <dd>evaluates each expression in turn, returning 'True'
        as soon as not all expressions evaluate to the same value. If all
        expressions evaluate to the same value, 'Xor' returns 'False'.
    </dl>

    >> Xor[False, True]
     = True
    >> Xor[True, True]
     = False

    If an expression does not evaluate to 'True' or 'False', 'Xor'
    returns a result in symbolic form:
    >> Xor[a, False, b]
     = a \u22BB b
    #> Xor[]
     = False
    #> Xor[a]
     = a
    #> Xor[False]
     = False
    #> Xor[True]
     = True
    #> Xor[a, b]
     = a \u22BB b
    """

    operator = "\u22BB"
    precedence = 215
    attributes = ("Flat", "OneIdentity", "Orderless")

    def apply(self, args, evaluation):
        "Xor[args___]"

        args = args.get_sequence()
        leaves = []
        flag = True
        for arg in args:
            result = arg.evaluate(evaluation)
            if result.is_true():
                flag = not flag
            elif result != Symbol("False"):
                leaves.append(result)
        if leaves and flag:
            if len(leaves) == 1:
                return leaves[0]
            else:
                return Expression("Xor", *leaves)
        elif leaves and not flag:
            if len(leaves) == 1:
                return Expression("Not", leaves[0])
            else:
                return Expression("Not", Expression("Xor", *leaves))
        else:
            return Symbol(repr(not flag))


class True_(Predefined):
    """
    <dl>
    <dt>'True'
        <dd>represents the Boolean true value.
    </dl>
    """

    name = "True"


class False_(Predefined):
    """
    <dl>
    <dt>'False'
        <dd>represents the Boolean false value.
    </dl>
    """

    name = "False"


class _ShortCircuit(Exception):
    def __init__(self, result):
        self.result = result


class _ManyTrue(Builtin):
    rules = {
        "%(name)s[list_List, test_]": "%(name)s[list, test, 1]",
        "%(name)s[test_][list_List]": "%(name)s[list, test]",
    }

    def _short_circuit(self, what):
        raise NotImplementedError

    def _no_short_circuit(self):
        raise NotImplementedError

    def apply(self, expr, test, level, evaluation):
        "%(name)s[expr_, test_, level_]"

        try:
            start, stop = python_levelspec(level)
        except InvalidLevelspecError:
            evaluation.message("Level", "level", level)
            return

        def callback(node):
            self._short_circuit(Expression(test, node).evaluate(evaluation).is_true())
            return node

        try:
            walk_levels(expr, start, stop, callback=callback)
        except _ShortCircuit as e:
            return e.result

        return self._no_short_circuit()


class NoneTrue(_ManyTrue):
    """
    <dl>
    <dt>'NoneTrue[{$expr1$, $expr2$, ...}, $test$]'
        <dd>returns True if no application of $test$ to $expr1$, $expr2$, ... evaluates to True.
    <dt>'NoneTrue[$list$, $test$, $level$]'
        <dd>returns True if no application of $test$ to items of $list$ at $level$ evaluates to True.
    <dt>'NoneTrue[$test$]'
        <dd>gives an operator that may be applied to expressions.
    </dl>

    >> NoneTrue[{1, 3, 5}, EvenQ]
     = True

    >> NoneTrue[{1, 4, 5}, EvenQ]
     = False

    #> NoneTrue[{}, EvenQ]
     = True
    """

    def _short_circuit(self, what):
        if what:
            raise _ShortCircuit(Symbol("False"))

    def _no_short_circuit(self):
        return Symbol("True")


class AnyTrue(_ManyTrue):
    """
    <dl>
    <dt>'AnyTrue[{$expr1$, $expr2$, ...}, $test$]'
        <dd>returns True if any application of $test$ to $expr1$, $expr2$, ... evaluates to True.
    <dt>'AnyTrue[$list$, $test$, $level$]'
        <dd>returns True if any application of $test$ to items of $list$ at $level$ evaluates to True.
    <dt>'AnyTrue[$test$]'
        <dd>gives an operator that may be applied to expressions.
    </dl>

    >> AnyTrue[{1, 3, 5}, EvenQ]
     = False

    >> AnyTrue[{1, 4, 5}, EvenQ]
     = True

    #> AnyTrue[{}, EvenQ]
     = False
    """

    def _short_circuit(self, what):
        if what:
            raise _ShortCircuit(Symbol("True"))

    def _no_short_circuit(self):
        return Symbol("False")


class AllTrue(_ManyTrue):
    """
    <dl>
    <dt>'AllTrue[{$expr1$, $expr2$, ...}, $test$]'
        <dd>returns True if all applications of $test$ to $expr1$, $expr2$, ... evaluate to True.
    <dt>'AllTrue[$list$, $test$, $level$]'
        <dd>returns True if all applications of $test$ to items of $list$ at $level$ evaluate to True.
    <dt>'AllTrue[$test$]'
        <dd>gives an operator that may be applied to expressions.
    </dl>

    >> AllTrue[{2, 4, 6}, EvenQ]
     = True

    >> AllTrue[{2, 4, 7}, EvenQ]
     = False

    #> AllTrue[{}, EvenQ]
     = True
    """

    def _short_circuit(self, what):
        if not what:
            raise _ShortCircuit(Symbol("False"))

    def _no_short_circuit(self):
        return Symbol("True")
