# -*- coding: utf-8 -*-

from mathics.version import __version__  # noqa used in loading to check consistency.

from mathics.core.expression import (
    Expression,
    Symbol,
    SymbolTrue,
    SymbolFalse,
)

from mathics.core.rules import Rule
from mathics.core.parser.util import SystemDefinitions

from mathics.core.parser import parse_builtin_rule

# TODO: Extend these rules?


def debug_logical_expr(pref, expr, evaluation):
    pass
    # return
    # print(pref , expr) #expr.format(evaluation,"OutputForm").boxes_to_text(evaluation=evaluation))


logical_algebraic_rules_spec = {
    # Inequality rules
    "Unequal[a_, b_]": "Not[Equal[a, b]]",
    "Greater[a_, b_]": "Less[b, a]",
    "GreaterEqual[a_, b_]": "Not[Less[a, b]]",
    "LessEqual[a_, b_]": "Not[Less[b, a]]",
    "PositiveQ[a_]": "Less[0, a]",
    "NegativeQ[a_]": "Less[a, 0]",
    # Logical basic reductions
    "Or[q_, Not[q_]]": "True",
    "Or[q_,]": "q",
    "Or[q_, q_]": "q",
    "Or[pred1___, q_, pred2___, q_, pred3___]": "Or[pred1, q, pred2, pred3]",
    # TODO: Logical operations should sort the leaves...
    "Or[Not[q_], q_]": "True",
    "Or[pred1___, q_, pred2___, Not[q_], pred3___]": "Or[pred1, pred2, pred3]",
    "Or[pred1___, Not[q_], pred2___, q_, pred3___]": "Or[pred1, pred2, pred3]",
    "And[q_,q_]": "q",
    "And[q_, Not[q_]]": "False",
    "And[Not[q_],q_]": "False",
    "And[pred1___, q_, pred2___, Not[q_], pred3___]": "False",
    "And[pred1___, Not[q_], pred2___, q_, pred3___]": "False",
    # Logical reductions involving equalities
    "Or[pred1___, a_==b_, pred2___ , b_==a_, pred3___]": "Or[pred1, a==b, pred2, pred3]",
    "And[pred1___, a_==b_, pred2___ , b_==a_, pred3___]": "And[pred1, a==b, pred2, pred3]",
    "Or[pred1___, a_==b_, pred2___ , Not[b_==a_], pred3___]": "Or[pred1, pred2, pred3]",
    "And[pred1___, a_==b_, pred2___ , Not[b_==a_], pred3___]": "False",
    "Xor[q_,  Not[q_]]": "True",
    "Xor[a_==b_,  Not[b_==a_]]": "True",
    # Logical reductions involving inequalities
    "Or[a_<b_, b_<a_]": "! (a==b)",
    "And[a_<b_, b_<a_]": "False",
    "Or[a_<b_, b_==a_]": "! (a>b)",
    "Or[b_==a_, a_<b_]": "! (a>b)",
    "And[a_<b_, b_==a_]": "False",
    "And[b_==a_, a_<b_]": "False",
    "And[pred1___, a_<b_, pred2___, Not[b_<a_], pred3___ ]": "And[pred1, a==b, pred2, pred3]",
    "And[pred1___, a_<b_, pred2___, b_<a_, pred3___ ]": "False",
    "And[pred1___, a_<b_, pred2___, b_==a_, pred3___ ]": "False",
    "Or[pred1___, a_<b_, pred2___, b_<a_, pred3___ ]": "And[pred1,Not[a==b],pred2, pred3]",
    "Or[pred1___, a_<b_, pred2___, b_==a_, pred3___ ]": "And[pred1,Not[b<a],pred2, pred3]",
    "Or[pred1___, q_, pred2___, Not[q_], pred3___  ]": "Or[pred1, pred2, pred3]",
    # Let's assume that variables are finite
    "-Infinity< Infinity": "True",
    "Infinity< -Infinity ": "False",
    "Infinity == -Infinity ": "False",
    "_Symbol < Infinity ": "True",
    "-Infinity  <_Symbol": "True",
}

remove_not_rules_spec = {
    "Not[a_<b_]": "a>=b",
    "Not[a_==b_]": "a!=b",
}


logical_algebraic_rules = None
remove_not_rules = None


def ensure_logical_algebraic_rules():
    global logical_algebraic_rules
    global remove_not_rules
    if logical_algebraic_rules is None:
        logical_algebraic_rules = []
        for pattern, replace in logical_algebraic_rules_spec.items():
            pattern = parse_builtin_rule(pattern, SystemDefinitions())
            logical_algebraic_rules.append(
                Rule(pattern, parse_builtin_rule(replace), system=True)
            )
        remove_not_rules = []
        for pattern, replace in remove_not_rules_spec.items():
            pattern = parse_builtin_rule(pattern, SystemDefinitions())
            remove_not_rules.append(
                Rule(pattern, parse_builtin_rule(replace), system=True)
            )
    return


def remove_nots_when_unnecesary(pred, evaluation):
    global remove_not_rules
    cc = True
    while cc:
        pred, cc = pred.apply_rules(remove_not_rules, evaluation)
        debug_logical_expr("->  ", pred, evaluation)
        if pred.is_true() or pred == SymbolFalse:
            return pred
    return pred


def get_assumptions_list(evaluation):
    assumptions = None
    assumptions_def = evaluation.definitions.get_definition(
        "System`$Assumptions", only_if_exists=True
    )
    if assumptions_def:
        assumptions = assumptions_def.ownvalues
        if len(assumptions) > 0:
            assumptions = assumptions[0].replace
    if assumptions is None:
        return None

    if assumptions.is_atom() or not assumptions.has_form("List", None):
        assumptions = (assumptions,)
    else:
        assumptions = assumptions._leaves

    return assumptions


def remove_duplicated_assumptions(assumptions_list, evaluation):
    if len(assumptions_list) == 0:
        return assumptions_list
    assumptions_list = sorted(assumptions_list)
    unique_assumptions = [assumptions_list[0]]
    for i, assumption in enumerate(assumptions_list):
        if not (assumption == unique_assumptions[-1]):
            unique_assumptions.append(assumption)
    return unique_assumptions


def logical_expand_assumptions(assumptions_list, evaluation):
    new_assumptions_list = []
    changed = False
    for assumption in assumptions_list:
        if assumption.is_symbol():
            if assumption.is_true():
                changed = True
                continue
            if assumption == SymbolFalse:
                evaluation.message("Assumption", "faas")
                changed = True
                continue
            if assumption.is_numeric():
                evaluation.message("Assumption", "baas")
                changed = True
                continue
            new_assumptions_list.append(assumption)
            continue
        if assumption.has_form("And", None):
            changed = True
            for leaf in assumption.leaves:
                new_assumptions_list.append(leaf)
            continue
        if assumption.has_form("Not", 1):
            sentence = assumption._leaves[0]
            if sentence.has_form("Or", None):
                changed = True
                for leaf in sentence._leaves:
                    new_assumptions_list.append(Expression("Not", leaf))
                continue
            if sentence.has_form("And", None):
                leaves = (Expression("Not", leaf) for leaf in sentence._leaves)
                new_assumptions_list.append(Expression("Or", *leaves))
                continue
            if sentence.has_form("Implies", 2):
                changed = True
                new_assumptions_list.append(sentence._leaves[0])
                new_assumptions_list.append(Expression("Not", sentence._leaves[1]))
        if assumption.has_form("Nor", None):
            changed = True
            for leaf in assumption.leaves:
                new_assumptions_list.append(Expression("Not", leaf))
            continue
        else:
            new_assumptions_list.append(assumption)

    if changed:
        new_assumptions_list = remove_duplicated_assumptions(
            new_assumptions_list, evaluation
        )

    return new_assumptions_list, changed


def algebraic_expand_assumptions(assumptions_list, evaluation):
    global logical_algebraic_rules
    ensure_logical_algebraic_rules()
    new_assumptions_list = []
    changed = False
    # First apply standard rules of reduction.
    # These rules are generated the first time that are used.
    for assumption in assumptions_list:
        assumption, applied = assumption.apply_rules(
            logical_algebraic_rules, evaluation
        )
        changed = changed or applied
        new_assumptions_list.append(assumption)
    if changed:
        return new_assumptions_list, changed
    # If not changed, let's try with the next set of rules
    for assumption in assumptions_list:
        if assumption.has_form("Not", 1):
            nas, local_change = algebraic_expand_assumptions(
                [assumption._leaves[0]], evaluation
            )
            if local_change:
                changed = local_change
                for na in nas:
                    if na.has_form("Not", 1):
                        new_assumptions_list.append(na._leaves[0])
                    else:
                        new_assumptions_list.append(Expression("Not", na))
            else:
                new_assumptions_list.append(assumption)
        elif assumption.has_form(("Equal", "Unequal", "Equivalent"), (3, None)):
            leaves = assumption.leaves()
            head = assumption.get_head()
            changed = True
            for i in range(len(leaves)):
                for j in range(i):
                    new_assumptions_list.append(Expression(head, leaves[i], leaves[j]))
                    new_assumptions_list.append(Expression(head, leaves[j], leaves[i]))
        elif assumption.has_form(
            ("Less", "Greater", "LessEqual", "GreaterEqual"), (3, None)
        ):
            leaves = assumption.leaves()
            head = assumption.get_head()
            changed = True
            for i in range(len(leaves)):
                for j in range(i):
                    new_assumptions_list.append(Expression(head, leaves[i], leaves[j]))
        else:
            new_assumptions_list.append(assumption)

    if changed:
        assumptions_list = remove_duplicated_assumptions(
            new_assumptions_list, evaluation
        )
        new_assumptions_list = []
        for assumption in assumptions_list:
            assumption, applied = assumption.apply_rules(
                logical_algebraic_rules, evaluation
            )
            new_assumptions_list.append(assumption)
    return new_assumptions_list, changed


def get_assumption_rules_dispatch(evaluation):
    # TODO: cache the generated rules...
    assumptions_list = get_assumptions_list(evaluation)
    if assumptions_list is None:
        return None

    # check for consistency:
    consistent_assumptions = Expression("And", *assumptions_list)
    val_consistent_assumptions = consistent_assumptions.evaluate(evaluation)
    if val_consistent_assumptions == SymbolFalse:
        evaluation.message("Inconsistent assumptions")

    if assumptions_list is None:
        return remove_nots_when_unnecesary(pred, evaluation).evaluate(evaluation)

    # Expands Logically
    assumptions_list, cont = logical_expand_assumptions(assumptions_list, evaluation)
    while cont:
        assumptions_list, cont = logical_expand_assumptions(
            assumptions_list, evaluation
        )

    # Expands algebraically
    assumptions_list, cont = algebraic_expand_assumptions(assumptions_list, evaluation)
    while cont:
        assumptions_list, cont = algebraic_expand_assumptions(
            assumptions_list, evaluation
        )
    assumption_rules = []
    for pat in assumptions_list:
        value = True
        while pat.has_form("Not", 1):
            value = not value
            pat = pat._leaves[0]

        if value:
            symbol_value = SymbolTrue
            symbol_negate_value = SymbolFalse
        else:
            symbol_value = SymbolFalse
            symbol_negate_value = SymbolTrue

        if pat.has_form("Equal", 2):
            if value:
                lhs, rhs = pat._leaves
                if lhs.is_numeric():
                    assumption_rules.append(Rule(rhs, lhs))
                else:
                    assumption_rules.append(Rule(lhs, rhs))
            else:
                assumption_rules.append(Rule(pat, SymbolFalse))
                symm_pat = Expression(pat._head, pat._leaves[1], pat._leaves[0])
                assumption_rules.append(Rule(symm_pat, SymbolFalse))
        elif pat.has_form("Equivalent", 2):
            assumption_rules.append(Rule(pat, symbol_value))
            symm_pat = Expression(pat._head, pat._leaves[1], pat._leaves[0])
            assumption_rules.append(Rule(symm_pat, symbol_value))
        elif pat.has_form("Less", 2):
            if value:
                assumption_rules.append(Rule(pat, SymbolTrue))
                assumption_rules.append(
                    Rule(
                        Expression(pat._head, pat._leaves[1], pat._leaves[0]),
                        SymbolFalse,
                    )
                )
                for head in ("Equal", "Equivalent"):
                    assumption_rules.append(
                        Rule(
                            Expression(head, pat._leaves[0], pat._leaves[1]),
                            SymbolFalse,
                        )
                    )
                    assumption_rules.append(
                        Rule(
                            Expression(head, pat._leaves[1], pat._leaves[0]),
                            SymbolFalse,
                        )
                    )
            else:
                assumption_rules.append(Rule(pat, SymbolFalse))
        else:
            assumption_rules.append(Rule(pat, symbol_value))
    # TODO: expand the pred and assumptions into an standard,
    # atomized form, and then apply the rules...
    if len(assumption_rules) == 0:
        return None
    return assumption_rules


def evaluate_predicate(pred, evaluation):
    global logical_algebraic_rules
    global remove_not_rules

    if pred.has_form(("List", "Sequence"), None):
        return Expression(
            pred._head, *[evaluate_predicate(subp, evaluation) for subp in pred._leaves]
        )

    debug_logical_expr("reducing ", pred, evaluation)
    ensure_logical_algebraic_rules()
    pred = pred.evaluate(evaluation)
    debug_logical_expr("->  ", pred, evaluation)
    cc = True
    while cc:
        pred, cc = pred.apply_rules(logical_algebraic_rules, evaluation)
        debug_logical_expr("->  ", pred, evaluation)
        if pred.is_true() or pred == SymbolFalse:
            return pred

    assumption_rules = get_assumption_rules_dispatch(evaluation)
    if assumption_rules is None:
        return remove_nots_when_unnecesary(pred, evaluation).evaluate(evaluation)

    if assumption_rules is not None:
        debug_logical_expr(" Now, using the assumptions over ", pred, evaluation)
        changed = True
        while changed:
            pred, changed = pred.apply_rules(assumption_rules, evaluation)
            debug_logical_expr(" -> ", pred, evaluation)

    pred = remove_nots_when_unnecesary(pred, evaluation).evaluate(evaluation)
    return pred
