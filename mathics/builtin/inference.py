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



def evaluate_predicate(pred, evaluation):
    if pred.has_form(("List", "Sequence"), None):
        return Expression(pred._head,  *[evaluate_predicate(subp, evaluation) for subp in pred._leaves]  )
    assumptions = get_assumptions_list(evaluation)

    assumption_rules = []
    for assumption in assumptions:
        true_state = True
        while assumption.has_form("Not",1):
            true_state = False
            assumption = assumption._leaves[0]
        if true_state:
            assumption_rules.append(Rule(assumption, SymbolTrue))
        else:
            assumption_rules.append(Rule(assumption, SymbolFalse))
    
    pred, changed = pred.apply_rules(assumption_rules, evaluation)
    pred = pred.evaluate(evaluation)
    return pred
