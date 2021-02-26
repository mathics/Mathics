from .helper import session, check_evaluation
import sys
import pytest

from mathics.core.expression import (Symbol, Atom, String, Expression)
from mathics.builtin.base import BoxConstruct, AtomBuiltin, Predefined

class CustomBoxConstruct(BoxConstruct):
    def __init__(self, evaluation):
        super().__init__(evaluation=evaluation)
        self._leaves = [1,2,3]

    def boxes_to_text(self, leaves=None, **options):
        if not leaves:
            leaves = self._leaves
        return 'CustomBoxConstruct<<' + self._leaves.__str__()   + '>>'

    def boxes_to_xml(self, leaves=None, **options):
        if not leaves:
            leaves = self._leaves
        return 'CustomBoxConstruct<<' + self._leaves.__str__()   + '>>'

    def boxes_to_tex(self, leaves=None, **options):
        if not leaves:
            leaves = self._leaves
        return 'CustomBoxConstruct<<' + int(self_leaves)   + '>>'

class CustomAtom(Predefined):
    """
    just a test
    """
    context = "System`"
    rules = {"N[System`CustomAtom]": "37", }
    
    def apply_to_boxes(self, evaluation):
        'System`MakeBoxes[System`CustomAtom, StandardForm|TraditionalForm|OutputForm|InputForm]'
        return CustomBoxConstruct(evaluation=evaluation)

def test_custom_boxconstruct():
    defs = session.evaluation.definitions
    instance_custom_atom = CustomAtom(expression=False)
    instance_custom_atom.contribute(defs, is_pymodule=True)
    expr = session.evaluate("MakeBoxes[CustomAtom, InputForm]")
    formatted = session.format_result().boxes_to_xml()
    assert formatted == "CustomBoxConstruct<<[1, 2, 3]>>"

