from .helper import session, check_evaluation
import sys
import pytest

from mathics.core.expression import (Symbol, Atom, String, Expression)
from mathics.builtin.base import BoxConstruct, AtomBuiltin, Predefined
from mathics.builtin.graphics import GRAPHICS_OPTIONS

class CustomBoxConstruct(BoxConstruct):
    def __init__(self, evaluation):
        super().__init__(evaluation=evaluation)
        self._leaves = [1,2,3]

    def boxes_to_text(self, leaves=None, **options):
        if not leaves:
            leaves = self._leaves
        return 'CustomBoxConstruct<<' + self._leaves.__str__()   + '>>'

    def boxes_to_mathml(self, leaves=None, **options):
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


class CustomGraphicsBox(BoxConstruct):
    """

    """
    options = GRAPHICS_OPTIONS
    attributes = ("HoldAll", "ReadProtected")


    def apply_box(self, elems, evaluation, options):
        """System`MakeBoxes[System`Graphics[elems_, System`OptionsPattern[System`Graphics]],
        System`StandardForm|System`TraditionalForm|System`OutputForm]"""
        instance = CustomGraphicsBox(*(elems._leaves), evaluation=evaluation)
        return instance

    def boxes_to_text(self, leaves=None, **options):
        if leaves:
           self._leaves = leaves 
        return "--custom graphics--: I should plot " + self._leaves.__str__() +" items"

    def boxes_to_tex(self, leaves=None, **options):
        return "--custom graphics--: I should plot " + self._leaves.__str__() +" items" 

    def boxes_to_mathml(self, leaves=None, **options):
        return "--custom graphics--: I should plot " + self._leaves.__str__() +" items"

    def boxes_to_svg(self, evaluation):
        return "--custom graphics--: I should plot " + self._leaves.__str__() +" items"

    



def test_custom_boxconstruct():
    defs = session.evaluation.definitions
    instance_custom_atom = CustomAtom(expression=False)
    instance_custom_atom.contribute(defs, is_pymodule=True)
    expr = session.evaluate("MakeBoxes[CustomAtom, InputForm]")
    formatted = session.format_result().boxes_to_mathml()
    assert formatted == "CustomBoxConstruct<<[1, 2, 3]>>"



def test_custom_graphicsbox_constructor():
    defs = session.evaluation.definitions
    instance_customgb_atom = CustomGraphicsBox(expression=False, evaluation=session.evaluation)
    instance_customgb_atom.contribute(defs, is_pymodule=True)
    expr = session.evaluate("MakeBoxes[Graphics[{Circle[{0,0},1]}], OutputForm]")
    formatted = session.format_result().boxes_to_mathml()
    assert formatted == "--custom graphics--: I should plot (<Expression: System`Circle[System`List[0, 0], 1]>,) items"
