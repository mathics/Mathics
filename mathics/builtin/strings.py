# -*- coding: utf8 -*-

"""
String functions
"""

from mathics.builtin.base import BinaryOperator, Builtin
from mathics.core.expression import Expression, Symbol, String, Integer

class StringJoin(BinaryOperator):
    """
    >> StringJoin["a", "b", "c"]
     = abc
    >> "a" <> "b" <> "c" // InputForm
     = "abc"
     
    'StringJoin' flattens lists out:
    >> StringJoin[{"a", "b"}] // InputForm
     = "ab"
    >> Print[StringJoin[{"Hello", " ", {"world"}}, "!"]]
     | Hello world!
    """
    
    operator = '<>'
    precedence = 600
    attributes = ('Flat', 'OneIdentity')
    
    def apply(self, items, evaluation):
        'StringJoin[items___]'
        
        result = ''
        items = items.flatten(Symbol('List'))
        if items.get_head_name() == 'List':
            items = items.leaves
        else:
            items = items.get_sequence()
        for item in items:
            if not isinstance(item, String):
                evaluation.message('StringJoin', 'string')
                return
            result += item.value
        return String(result)
    
class StringLength(Builtin):
    """
    'StringLength' gives the length of a string.
    >> StringLength["abc"]
     = 3
    'StringLength' is listable:
    >> StringLength[{"a", "bc"}]
     = {1, 2}
    
    >> StringLength[x]
     : String expected.
     = StringLength[x]
    """
    
    attributes = ('Listable',)
    
    def apply(self, str, evaluation):
        'StringLength[str_]'
        
        if not isinstance(str, String):
            evaluation.message('StringLength', 'string')
            return
        return Integer(len(str.value))
    
class Characters(Builtin):
    """
    >> Characters["abc"]
     = {a, b, c}
    """
    
    attributes = ('Listable',)
    
    def apply(self, string, evaluation):
        'Characters[string_String]'
        
        return Expression('List', *(String(c) for c in string.value))
    
class CharacterRange(Builtin):
    """
    >> CharacterRange["a", "e"]
     = {a, b, c, d, e}
    >> CharacterRange["b", "a"]
     = {}
    """
    
    attributes = ('ReadProtected',)
    
    messages = {
        'argtype': "Arguments `1` and `2` are not both strings of length 1.",
    }
    
    def apply(self, start, stop, evaluation):
        'CharacterRange[start_String, stop_String]'
        
        if len(start.value) != 1 or len(stop.value) != 1:
            evaluation.message('CharacterRange', 'argtype', start, stop)
            return
        start = ord(start.value[0])
        stop = ord(stop.value[0])
        return Expression('List', *(String(unichr(code)) for code in range(start, stop + 1)))
    
class String_(Builtin):
    """
    'String' is the head of strings.
    >> Head["abc"]
     = String
    >> "abc"
     = abc
    Use 'InputForm' to display quotes around strings:
    >> InputForm["abc"]
     = "abc"
    'FullForm' also displays quotes:
    >> FullForm["abc" + 2]
     = Plus[2, "abc"]
    """
    
    name = 'String'
    
class ToString(Builtin):
    """
    >> ToString[2]
     = 2
    >> ToString[2] // InputForm
     = "2"
    >> ToString[a+b]
     = a + b
    >> "U" <> 2
     : String expected.
     = U <> 2
    >> "U" <> ToString[2]
     = U2
    """
    
    def apply(self, value, evaluation):
        'ToString[value_]'
        
        text = value.format(evaluation, 'OutputForm').boxes_to_text(evaluation=evaluation)
        return String(text)
    
            
class StringQ(Builtin):
    """
    <dl>
    <dt> StringQ[$expr$]
      <dd>returns 'True' if $expr$ is a string or false otherwise.
    </dl>


    >> StringQ["abc"]
     = True

    >> Select[{"12", 1, 3, 5, "yz", x, y}, StringQ]
     = {12, yz}
    """

    def apply(self, expr, evaluation):
        'StringQ[expr_]'

        def test(expr):
            return expr.get_head_name() == 'String'

        return Symbol('True') if test(expr) else Symbol('False')


