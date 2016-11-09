
import six
import base64


from mathics.builtin.base import (Builtin, AtomBuiltin, Test, BoxConstruct, String)
from mathics.core.expression import (Atom, Expression, Integer, Rational, Real, MachineReal, Symbol, from_python)


class B64Encode(Builtin):
    """
    <dl>
    <dt> 'BEncode[$expr$]'
    <dd>Encodes $expr$ in Base64 coding
    </dl>

    >> B64Encode["Hello world"]
     = ""
    >> B64Encode[Integrate[f[x],{x,0,2}]]
     = ""
    >> B64Encode[RandomImage[0.,{10,10}]]
     = ...
    """

    def apply(self, expr, evaluation):
        'B64Encode[expr_]'
        if isinstance(expr,String):
            stringtocodify = "\"" + expr.get_string_value() + "\""
        else:
            stringtocodify = Expression('ToString',expr).evaluate(evaluation).get_string_value()
        return String(base64.b64encode(bytearray(stringtocodify, 'utf8')).decode('utf8'))
            



class B64Decode(Builtin):
    """
    <dl>
    <dt> 'BDecode[$string$]'
    <dd>Decode  $string$ in Base64 coding to an expression.
    </dl>

    >> B64Decode["Hello world"]
     = ""
    >> B64Decode[Integrate[f[x],{x,0,2}]]
     = ""
    >> B64Decode[RandomImage[0.,{10,10}]]
     = ...
    """


    def apply(self, expr, evaluation):
        'B64Decode[expr_String]'
        clearstring = base64.b64decode(bytearray(expr.get_string_value(), 'utf8')).decode('utf8')
        return Expression('ToExpression', String(str(clearstring,))).evaluate(evaluation)




