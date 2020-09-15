#
"""
Pymathics TestPyMathics
This is an example of an external mathics module. It just defines a function and a symbol, in the same way is done for  builtin symbols.
"""

from mathics.builtin.base import Builtin, Symbol, String, AtomBuiltin

# To be recognized as an external mathics module, the following variable
# is required:
#
pymathics_version_data = {'author': 'Juan Mauricio Matera',
                          'version': '1.0',
                          'requires': [], }                # Maybe this field is useful.


class MyPyTestFunction(Builtin):
    """
    <dl>
    <dt>'MyPyTestFunction'[$m$]
        <dd>Just an example function in pymathics module.
    </dl>
    >> MyPyTestContext`MyPyTestFunction["anything"]
     = This is a PyMathics output
    """
    context = "MyPyTestContext`"

    attributes = ("Protected", "OneIdentity", "HoldFirst")

    def apply(self, val, evaluation):
        'MyPyTestContext`MyPyTestFunction[val_]'
        return String("This is a PyMathics output")


class MyPyTestSymbol(AtomBuiltin):
    """
    <dl>
    <dt>'MyPyTestSymbol'
        <dd>Just an example function in pymathics module.
    </dl>
    >> MyPyTestContext`MyPyTestSymbol
     = 1234
    MyPyTestSymbol defines a Symbol.
    """
    context = "MyPyTestContext`"
    rules = {'MyPyTestContext`MyPyTestSymbol': '1234'}

    def __init__(self, *args, **kargs):
        super(MyPyTestSymbol, self).__init__(name="MyPyTestSymbol", *args, **kargs)
