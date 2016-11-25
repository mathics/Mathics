#
"""
This is an example of an external mathics module. It just defines a function and a symbol, in the same way is done for  builtin symbols.
"""

import six

# To be recognized as an external mathics module, the following variable
# is required:
#
pymathics_version_data = {'author': 'Juan Mauricio Matera',
                          'version': '1.0',
                          'requires': [],  # Maybe this field is useful.
                         }

from  mathics.builtin.base import Builtin, Symbol, String, AtomBuiltin


class MyPyTestFunction(Builtin):
    """
    MyPyTestFunction defines a Symbol.
    """
    context = "MyPyTestContext`"

    attributes = ("Protected", "OneIdentity", "HoldFirst")
    def apply(self, val, evaluation):
        'MyPyTestContext`MyPyTestFunction[val_]'
        return String("This is a PyMathics output")
    
    
    
class MyPyTestSymbol(AtomBuiltin):
    """
    MyPyTestSymbol defines a Symbol.
    """
    context = "MyPyTestContext`"
    rules = {'MyPyTestContext`MyPyTestSymbol': '1234'}
    def __init__(self, *args, **kargs):
        super(MyPyTestSymbol,self).__init__(name="MyPyTestSymbol",*args,**kargs)        

