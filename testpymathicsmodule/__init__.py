
from  mathics.builtin.base import Builtin, Symbol, String, AtomBuiltin

class MyPyTestFunction(Builtin):
    """
    MyPyTestFunction defines a Symbol.
    """
    author = "Mauricio Matera"
    context = "MyPyTestContext`"

    attributes = ("Protected", "OneIdentity", "HoldFirst")
    def apply(self, val, evaluation):
        'MyPyTestContext`MyPyTestFunction[val_]'
        return String("This is a PyMathics output")
    
    
    
class MyPyTestSymbol(AtomBuiltin):
    """
    MyPyTestSymbol defines a Symbol.
    """
    author = "Mauricio Matera"
    context = "MyPyTestContext`"
    rules = {'MyPyTestContext`MyPyTestSymbol': '1234'}
    def __init__(self, *args, **kargs):
        super(MyPyTestSymbol,self).__init__(name="MyPyTestSymbol",*args,**kargs)        

