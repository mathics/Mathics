
from  mathics.builtin.base import Builtin, Symbol, String

class MyPyTestFunction(Builtin):
    """
    MyPyTestSymbol defines a Symbol.
    """
    author = "Mauricio Matera"
    context = "MyPyTestContext`"

    attributes = ("Protected", "OneIdentity", "HoldFirst")
    def apply(self, val, expression):
        'MyPyTestSymbol[val_]'
        print(val.__repr__())
        return String("This is a PyMathics output")
    
    
    
class MyPyTestSymbol(Symbol):
    """
    MyPyTestSymbol defines a Symbol.
    """
    author = "Mauricio Matera"
    context = "MyPyTestContext`"
    rules = {'MyPyTestSymbol': '1234'}
    def __init__(self, *args, **kargs):
        super(MyPyTestSymbol,self).__init__(name="MyPyTestSymbol",*args,**kargs)        

