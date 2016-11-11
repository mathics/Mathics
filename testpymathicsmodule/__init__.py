


from  mathics.builtin.base import Symbol, String

class MyPyTestSymbol(Symbol):
    """
    MyPyTestSymbol defines a Symbol.
    """
    author = "Mauricio Matera"
    context = "MyPyTestSymbolContext`"
    def apply(expression):
        ''MyPyTestSymbol[]''
        return String("This is a PyMathics output")

