 # key is str: to_xxx name, value is formatter function to call
format2fn = {}
import inspect

def lookup_method(self, format: str):
    for cls in inspect.getmro(type(self)):
        format_fn = format2fn.get(("svg", cls), None)
        if format_fn is not None:
            return format_fn
    raise RuntimeError(f"Can't find formatter {format} for {type(self)}")
