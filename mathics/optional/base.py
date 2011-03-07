from mathics.builtin.base import Builtin, SageFunction

class OptionalBuiltin(Builtin):
    pass

class OptionalSageFunction(SageFunction, OptionalBuiltin):
    pass

def catching(func):
    " Decorator to catch TypeError and ValueError to print an invalidargs message "
    
    def new_func(*args, **kwargs):
        try:
            return func(*args, **kwargs)
        except (TypeError, ValueError):
            evaluation = kwargs['evaluation']
            evaluation.message(args[0].get_name(), 'invalidargs')
            return
    
    new_func.__name__ = func.__name__
    new_func.__doc__ = func.__doc__
    return new_func
            