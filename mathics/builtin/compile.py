from llvmlite import ir
import llvmlite.binding as llvm

from mathics.core.expression import Expression, Integer, Symbol, Real

from ctypes import c_int64, c_double, CFUNCTYPE


class CompilationError(Exception):
    pass

# create some useful types
int_type = ir.IntType(64)
real_type = ir.DoubleType()
ifunc2_type = ir.FunctionType(int_type, (int_type, int_type))


class MathicsArg(object):
    def __init__(self, name, type):
        self.name = name
        self.type = type


def generate_ir(expr, args, func_name, ret_type):
    '''
    generates LLVM IR for a given expression
    '''
    # create an empty module
    module = ir.Module(name=__file__)

    # infer function type from args and ret_type
    func_type = ir.FunctionType(ret_type, tuple(arg.type for arg in args))

    # declare a function inside the module
    func = ir.Function(module, func_type, name=func_name)

    # declare a function inside it
    # func = ir.Function(module, ifunc2_type, name=func_name)

    # implement the function
    block = func.append_basic_block(name='entry')
    builder = ir.IRBuilder(block)

    lookup_args = {arg.name: func_arg for arg, func_arg in zip(args, func.args)}

    builder.ret(_gen_ir(expr, lookup_args, builder))
    return str(module)


def _gen_ir(expr, lookup_args, builder):
    '''
    walks an expression tree and constructs the ir block
    '''
    if isinstance(expr, Symbol):
        arg = lookup_args[expr.get_name()]
        return arg
    elif isinstance(expr, Integer):
        return int_type(expr.get_int_value())
    elif isinstance(expr, Real):
        return real_type(expr.round_to_float())
    elif expr.has_form('Plus', 2):
        a, b = [_gen_ir(leaf, lookup_args, builder) for leaf in expr.get_leaves()]
        if a.type == int_type and b.type == int_type:
            return builder.add(a, b)
        elif a.type == int_type and b.type == real_type:
            v = builder.sitofp(a, real_type)
            return builder.fadd(v, b)
        elif a.type == real_type and b.type == int_type:
            v = builder.sitofp(b, real_type)
            return builder.fadd(a, v)
        elif a.type == real_type and b.type == real_type:
            return builder.fadd(a, b)
        else:
            raise CompilationError
    else:
        raise CompilationError


def create_execution_engine():
    """
    Create an ExecutionEngine suitable for JIT code generation on
    the host CPU.  The engine is reusable for an arbitrary number of
    modules.
    """
    # Create a target machine representing the host
    target = llvm.Target.from_default_triple()
    target_machine = target.create_target_machine()
    # And an execution engine with an empty backing module
    backing_mod = llvm.parse_assembly("")
    engine = llvm.create_mcjit_compiler(backing_mod, target_machine)
    return engine


def compile_ir(engine, llvm_ir):
    """
    Compile the LLVM IR string with the given engine.
    The compiled module object is returned.
    """
    # Create a LLVM module object from the IR
    mod = llvm.parse_assembly(llvm_ir)
    mod.verify()
    # Now add the module and make sure it is ready for execution
    engine.add_module(mod)
    engine.finalize_object()
    return mod


# setup llvm for code generation
llvm.initialize()
llvm.initialize_native_target()
llvm.initialize_native_asmprinter()  # yes, even this one

engine = create_execution_engine()


def llvm_to_ctype(t):
    'converts llvm types to ctypes'
    if t == int_type:
        return c_int64
    elif t == real_type:
        return c_double


def _compile(expr, args, ret_type):
    llvm_ir = generate_ir(expr, args, 'plus', ret_type)
    # print(llvm_ir)
    mod = compile_ir(engine, llvm_ir)

    # lookup function pointer
    func_ptr = engine.get_function_address('plus')

    # run function via ctypes
    cfunc = CFUNCTYPE(llvm_to_ctype(ret_type), *(llvm_to_ctype(arg.type) for arg in args))(func_ptr)
    return cfunc
