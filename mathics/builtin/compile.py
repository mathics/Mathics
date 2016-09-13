from functools import reduce

from llvmlite import ir
import llvmlite.binding as llvm
import llvmlite.llvmpy.core as lc
from llvmlite.llvmpy.core import Type

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


def check_type(arg):
    if arg.type not in (int_type, double_type):
        raise CompilationError()


def call_fp_intr(builder, name, args):
    '''
    call a LLVM intrinsic floating-point operation
    '''
    mod = builder.module
    intr = lc.Function.intrinsic(mod, name, [arg.type for arg in args])
    return builder.call(intr, args)


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
    elif not isinstance(expr, Expression):
        raise CompilationError()

    # generate leaves
    args = [_gen_ir(leaf, lookup_args, builder) for leaf in expr.get_leaves()]

    # check leaf types
    if any(arg.type == real_type for arg in args):
        for i, arg in enumerate(args):
            if arg.type == int_type:
                args[i] = builder.sitofp(arg, real_type)
        ret_type = real_type
    elif all(arg.type == int_type for arg in args):
        ret_type = int_type
    else:
        raise CompilationError()

    # convert expression
    if expr.has_form('Plus', 1, None):
        if ret_type == real_type:
            return reduce(builder.fadd, args)
        elif ret_type == int_type:
            return reduce(builder.add, args)
    elif expr.has_form('Times', 1, None):
        if ret_type == real_type:
            return reduce(builder.fmul, args)
        elif ret_type == int_type:
            return reduce(builder.mul, args)
    elif expr.has_form('Sin', 1):
        if ret_type == real_type:
            return call_fp_intr(builder, 'llvm.sin', args)
    elif expr.has_form('Cos', 1):
        if ret_type == real_type:
            return call_fp_intr(builder, 'llvm.cos', args)
    elif expr.has_form('Tan', 1):
        if ret_type == real_type:
            # FIXME this approach is inaccurate
            a = call_fp_intr(builder, 'llvm.sin', args)
            b = call_fp_intr(builder, 'llvm.cos', args)
            return builder.fdiv(a, b)
    elif expr.has_form('Power', 2):
        # FIXME unknown intrinsic
        # TODO llvm.powi if second argument is integer
        # TODO llvm.exp if first argument is E
        # TODO llvm.exp2 if first argument is 2
        if ret_type == real_type:
            return call_fp_intr(builder, 'llvm.pow', args)
    elif expr.has_form('Exp', 1):
        if ret_type == real_type:
            return call_fp_intr(builder, 'llvm.exp', args)
    elif expr.has_form('Log', 1):
        # TODO log2 and log10 special cases
        if ret_type == real_type:
            return call_fp_intr(builder, 'llvm.log', args)
    elif expr.has_form('Abs', 1):
        if ret_type == real_type:
            return call_fp_intr(builder, 'llvm.fabs', args)
    elif expr.has_form('Min', 1, None):
        if ret_type == real_type:
            # FIXME unknown intrinsic
            return reduce(lambda arg1, arg2: call_fp_intr(builder, 'llvm.minnum', [arg1, arg2]), args)
    elif expr.has_form('Max', 1, None):
        if ret_type == real_type:
            # FIXME unknown intrinsic
            return reduce(lambda arg1, arg2: call_fp_intr(builder, 'llvm.maxnum', [arg1, arg2]), args)
    raise CompilationError()


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


def infer_return_type(expr, args):
    if all(arg.type == int_type for arg in args):
        return int_type
    else:
        return real_type


def _compile(expr, args):
    ret_type = infer_return_type(expr, args)

    llvm_ir = generate_ir(expr, args, 'mathics', ret_type)
    mod = compile_ir(engine, llvm_ir)

    # lookup function pointer
    func_ptr = engine.get_function_address('mathics')

    # run function via ctypes
    cfunc = CFUNCTYPE(llvm_to_ctype(ret_type), *(llvm_to_ctype(arg.type) for arg in args))(func_ptr)
    return cfunc
