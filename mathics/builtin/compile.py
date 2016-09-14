from functools import reduce
import itertools

from llvmlite import ir
import llvmlite.binding as llvm
import llvmlite.llvmpy.core as lc
from llvmlite.llvmpy.core import Type

from mathics.core.expression import Expression, Integer, Symbol, Real

from ctypes import c_int64, c_double, c_bool, CFUNCTYPE


class CompilationError(Exception):
    pass

# create some useful types
int_type = ir.IntType(64)
real_type = ir.DoubleType()
bool_type = ir.IntType(1)


class MathicsArg(object):
    def __init__(self, name, type):
        self.name = name
        self.type = type


def generate_ir(expr, args, func_name, known_ret_type=None):
    '''
    generates LLVM IR for a given expression
    '''
    # assume that the function returns a real. Note that this is verified by
    # looking at the type of the head of the converted expression.
    ret_type = real_type if known_ret_type is None else known_ret_type

    # create an empty module
    module = ir.Module(name=__file__)

    func_type = ir.FunctionType(ret_type, tuple(arg.type for arg in args))

    # declare a function inside the module
    func = ir.Function(module, func_type, name=func_name)

    # implement the function
    block = func.append_basic_block(name='entry')
    builder = ir.IRBuilder(block)

    lookup_args = {arg.name: func_arg for arg, func_arg in zip(args, func.args)}

    ir_code = _gen_ir(expr, lookup_args, builder)

    # if the return isn't correct then try again
    if known_ret_type is None and ir_code.type != ret_type:
        return generate_ir(expr, args, func_name, ir_code.type)

    # type was known or guessed correctly
    assert ir_code.type == ret_type

    builder.ret(ir_code)
    return str(module), ret_type


def call_fp_intr(builder, name, args):
    '''
    call a LLVM intrinsic floating-point operation
    '''
    mod = builder.module
    intr = lc.Function.intrinsic(mod, name, [arg.type for arg in args])
    return builder.call(intr, args)


def convert_args(args, builder):
    # check/convert leaf types
    if any(arg.type == real_type for arg in args):
        for i, arg in enumerate(args):
            if arg.type == int_type:
                args[i] = builder.sitofp(arg, real_type)
        ret_type = real_type
    elif all(arg.type == int_type for arg in args):
        ret_type = int_type
    elif all(arg.type == bool_type for arg in args):
        ret_type = bool_type
    else:
        raise CompilationError()
    return ret_type, args


def pairwise(args):
    '''
    [a, b, c] -> [(a, b), (b, c)]
    >>> list(pairwise([1, 2, 3]))
    [(1, 2), (2, 3)]
    '''
    first = True
    for arg in args:
        if not first:
            yield last, arg
        first = False
        last = arg


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

    if expr.has_form('If', 3):
        args = expr.get_leaves()

        # condition
        cond = _gen_ir(args[0], lookup_args, builder)
        if cond.type == int_type:
            cond = builder.icmp_signed('!=', cond, int_type(0))
        if cond.type != bool_type:
            raise CompilationError()

        # construct new blocks
        then_block = builder.append_basic_block()
        else_block = builder.append_basic_block()
        cont_block = builder.append_basic_block()

        # branch to then or else block
        builder.cbranch(cond, then_block, else_block)

        # results for both block
        with builder.goto_block(then_block):
            then_result = _gen_ir(args[1], lookup_args, builder)
        with builder.goto_block(else_block):
            else_result = _gen_ir(args[2], lookup_args, builder)

        # type check both blocks - determine resulting type
        if then_result.type == else_result.type:
            ret_type = then_result.type
        elif then_result.type == int_type and else_result.type == real_type:
            builder.position_at_end(then_block)
            then_result = builder.sitofp(then_result, real_type)
            ret_type = real_type
        elif then_result.type == real_type and else_result.type == int_type:
            builder.position_at_end(else_block)
            else_result = builder.sitofp(else_result, real_type)
            ret_type = real_type

        # both blocks branch to continuation block
        with builder.goto_block(then_block):
            builder.branch(cont_block)
        with builder.goto_block(else_block):
            builder.branch(cont_block)

        # continuation block
        builder.position_at_start(cont_block)
        result = builder.phi(ret_type)
        result.add_incoming(then_result, then_block)
        result.add_incoming(else_result, else_block)
        return result

    # generate leaves
    args = [_gen_ir(leaf, lookup_args, builder) for leaf in expr.get_leaves()]


    # check leaf types
    ret_type, args = convert_args(args, builder)

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
            sinx = call_fp_intr(builder, 'llvm.sin', args)
            cosx = call_fp_intr(builder, 'llvm.cos', args)
            return builder.fdiv(sinx, cosx)
    elif expr.has_form('Sec', 1) and ret_type == real_type:
        # FIXME this approach is inaccurate
        cosx = call_fp_intr(builder, 'llvm.cos', args)
        return builder.fdiv(real_type(1.0), cosx)
    elif expr.has_form('Csc', 1) and ret_type == real_type:
        # FIXME this approach is inaccurate
        sinx = call_fp_intr(builder, 'llvm.sin', args)
        return builder.fdiv(real_type(1.0), sinx)
    elif expr.has_form('Cot', 1) and ret_type == real_type:
        # FIXME this approach is inaccurate
        sinx = call_fp_intr(builder, 'llvm.sin', args)
        cosx = call_fp_intr(builder, 'llvm.cos', args)
        return builder.fdiv(cosx, sinx)
    elif expr.has_form('Power', 2):
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
            return reduce(lambda arg1, arg2: call_fp_intr(builder, 'llvm.minnum', [arg1, arg2]), args)
    elif expr.has_form('Max', 1, None):
        if ret_type == real_type:
            return reduce(lambda arg1, arg2: call_fp_intr(builder, 'llvm.maxnum', [arg1, arg2]), args)
    elif expr.has_form('Sinh', 1) and ret_type == real_type:
        # FIXME this approach is inaccurate
        # Sinh[x] = (Exp[x] - Exp[-x]) / 2
        a = call_fp_intr(builder, 'llvm.exp', args)
        negx = builder.fsub(real_type(0.0), args[0])
        b = call_fp_intr(builder, 'llvm.exp', [negx])
        c = builder.fsub(a, b)
        return builder.fmul(c, real_type(0.5))
    elif expr.has_form('Cosh', 1) and ret_type == real_type:
        # FIXME this approach is inaccurate
        # Cosh[x] = (Exp[x] + Exp[-x]) / 2
        a = call_fp_intr(builder, 'llvm.exp', args)
        negx = builder.fsub(real_type(0.0), args[0])
        b = call_fp_intr(builder, 'llvm.exp', [negx])
        c = builder.fadd(a, b)
        return builder.fmul(c, real_type(0.5))
    elif expr.has_form('Tanh', 1) and ret_type == real_type:
        # FIXME this approach is inaccurate
        # Tanh[x] = (Exp[x] - Exp[-x]) / (Exp[x] + Exp[-x])
        a = call_fp_intr(builder, 'llvm.exp', args)
        negx = builder.fsub(real_type(0.0), args[0])
        b = call_fp_intr(builder, 'llvm.exp', [negx])
        return builder.fdiv(builder.fsub(a, b), builder.fadd(a, b))
    elif expr.has_form('Sech', 1) and ret_type == real_type:
        # FIXME this approach is inaccurate
        # Sech[x] = 2 / (Exp[x] - Exp[-x])
        a = call_fp_intr(builder, 'llvm.exp', args)
        negx = builder.fsub(real_type(0.0), args[0])
        b = call_fp_intr(builder, 'llvm.exp', [negx])
        return builder.fdiv(real_type(2.0), builder.fadd(a, b))
    elif expr.has_form('Csch', 1) and ret_type == real_type:
        # FIXME this approach is inaccurate
        # Csch[x] = 2 / (Exp[x] + Exp[-x])
        a = call_fp_intr(builder, 'llvm.exp', args)
        negx = builder.fsub(real_type(0.0), args[0])
        b = call_fp_intr(builder, 'llvm.exp', [negx])
        return builder.fdiv(real_type(2.0), builder.fsub(a, b))
    elif expr.has_form('Coth', 1) and ret_type == real_type:
        # FIXME this approach is inaccurate
        # Coth[x] = (Exp[x] + Exp[-x]) / (Exp[x] - Exp[-x])
        a = call_fp_intr(builder, 'llvm.exp', args)
        negx = builder.fsub(real_type(0.0), args[0])
        b = call_fp_intr(builder, 'llvm.exp', [negx])
        return builder.fdiv(builder.fadd(a, b), builder.fsub(a, b))
    elif expr.has_form('Equal', 2, None):
        result = []
        for lhs, rhs in pairwise(args):
            if ret_type == real_type:
                result.append(builder.fcmp_ordered('==', lhs, rhs))
            elif ret_type == int_type:
                result.append(builder.icmp_signed('==', lhs, rhs))
            else:
                raise CompilationError()
        return reduce(builder.and_, result)
    elif expr.has_form('Unequal', 2, None):
        # Unequal[e1, e2, ... en] gives True only if none of the ei are equal.
        result = []
        for lhs, rhs in itertools.combinations(args, 2):
            if ret_type == real_type:
                result.append(builder.fcmp_ordered('!=', lhs, rhs))
            elif ret_type == int_type:
                result.append(builder.icmp_signed('!=', lhs, rhs))
            else:
                raise CompilationError()
        return reduce(builder.and_, result)
    elif expr.has_form('Less', 2, None):
        result = []
        for lhs, rhs in pairwise(args):
            if ret_type == real_type:
                result.append(builder.fcmp_ordered('<', lhs, rhs))
            elif ret_type == int_type:
                result.append(builder.icmp_signed('<', lhs, rhs))
            else:
                raise CompilationError()
        return reduce(builder.and_, result)
    elif expr.has_form('LessEqual', 2, None):
        result = []
        for lhs, rhs in pairwise(args):
            if ret_type == real_type:
                result.append(builder.fcmp_ordered('<=', lhs, rhs))
            elif ret_type == int_type:
                result.append(builder.icmp_signed('<=', lhs, rhs))
            else:
                raise CompilationError()
        return reduce(builder.and_, result)
    elif expr.has_form('Greater', 2, None):
        result = []
        for lhs, rhs in pairwise(args):
            if ret_type == real_type:
                result.append(builder.fcmp_ordered('>', lhs, rhs))
            elif ret_type == int_type:
                result.append(builder.icmp_signed('>', lhs, rhs))
            else:
                raise CompilationError()
        return reduce(builder.and_, result)
    elif expr.has_form('GreaterEqual', 2, None):
        result = []
        for lhs, rhs in pairwise(args):
            if ret_type == real_type:
                result.append(builder.fcmp_ordered('>=', lhs, rhs))
            elif ret_type == int_type:
                result.append(builder.icmp_signed('>=', lhs, rhs))
            else:
                raise CompilationError()
        return reduce(builder.and_, result)
    elif expr.has_form('And', 1, None) and ret_type == bool_type:
        return reduce(builder.and_, args)
    elif expr.has_form('Or', 1, None) and ret_type == bool_type:
        return reduce(builder.or_, args)
    elif expr.has_form('Xor', 1, None) and ret_type == bool_type:
        return reduce(builder.xor, args)
    elif expr.has_form('Not', 1) and ret_type == bool_type:
        return builder.not_(args[0])
    elif expr.has_form('BitAnd', 1, None) and ret_type == int_type:
        return reduce(builder.and_, args)
    elif expr.has_form('BitOr', 1, None) and ret_type == int_type:
        return reduce(builder.or_, args)
    elif expr.has_form('BitXor', 1, None) and ret_type == int_type:
        return reduce(builder.xor, args)
    elif expr.has_form('BitNot', 1) and ret_type == int_type:
        return builder.not_(args[0])
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
    elif t == bool_type:
        return c_bool
    else:
        raise TypeError(t)


def _compile(expr, args):
    llvm_ir, ret_type = generate_ir(expr, args, 'mathics')
    mod = compile_ir(engine, llvm_ir)

    # lookup function pointer
    func_ptr = engine.get_function_address('mathics')

    # run function via ctypes
    cfunc = CFUNCTYPE(llvm_to_ctype(ret_type), *(llvm_to_ctype(arg.type) for arg in args))(func_ptr)
    return cfunc
