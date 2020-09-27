from functools import reduce
import itertools

from llvmlite import ir
import ctypes

from mathics.core.expression import Expression, Integer, Symbol, Real, String
from mathics.builtin.compile.types import int_type, real_type, bool_type, void_type
from mathics.builtin.compile.utils import pairwise, llvm_to_ctype
from mathics.builtin.compile.base import CompileError


def single_real_arg(f):
    '''
    One real argument.
    Converts integer argument to real argument.
    '''
    def wrapped_f(self, expr):
        leaves = expr.get_leaves()
        if len(leaves) != 1:
            raise CompileError()
        arg = self._gen_ir(leaves[0])
        if arg.type == void_type:
            return arg
        elif arg.type == int_type:
            arg = self.int_to_real(arg)
        return f(self, [arg])
    return wrapped_f


def int_real_args(minargs):
    '''
    Many real or integer arguments expected.
    If any real arguments are provided all integer arguments will be converted.
    '''
    def wraps(f):
        def wrapped_f(self, expr):
            leaves = expr.get_leaves()
            if len(leaves) < minargs:
                raise CompileError()
            args = [self._gen_ir(leaf) for leaf in leaves]
            for arg in args:
                if arg.type == void_type:
                    return arg
            if any(arg.type not in (real_type, int_type) for arg in args):
                raise CompileError()
            if all(arg.type == int_type for arg in args):
                ret_type = int_type
            else:
                ret_type = real_type
                for i, arg in enumerate(args):
                    if arg.type == int_type:
                        args[i] = self.int_to_real(arg)
            return f(self, args, ret_type)
        return wrapped_f
    return wraps


def int_args(f):
    '''
    Integer arguments.
    Converts boolean to integer arguments.
    '''
    def wrapped_f(self, expr):
        leaves = expr.get_leaves()
        args = [self._gen_ir(leaf) for leaf in leaves]
        for arg in args:
            if arg.type == void_type:
                return arg
        for i, arg in enumerate(args):
            if arg.type == bool_type:
                args[i] = self.bool_to_int(arg)
        if any(arg.type != int_type for arg in args):
            raise CompileError()
        return f(self, args)
    return wrapped_f


def bool_args(f):
    '''
    Boolean arguments.
    Converts integer to boolean arguments.
    '''
    def wrapped_f(self, expr):
        leaves = expr.get_leaves()
        args = [self._gen_ir(leaf) for leaf in leaves]
        for arg in args:
            if arg.type == void_type:
                return arg
        for i, arg in enumerate(args):
            if arg.type == int_type:
                args[i] = self.int_to_bool(arg)
        if any(arg.type != bool_type for arg in args):
            raise CompileError()
        return f(self, args)
    return wrapped_f


class IRGenerator(object):
    def __init__(self, expr, args, func_name):
        self.expr = expr
        self.args = args
        self.func_name = func_name      # function name of entry point
        self.builder = None
        self._known_ret_type = None
        self._returned_type = None
        self.lookup_args = None

    def generate_ir(self):
        '''
        generates LLVM IR for a given expression
        '''
        # assume that the function returns a real. Note that this is verified by
        # looking at the type of the head of the converted expression.
        ret_type = real_type if self._known_ret_type is None else self._known_ret_type

        # create an empty module
        module = ir.Module(name=__file__)

        func_type = ir.FunctionType(ret_type, tuple(arg.type for arg in self.args))

        # declare a function inside the module
        func = ir.Function(module, func_type, name=self.func_name)

        # implement the function
        block = func.append_basic_block(name='entry')
        self.builder = ir.IRBuilder(block)

        self.lookup_args = {arg.name: func_arg for arg, func_arg in zip(self.args, func.args)}

        ir_code = self._gen_ir(self.expr)

        # if the return type isn't correct then try again
        if self._known_ret_type is None:
            # determine the type returned
            if ir_code.type == void_type:
                if self._returned_type is not None:
                    # we returned something so use that type
                    self._known_ret_type = self._returned_type
                    # force generation again in case multiple returns of different types
                    return self.generate_ir()
                else:
                    # actually returned void e.g. Print[]
                    pass

            if ir_code.type != ret_type:
                # guessed incorrectly - try again
                self._known_ret_type = ir_code.type
                return self.generate_ir()

        # void handles its own returns
        if ir_code.type != void_type:
            self.builder.ret(ir_code)

        return str(module), ret_type

    def call_fp_intr(self, name, args, ret_type=real_type):
        '''
        call a LLVM intrinsic floating-point operation
        '''
        # see https://github.com/numba/llvmlite/pull/205 for an explanation of declare_intrinsic
        mod = self.builder.module
        fullname = name + '.' + ret_type.intrinsic_name
        fnty = ir.FunctionType(ret_type, [arg.type for arg in args])
        intr = mod.declare_intrinsic(fullname, fnty=fnty)
        return self.builder.call(intr, args)

    def int_to_real(self, arg):
        assert arg.type == int_type
        return self.builder.sitofp(arg, real_type)

    def int_to_bool(self, arg):
        assert arg.type == int_type
        # any non-zero int is true
        return self.builder.icmp_signed('!=', arg, int_type(0))

    def bool_to_int(self, arg):
        assert arg.type == bool_type
        return self.builder.zext(arg)

    def add_caller(self, py_f, ret_type, args):
        '''
        Inserts a caller to a python function
        '''
        # see http://eli.thegreenplace.net/2015/calling-back-into-python-from-llvmlite-jited-code/

        c_func_type = ctypes.CFUNCTYPE(llvm_to_ctype(ret_type), *(llvm_to_ctype(arg.type) for arg in args))
        c_func = c_func_type(py_f)
        c_func_addr = ctypes.cast(c_func, ctypes.c_void_p).value

        addrcaller_func_type = ir.FunctionType(ret_type, [arg.type for arg in args])
        cb_func_ptr_type = addrcaller_func_type.as_pointer()

        f = self.builder.inttoptr(int_type(c_func_addr), cb_func_ptr_type, name='f')
        call = self.builder.call(f, args)
        if call.type == void_type:
            return self.builder.ret_void()
        return call

    def _gen_ir(self, expr):
        '''
        walks an expression tree and constructs the ir block
        '''
        if isinstance(expr, Symbol):
            try:
                arg = self.lookup_args[expr.get_name()]
            except KeyError:
                raise CompileError()
            return arg
        elif isinstance(expr, Integer):
            return int_type(expr.get_int_value())
        elif isinstance(expr, Real):
            return real_type(expr.round_to_float())
        elif not isinstance(expr, Expression):
            raise CompileError()

        head_name = expr.get_head_name()
        if head_name.startswith('System`'):
            head_name = head_name[7:]
            method = getattr(self, '_gen_' + head_name, None)
        else:
            method = None

        if method is None:
            raise CompileError()

        return method(expr)

    def _gen_If(self, expr):
        if not expr.has_form('If', 3):
            raise CompileError()

        builder = self.builder
        args = expr.get_leaves()

        # condition
        cond = self._gen_ir(args[0])
        if cond.type == int_type:
            cond = self.int_to_bool(cond)
        if cond.type != bool_type:
            raise CompileError()

        # construct new blocks
        then_block = builder.append_basic_block()
        else_block = builder.append_basic_block()

        # branch to then or else block
        builder.cbranch(cond, then_block, else_block)

        # results for both block
        with builder.goto_block(then_block):
            then_result = self._gen_ir(args[1])
        with builder.goto_block(else_block):
            else_result = self._gen_ir(args[2])

        # type check both blocks - determine resulting type
        if then_result.type == void_type and else_result.type == void_type:
            # both blocks terminate so no continuation block
            return then_result
        elif then_result.type == else_result.type:
            ret_type = then_result.type
        elif then_result.type == int_type and else_result.type == real_type:
            builder.position_at_end(then_block)
            then_result = self.int_to_real(then_result)
            ret_type = real_type
        elif then_result.type == real_type and else_result.type == int_type:
            builder.position_at_end(else_block)
            else_result = self.int_to_real(else_result)
            ret_type = real_type
        elif then_result.type == void_type and else_result.type != void_type:
            ret_type = else_result.type
        elif then_result.type != void_type and else_result.type == void_type:
            ret_type = then_result.type
        else:
            raise CompileError()

        # continuation block
        cont_block = builder.append_basic_block()
        builder.position_at_start(cont_block)
        result = builder.phi(ret_type)

        # both blocks branch to continuation block (unless they terminate)
        if then_result.type != void_type:
            with builder.goto_block(then_block):
                builder.branch(cont_block)
            result.add_incoming(then_result, then_block)
        if else_result.type != void_type:
            with builder.goto_block(else_block):
                builder.branch(cont_block)
            result.add_incoming(else_result, else_block)
        return result

    def _gen_Return(self, expr):
        leaves = expr.get_leaves()
        if len(leaves) != 1:
            raise CompileError()

        arg = self._gen_ir(leaves[0])
        if arg.type == void_type:
            return arg

        if self._returned_type == arg.type:
            pass
        elif self._returned_type is None:
            self._returned_type = arg.type
        elif self._returned_type == real_type and arg.type == int_type:
            arg = self.int_to_real(arg)
        elif self._returned_type == int_type and arg.type == real_type:
            self._returned_type = arg.type
        else:
            raise CompileError('Conflicting return types {} and {}.'.format(self._returned_type, arg.type))
        return self.builder.ret(arg)

    @int_real_args(1)
    def _gen_Plus(self, args, ret_type):
        if ret_type == real_type:
            return reduce(self.builder.fadd, args)
        elif ret_type == int_type:
            return reduce(self.builder.add, args)

    @int_real_args(1)
    def _gen_Times(self, args, ret_type):
        if ret_type == real_type:
            return reduce(self.builder.fmul, args)
        elif ret_type == int_type:
            return reduce(self.builder.mul, args)

    def _gen_Power(self, expr):
        # TODO (int_type, int_type) power
        leaves = expr.get_leaves()
        if len(leaves) != 2:
            raise CompileError()

        # convert exponent
        exponent = self._gen_ir(leaves[1])
        if exponent.type == int_type:
            exponent = self.int_to_real(exponent)
        elif exponent.type == void_type:
            return exponent

        # E ^ exponent
        if leaves[0].same(Symbol('E')) and exponent.type == real_type:
            return self.call_fp_intr('llvm.exp', [exponent])

        # 2 ^ exponent
        if leaves[0].get_int_value() == 2 and exponent.type == real_type:
            return self.call_fp_intr('llvm.exp2', [exponent])

        # convert base
        base =  self._gen_ir(leaves[0])
        if base.type == int_type:
            base = self.int_to_real(base)
        elif base.type == void_type:
            return base

        # base ^ exponent
        if base.type == real_type and exponent.type == real_type:
            return self.call_fp_intr('llvm.pow', [base, exponent])
        else:
            raise CompileError()

    @single_real_arg
    def _gen_Sin(self, args):
        return self.call_fp_intr('llvm.sin', args)

    @single_real_arg
    def _gen_Cos(self, args):
        return self.call_fp_intr('llvm.cos', args)

    @single_real_arg
    def _gen_Tan(self, args):
        # FIXME this approach is inaccurate
        sinx = self.call_fp_intr('llvm.sin', args)
        cosx = self.call_fp_intr('llvm.cos', args)
        return self.builder.fdiv(sinx, cosx)

    @single_real_arg
    def _gen_Sec(self, args):
        # FIXME this approach is inaccurate
        cosx = self.call_fp_intr('llvm.cos', args)
        return self.builder.fdiv(real_type(1.0), cosx)

    @single_real_arg
    def _gen_Csc(self, args):
        # FIXME this approach is inaccurate
        sinx = self.call_fp_intr('llvm.sin', args)
        return self.builder.fdiv(real_type(1.0), sinx)

    @single_real_arg
    def _gen_Cot(self, args):
        # FIXME this approach is inaccurate
        sinx = self.call_fp_intr('llvm.sin', args)
        cosx = self.call_fp_intr('llvm.cos', args)
        return self.builder.fdiv(cosx, sinx)

    @single_real_arg
    def _gen_Exp(self, args):
        return self.call_fp_intr('llvm.exp', args)

    @single_real_arg
    def _gen_Log(self, args):
        return self.call_fp_intr('llvm.log', args)

    @int_real_args(1)
    def _gen_Abs(self, args, ret_type):
        if len(args) != 1:
            raise CompileError()
        arg = args[0]
        if ret_type == int_type:
            # FIXME better way to do this?
            neg_arg = self.builder.mul(arg, int_type(-1))
            cond = self.builder.icmp_signed('<', arg, int_type(0))
            return self.builder.select(cond, neg_arg, arg)
        elif ret_type == real_type:
            return self.call_fp_intr('llvm.fabs', args)

    @int_real_args(1)
    def _gen_Min(self, args, ret_type):
        if ret_type == int_type:
            # FIXME better way to do this?
            return reduce(lambda arg1, arg2: self.builder.select(
                self.builder.icmp_signed('<', arg1, arg2), arg1, arg2), args)
        elif ret_type == real_type:
            return reduce(lambda arg1, arg2: self.call_fp_intr('llvm.minnum', [arg1, arg2]), args)

    @int_real_args(1)
    def _gen_Max(self, args, ret_type):
        if ret_type == int_type:
            # FIXME better way to do this?
            return reduce(lambda arg1, arg2: self.builder.select(
                self.builder.icmp_signed('>', arg1, arg2), arg1, arg2), args)
        elif ret_type == real_type:
            return reduce(lambda arg1, arg2: self.call_fp_intr('llvm.maxnum', [arg1, arg2]), args)

    @single_real_arg
    def _gen_Sinh(self, args):
        # FIXME this approach is inaccurate
        # Sinh[x] = (Exp[x] - Exp[-x]) / 2
        a = self.call_fp_intr('llvm.exp', args)
        negx = self.builder.fsub(real_type(0.0), args[0])
        b = self.call_fp_intr('llvm.exp', [negx])
        c = self.builder.fsub(a, b)
        return self.builder.fmul(c, real_type(0.5))

    @single_real_arg
    def _gen_Cosh(self, args):
        # FIXME this approach is inaccurate
        # Cosh[x] = (Exp[x] + Exp[-x]) / 2
        a = self.call_fp_intr('llvm.exp', args)
        negx = self.builder.fsub(real_type(0.0), args[0])
        b = self.call_fp_intr('llvm.exp', [negx])
        c = self.builder.fadd(a, b)
        return self.builder.fmul(c, real_type(0.5))

    @single_real_arg
    def _gen_Tanh(self, args):
        # FIXME this approach is inaccurate
        # Tanh[x] = (Exp[x] - Exp[-x]) / (Exp[x] + Exp[-x])
        a = self.call_fp_intr('llvm.exp', args)
        negx = self.builder.fsub(real_type(0.0), args[0])
        b = self.call_fp_intr('llvm.exp', [negx])
        return self.builder.fdiv(self.builder.fsub(a, b), self.builder.fadd(a, b))

    @single_real_arg
    def _gen_Sech(self, args):
        # FIXME this approach is inaccurate
        # Sech[x] = 2 / (Exp[x] - Exp[-x])
        a = self.call_fp_intr('llvm.exp', args)
        negx = self.builder.fsub(real_type(0.0), args[0])
        b = self.call_fp_intr('llvm.exp', [negx])
        return self.builder.fdiv(real_type(2.0), self.builder.fadd(a, b))

    @single_real_arg
    def _gen_Csch(self, args):
        # FIXME this approach is inaccurate
        # Csch[x] = 2 / (Exp[x] + Exp[-x])
        a = self.call_fp_intr('llvm.exp', args)
        negx = self.builder.fsub(real_type(0.0), args[0])
        b = self.call_fp_intr('llvm.exp', [negx])
        return self.builder.fdiv(real_type(2.0), self.builder.fsub(a, b))

    @single_real_arg
    def _gen_Coth(self, args):
        # FIXME this approach is inaccurate
        # Coth[x] = (Exp[x] + Exp[-x]) / (Exp[x] - Exp[-x])
        a = self.call_fp_intr('llvm.exp', args)
        negx = self.builder.fsub(real_type(0.0), args[0])
        b = self.call_fp_intr('llvm.exp', [negx])
        return self.builder.fdiv(self.builder.fadd(a, b), self.builder.fsub(a, b))

    @int_real_args(2)
    def _gen_Equal(self, args, ret_type):
        result = []
        for lhs, rhs in pairwise(args):
            if ret_type == real_type:
                result.append(self.builder.fcmp_ordered('==', lhs, rhs))
            elif ret_type == int_type:
                result.append(self.builder.icmp_signed('==', lhs, rhs))
            else:
                raise CompileError()
        return reduce(self.builder.and_, result)

    @int_real_args(2)
    def _gen_Unequal(self, args, ret_type):
        result = []
        # Unequal[e1, e2, ... en] gives True only if none of the ei are equal.
        result = []
        for lhs, rhs in itertools.combinations(args, 2):
            if ret_type == real_type:
                result.append(self.builder.fcmp_ordered('!=', lhs, rhs))
            elif ret_type == int_type:
                result.append(self.builder.icmp_signed('!=', lhs, rhs))
            else:
                raise CompileError()
        return reduce(self.builder.and_, result)

    @int_real_args(2)
    def _gen_Less(self, args, ret_type):
        result = []
        for lhs, rhs in pairwise(args):
            if ret_type == real_type:
                result.append(self.builder.fcmp_ordered('<', lhs, rhs))
            elif ret_type == int_type:
                result.append(self.builder.icmp_signed('<', lhs, rhs))
            else:
                raise CompileError()
        return reduce(self.builder.and_, result)

    @int_real_args(2)
    def _gen_LessEqual(self, args, ret_type):
        result = []
        for lhs, rhs in pairwise(args):
            if ret_type == real_type:
                result.append(self.builder.fcmp_ordered('<=', lhs, rhs))
            elif ret_type == int_type:
                result.append(self.builder.icmp_signed('<=', lhs, rhs))
            else:
                raise CompileError()
        return reduce(self.builder.and_, result)

    @int_real_args(2)
    def _gen_Greater(self, args, ret_type):
        result = []
        for lhs, rhs in pairwise(args):
            if ret_type == real_type:
                result.append(self.builder.fcmp_ordered('>', lhs, rhs))
            elif ret_type == int_type:
                result.append(self.builder.icmp_signed('>', lhs, rhs))
            else:
                raise CompileError()
        return reduce(self.builder.and_, result)

    @int_real_args(2)
    def _gen_GreaterEqual(self, args, ret_type):
        result = []
        for lhs, rhs in pairwise(args):
            if ret_type == real_type:
                result.append(self.builder.fcmp_ordered('>=', lhs, rhs))
            elif ret_type == int_type:
                result.append(self.builder.icmp_signed('>=', lhs, rhs))
            else:
                raise CompileError()
        return reduce(self.builder.and_, result)

    @bool_args
    def _gen_And(self, args):
        return reduce(self.builder.and_, args)

    @bool_args
    def _gen_Or(self, args):
        return reduce(self.builder.or_, args)

    @bool_args
    def _gen_Xor(self, args):
        return reduce(self.builder.xor, args)

    @bool_args
    def _gen_Not(self, args):
        if len(args) != 1:
            raise CompileError
        return self.builder.not_(args[0])

    @int_args
    def _gen_BitAnd(self, args):
        return reduce(self.builder.and_, args)

    @int_args
    def _gen_BitOr(self, args):
        return reduce(self.builder.or_, args)

    @int_args
    def _gen_BitXor(self, args):
        return reduce(self.builder.xor, args)

    @int_args
    def _gen_BitNot(self, args):
        return self.builder.not_(args[0])
