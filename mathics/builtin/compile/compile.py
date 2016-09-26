
import llvmlite.binding as llvm
from llvmlite.llvmpy.core import Type
from ctypes import CFUNCTYPE

from mathics.builtin.compile.utils import llvm_to_ctype
from mathics.builtin.compile.ir import IRGenerator


# setup llvm for code generation
llvm.initialize()
llvm.initialize_native_target()
llvm.initialize_native_asmprinter()  # yes, even this one


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


engine = create_execution_engine()


def _compile(expr, args):
    ir_gen = IRGenerator(expr, args, 'mathics')
    llvm_ir, ret_type = ir_gen.generate_ir()
    mod = compile_ir(engine, llvm_ir)

    # lookup function pointer
    func_ptr = engine.get_function_address('mathics')

    # run function via ctypes
    cfunc = CFUNCTYPE(llvm_to_ctype(ret_type), *(llvm_to_ctype(arg.type) for arg in args))(func_ptr)
    return cfunc

