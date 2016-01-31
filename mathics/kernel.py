from ipykernel.kernelbase import Kernel

from mathics.core.definitions import Definitions
from mathics.core.evaluation import Evaluation
from mathics.core.expression import Integer
from mathics import settings
from mathics.version import __version__


class MathicsKernel(Kernel):
    implementation = 'Mathics'
    implementation_version = __version__
    language = 'Wolfram'
    language_version = '0.1'    # TODO
    language_info = {'mimetype': 'text/plain'}
    banner = "Mathics kernel"   # TODO

    def __init__(self, **kwargs):
        Kernel.__init__(self, **kwargs)
        self.definitions = Definitions(add_builtin=True)
        self.definitions.set_ownvalue('$Line', Integer(0))  # Reset the line number

    def do_execute(self, code, silent, store_history=True, user_expressions=None,
                   allow_stdin=False):
        if not silent:

            def out_callback(out):
                print(out)      # TODO - self.send_xxx()

            # TODO update user definitions

            evaluation = Evaluation(code, self.definitions, timeout=settings.TIMEOUT,
                                    out_callback=out_callback)

            for result in evaluation.results:
                if result is not None:
                    stream_content = {'name': 'stdout', 'text': result.result}
                    self.send_response(self.iopub_socket, 'stream', stream_content)

        return {'status': 'ok',
                # The base class increments the execution count
                'execution_count': self.execution_count,
                'payload': [],
                'user_expressions': {},
               }

    @staticmethod
    def do_is_complete(code):
        code = code.rstrip()

        trailing_ops = ['+', '-', '/', '*', '^', '=', '>', '<', '/;', '/:',
                        '/.', '&&', '||']
        if any(code.endswith(op) for op in trailing_ops):
            return {'status': 'incomplete', 'indent': ''}

        brackets = [('(', ')'), ('[', ']'), ('{', '}')]
        kStart, kEnd, stack = 0, 1, []
        in_string = False
        for char in code:
            if char == '"':
                in_string = not in_string
            if not in_string:
                for bracketPair in brackets:
                    if char == bracketPair[kStart]:
                        stack.append(char)
                    elif char == bracketPair[kEnd]:
                        if len(stack) == 0:
                            return {'status': 'invalid'}
                        if stack.pop() != bracketPair[kStart]:
                            return {'status': 'invalid'}
        if in_string:
            return {'status': 'incomplete', 'indent': ''}
        elif len(stack) != 0:
            return {'status': 'incomplete', 'indent': 4 * len(stack) * ' '}
        else:
            return {'status': 'complete'}

if __name__ == '__main__':
    from ipykernel.kernelapp import IPKernelApp
    IPKernelApp.launch_instance(kernel_class=MathicsKernel)
