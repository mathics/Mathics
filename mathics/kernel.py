import sys
import traceback

from ipykernel.kernelbase import Kernel

from mathics.core.definitions import Definitions
from mathics.core.evaluation import Evaluation, Message, Result
from mathics.core.expression import Integer
from mathics.builtin import builtins
from mathics import settings
from mathics.version import __version__
from mathics.doc import Doc


class MathicsKernel(Kernel):
    implementation = 'Mathics'
    implementation_version = __version__
    language = 'Wolfram'
    language_version = '0.1'    # TODO
    language_info = {
        'name': 'mathics',
        'mimetype': 'text/plain',
        'file_extension': 'm',
    }
    banner = "Mathics kernel"   # TODO

    def __init__(self, **kwargs):
        Kernel.__init__(self, **kwargs)
        self.definitions = Definitions(add_builtin=True)
        self.definitions.set_ownvalue('$Line', Integer(0))  # Reset the line number

    def do_execute(self, code, silent, store_history=True, user_expressions=None,
                   allow_stdin=False):
        # TODO update user definitions

        response = {
            # FIXME Hack - base class increments the execution count
            'execution_count': self.execution_count,
            'payload': [],
            'user_expressions': {},
            }
        try:
            evaluation = Evaluation(code, self.definitions,
                                    timeout=settings.TIMEOUT)
        except Exception as exc:
            response['status'] = 'error'
            response['ename'] = 'System:exception'
            response['traceback'] = traceback.format_exception(*sys.exc_info())
            # if settings.DEBUG:
            #     evaluation = Evaluation()
            #     info = '\n'.join(response['traceback'])
            #     msg = 'Exception raised: %s\n\n%s' % (exc, info)
            #     msg = Message('System', 'exception', msg)
            #     evaluation.results = [Result([msg], None, None)]
        else:
            response['status'] = 'ok'

        if not silent:
            for result in evaluation.results:
                if result is not None:
                    stream_content = {'name': 'stdout', 'text': result.result}
                    self.send_response(self.iopub_socket, 'stream', stream_content)

        return response

    def do_inspect(self, code, cursor_pos, detail_level=0):
        # name = code[:cursor_pos]
        name = code

        if '`' not in name:
            name = 'System`' + name

        try:
            instance = builtins[name]
        except KeyError:
            return {'status': 'ok', 'found': False, 'data': {}, 'metadata': {}}

        doc = Doc(instance.__doc__ or '')    # TODO Handle possible ValueError here
        data = {'text/plain': doc.text(detail_level), 'text/html': doc.html()}        # TODO 'application/x-tex': doc.latex()
        return {'status': 'ok', 'found': True, 'data' : data, 'metadata': {}}

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
