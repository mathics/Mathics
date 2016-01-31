from ipykernel.kernelbase import Kernel

from mathics.core.definitions import Definitions
from mathics.core.evaluation import Evaluation
from mathics.core.expression import Integer
from mathics import settings

class MathicsKernel(Kernel):
    implementation = 'Mathics'
    implementation_version = '1.0'  # TODO
    language = 'wolfram'
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

if __name__ == '__main__':
    from ipykernel.kernelapp import IPKernelApp
    IPKernelApp.launch_instance(kernel_class=MathicsKernel)
