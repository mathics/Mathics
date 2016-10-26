import sys
import traceback

from ipykernel.kernelbase import Kernel
from ipykernel.comm import CommManager
from traitlets import Instance, Type, Any
from ipykernel.zmqshell import ZMQInteractiveShell

from mathics.core.definitions import Definitions
from mathics.core.evaluation import Evaluation, Message, Result, Output, Print
from mathics.core.expression import Integer
from mathics.core.parser import IncompleteSyntaxError, TranslateError, ScanError
from mathics.core.parser import parse, TranslateError
from mathics.core.parser.feed import SingleLineFeeder
from mathics.core.parser.tokeniser import Tokeniser
from mathics.builtin import builtins
from mathics import settings
from mathics.version import __version__
from mathics.doc.doc import Doc
from mathics.layout.client import WebEngine


def parse_lines(lines, definitions):
    '''
    Given some lines of code try to construct a list of expressions.

    In the case of incomplete lines append more lines until a complete
    expression is found. If the end is reached and no complete expression is
    found then reraise the exception.

    We use a generator so that each expression can be evaluated (as the parser
    is dependent on defintions and evaluation may change the definitions).
    '''
    query = ''
    lines = lines.splitlines()

    incomplete_exc = None
    for line in lines:
        if not line:
            query += ' '
            continue
        query += line
        if query.endswith('\\'):
            query = query.rstrip('\\')
            incomplete_exc = IncompleteSyntaxError(len(query)-1)
            continue
        try:
            expression = parse(definitions, SingleLineFeeder(query))
        except IncompleteSyntaxError as exc:
            incomplete_exc = exc
        else:
            if expression is not None:
                yield expression
            query = ''
            incomplete_exc = None

    if incomplete_exc is not None:
        # ran out of lines
        raise incomplete_exc

    raise StopIteration


class KernelOutput(Output):
    def __init__(self, kernel):
        if getattr(Output, 'version', None) is None:
            super(KernelOutput, self).__init__()
        else:
            super(KernelOutput, self).__init__(kernel.web_engine)
        self.kernel = kernel

    def max_stored_size(self, settings):
        return None

    def out(self, out):
        self.kernel.out_callback(out)

    def clear(self, wait=False):
        self.kernel.clear_output_callback(wait=wait)

    def display(self, data, metadata):
        self.kernel.display_data_callback(data, metadata)

    def warn_about_web_engine(self):
        return True


class MathicsKernel(Kernel):
    implementation = 'Mathics'
    implementation_version = '0.1'
    language_info = {
        'version': __version__,
        'name': 'Mathematica',
        'mimetype': 'text/x-mathematica',
    }
    banner = "Mathics kernel"   # TODO

    shell = Instance('IPython.core.interactiveshell.InteractiveShellABC', allow_none=True)
    shell_class = Type(ZMQInteractiveShell)

    user_module = Any()
    user_ns = Instance(dict, args=None, allow_none=True)

    def __init__(self, **kwargs):
        Kernel.__init__(self, **kwargs)
        self.definitions = Definitions(add_builtin=True)        # TODO Cache
        self.definitions.set_ownvalue('$Line', Integer(0))  # Reset the line number
        self.establish_comm_manager()  # needed for ipywidgets and Manipulate[]
        self.web_engine = None

    def establish_comm_manager(self):
        # see ipykernel/ipkernel.py

        self.shell = self.shell_class.instance(
            parent=self,
            profile_dir=self.profile_dir,
            user_module=self.user_module,
            user_ns=self.user_ns,
            kernel=self)
        self.shell.displayhook.session = self.session
        self.shell.displayhook.pub_socket = self.iopub_socket
        self.shell.displayhook.topic = self._topic('execute_result')
        self.shell.display_pub.session = self.session
        self.shell.display_pub.pub_socket = self.iopub_socket

        self.comm_manager = CommManager(parent=self, kernel=self)
        comm_msg_types = ['comm_open', 'comm_msg', 'comm_close']
        for msg_type in comm_msg_types:
            self.shell_handlers[msg_type] = getattr(self.comm_manager, msg_type)

    def init_web_engine(self):
        if self.web_engine is None:
            self.web_engine = WebEngine()

    def do_execute(self, code, silent, store_history=True, user_expressions=None,
                   allow_stdin=False):
        # TODO update user definitions

        response = {
            'payload': [],
            'user_expressions': {},
        }

        formats = {
            'text/plain': 'text',
            'text/html': 'xml',
            'text/latex': 'tex',
        }

        try:
            self.init_web_engine()

            evaluation = Evaluation(self.definitions, output=KernelOutput(self), format=formats)

            result = evaluation.parse_evaluate(code, timeout=settings.TIMEOUT)

            if result:
                self.result_callback(result)
        except Exception as exc:
            stack = traceback.format_exception(*sys.exc_info())

            self.out_callback(Print('An error occured: ' + str(exc) + '\n\n' + '\n'.join(stack)))

            # internal error
            response['status'] = 'error'
            response['ename'] = 'System:exception'
            response['traceback'] = stack
        else:
            response['status'] = 'ok'

        response['execution_count'] = self.definitions.get_line_no()

        return response

    def out_callback(self, out):
        if out.is_message:
            content = {
                'name': 'stderr',
                'text': '{symbol}::{tag}: {text}\n'.format(**out.get_data()),
            }
        elif out.is_print:
            content = {
                'name': 'stdout',
                'text': out.text + '\n',
            }
        else:
            raise ValueError('Unknown out')
        self.send_response(self.iopub_socket, 'stream', content)

    def reconfigure_mathjax(self):
        # Jupyter's default MathJax configuration ("safe" mode) blocks the use
        # of data uris which we use in mglyphs for displaying svgs and imgs.
        # enable the "data" protocol here. also remove font size restrictions.

        # we set processSectionDelay to 0 since that drastically improves the
        # visual experience of Manipulate as there's a lot less jitter, also see
        # http://docs.mathjax.org/en/latest/api/hub.html

        safeModeJS = """
            MathJax.Hub.Config({
              showMathMenu: false,
              showProcessingMessages: false,
              messageStyle: "normal",
              displayAlign: "left",
              Safe: {
                  safeProtocols: {
                    data: true
                  },
                  allow: {
                    fontsize: "all"
                  }
                },
                "HTML-CSS": {
                    availableFonts: [], /* force Web font */
                    preferredFont: null, /* force Web font */
                    webFont: "Asana-Math",
                    linebreaks: {
                        automatic: true,
                        width: "70%"
                    }
                }
          });

          MathJax.Hub.processSectionDelay = 0;
        """

        # see http://jupyter-client.readthedocs.org/en/latest/messaging.html
        content = {
            'data': {'application/javascript': safeModeJS},
            'metadata': {},
        }
        self.send_response(self.iopub_socket, 'display_data', content)

    def result_callback(self, result):
        self.reconfigure_mathjax()

        content = {
            'execution_count': result.line_no,
            'data': result.result,
            'metadata': {},
        }
        self.send_response(self.iopub_socket, 'execute_result', content)

    def clear_output_callback(self, wait=False):
        # see http://jupyter-client.readthedocs.org/en/latest/messaging.html
        content = dict(wait=wait)
        self.send_response(self.iopub_socket, 'clear_output', content)

    def display_data_callback(self, data, metadata):
        self.reconfigure_mathjax()

        # see http://jupyter-client.readthedocs.org/en/latest/messaging.html
        content = {
            'data': data,
            'metadata': metadata,
        }
        self.send_response(self.iopub_socket, 'display_data', content)

    def do_inspect(self, code, cursor_pos, detail_level=0):
        start_pos, end_pos, name = self.find_symbol_name(code, cursor_pos)

        if name is None:
            return {'status': 'error'}

        if '`' not in name:
            name = 'System`' + name

        try:
            instance = builtins[name]
        except KeyError:
            return {'status': 'ok', 'found': False, 'data': {}, 'metadata': {}}

        doc = Doc(instance.__doc__ or '')
        data = {
            'text/plain': str(doc),
            # TODO latex
            # TODO html
        }
        return {'status': 'ok', 'found': True, 'data': data, 'metadata': {}}

    def do_complete(self, code, cursor_pos):
        start_pos, end_pos, name = self.find_symbol_name(code, cursor_pos)

        if name is None:
            return {'status': 'error'}

        remove_system = False
        system_prefix = 'System`'
        if '`' not in name:
            name = system_prefix + name
            remove_system = True

        matches = []
        for key in builtins:
            if key.startswith(name):
                matches.append(key)

        if remove_system:
            matches = [match[len(system_prefix):] for match in matches]

        return {
            'status': 'ok',
            'matches': matches,
            'cursor_start': start_pos,
            'cursor_end': end_pos,
            'metadata': {},
        }

    def do_is_complete(self, code):
        try:
            # list forces generator evaluation (parse all lines)
            list(parse_lines(code, self.definitions))
        except IncompleteSyntaxError:
            return {'status': 'incomplete', 'indent': ''}
        except TranslateError:
            return {'status': 'invalid'}
        else:
            return {'status': 'complete'}

    @staticmethod
    def find_symbol_name(code, cursor_pos):
        '''
        Given a string of code tokenize it until cursor_pos and return the final symbol name.
        returns None if no symbol is found at cursor_pos.

        >>> MathicsKernel.find_symbol_name('1 + Sin', 6)
        'System`Sin'

        >>> MathicsKernel.find_symbol_name('1 + ` Sin[Cos[2]] + x', 8)
        'System`Sin'

        >>> MathicsKernel.find_symbol_name('Sin `', 4)
        '''

        tokeniser = Tokeniser(SingleLineFeeder(code))

        start_pos = None
        end_pos = None
        name = None
        while True:
            try:
                token = tokeniser.next()
            except ScanError:
                continue
            if token.tag == 'END':
                break   # ran out of tokens
            # find first token which contains cursor_pos
            if tokeniser.pos >= cursor_pos:
                if token.tag == 'Symbol':
                    name = token.text
                    start_pos = token.pos
                    end_pos = tokeniser.pos
                break
        return start_pos, end_pos, name
