from mathics.core.parser import parse, SingleLineFeeder
from mathics.core.definitions import Definitions
from mathics.core.evaluation import Evaluation
from mathics import settings


class MathicsSession:
    def __init__(self, add_builtin=True, catch_interrupt=False, form="InputForm"):
        self.definitions = Definitions(add_builtin)
        self.evaluation = Evaluation(definitions=self.definitions, catch_interrupt=catch_interrupt)
        self.form = form
        self.last_result = None
        
    def evaluate(self, str_expression, timeout = None, form=None):
        expr = parse(self.definitions, SingleLineFeeder(str_expression))
        if form is None:
            form = self.form
        self.last_result = expr.evaluate(self.evaluation)
        return self.last_result

    def format_result(self, str_expression=None, timeout = None, form=None):
        self.last_result
        if form is None:
            form = self.form
        return res.do_format(self.evaluation, form)
