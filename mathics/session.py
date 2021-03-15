from mathics.core.parser import parse, MathicsSingleLineFeeder
from mathics.core.definitions import Definitions
from mathics.core.evaluation import Evaluation
from mathics import settings


class MathicsSession:
    def __init__(self, add_builtin=True, catch_interrupt=False, format="InputForm"):
        self.definitions = Definitions(add_builtin)
        self.evaluation = Evaluation(definitions=self.definitions, catch_interrupt=catch_interrupt)
        self.format = format
        self.last_result = None

    def raw_evaluation(self, str_expression, timeout = None, form=None):
        expr = parse(self.definitions, MathicsSingleLineFeeder(str_expression))
        if form is None:
            form = self.form
        return expr.evaluate(evaluation)

    def evaluate(self, str_expression, timeout = None, format=None):
        expr = parse(self.definitions, MathicsSingleLineFeeder(str_expression))
        if format is None:
            format = self.format
        self.last_result = self.evaluation.evaluate(expr, timeout=timeout, format=format)
        return self.last_result.result


