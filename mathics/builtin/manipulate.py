# -*- coding: utf-8 -*-

from mathics.version import __version__  # noqa used in loading to check consistency.
from mathics.core.expression import String, strip_context
from mathics import settings
from mathics.core.evaluation import Output

from mathics.builtin.base import Builtin
from mathics.core.expression import Expression, Symbol, Integer, from_python

try:
    from ipykernel.kernelbase import Kernel

    _jupyter = True
except ImportError:
    _jupyter = False

try:
    from ipywidgets import IntSlider, FloatSlider, ToggleButtons, Box, DOMWidget
    from IPython.core.formatters import IPythonDisplayFormatter

    _ipywidgets = True
except ImportError:
    # fallback to non-Manipulate-enabled build if we don't have ipywidgets installed.
    _ipywidgets = False


"""
A basic implementation of Manipulate[]. There is currently no support for Dynamic[] elements.
This implementation is basically a port from ipywidget.widgets.interaction for Mathics.
"""


def _interactive(interact_f, kwargs_widgets):
    # this is a modified version of interactive() in ipywidget.widgets.interaction

    container = Box(_dom_classes=["widget-interact"])
    container.children = [w for w in kwargs_widgets if isinstance(w, DOMWidget)]

    def call_f(name=None, old=None, new=None):
        kwargs = dict((widget._kwarg, widget.value) for widget in kwargs_widgets)
        try:
            interact_f(**kwargs)
        except Exception as e:
            container.log.warn("Exception in interact callback: %s", e, exc_info=True)

    for widget in kwargs_widgets:
        widget.on_trait_change(call_f, "value")

    container.on_displayed(lambda _: call_f(None, None, None))

    return container


class IllegalWidgetArguments(Exception):
    def __init__(self, var):
        super(IllegalWidgetArguments, self).__init__()
        self.var = var


class JupyterWidgetError(Exception):
    def __init__(self, err):
        super(JupyterWidgetError, self).__init__()
        self.err = err


class ManipulateParameter(
    Builtin
):  # parses one Manipulate[] parameter spec, e.g. {x, 1, 2}, see _WidgetInstantiator
    context = "System`Private`"

    rules = {
        # detect x and {x, default} and {x, default, label}.
        "System`Private`ManipulateParameter[{s_Symbol, r__}]": "System`Private`ManipulateParameter[{Symbol -> s, Label -> s}, {r}]",
        "System`Private`ManipulateParameter[{{s_Symbol, d_}, r__}]": "System`Private`ManipulateParameter[{Symbol -> s, Default -> d, Label -> s}, {r}]",
        "System`Private`ManipulateParameter[{{s_Symbol, d_, l_}, r__}]": "System`Private`ManipulateParameter[{Symbol -> s, Default -> d, Label -> l}, {r}]",
        # detect different kinds of widgets. on the use of the duplicate key "Default ->", see _WidgetInstantiator.add()
        "System`Private`ManipulateParameter[var_, {min_?RealNumberQ, max_?RealNumberQ}]": 'Join[{Type -> "Continuous", Minimum -> min, Maximum -> max, Default -> min}, var]',
        "System`Private`ManipulateParameter[var_, {min_?RealNumberQ, max_?RealNumberQ, step_?RealNumberQ}]": 'Join[{Type -> "Discrete", Minimum -> min, Maximum -> max, Step -> step, Default -> min}, var]',
        "System`Private`ManipulateParameter[var_, {opt_List}] /; Length[opt] > 0": 'Join[{Type -> "Options", Options -> opt, Default -> Part[opt, 1]}, var]',
    }


def _manipulate_label(x):  # gets the label that is displayed for a symbol or name
    if isinstance(x, String):
        return x.get_string_value()
    elif isinstance(x, Symbol):
        return strip_context(x.get_name())
    else:
        return str(x)


def _create_widget(widget, **kwargs):
    try:
        return widget(**kwargs)
    except Exception as e:
        raise JupyterWidgetError(str(e))


class _WidgetInstantiator:
    # we do not want to have widget instances (like FloatSlider) get into the evaluation pipeline (e.g. via Expression
    # or Atom), since there might be all kinds of problems with serialization of these widget classes.  therefore, the
    # elegant recursive solution for parsing parameters (like in Table[]) is not feasible here; instead, we must create
    # and use the widgets in one "transaction" here, without holding them in expressions or atoms.

    def __init__(self):
        self._widgets = []  # the ipywidget widgets to control the manipulated variables
        self._parsers = (
            {}
        )  # lambdas to decode the widget values into Mathics expressions

    def add(self, expression, evaluation):
        expr = Expression("System`Private`ManipulateParameter", expression).evaluate(
            evaluation
        )
        if (
            expr.get_head_name() != "System`List"
        ):  # if everything was parsed ok, we get a List
            return False
        # convert the rules given us by ManipulateParameter[] into a dict. note: duplicate keys
        # will be overwritten, the latest one wins.
        kwargs = {"evaluation": evaluation}
        for rule in expr.leaves:
            if rule.get_head_name() != "System`Rule" or len(rule.leaves) != 2:
                return False
            kwargs[strip_context(rule.leaves[0].to_python()).lower()] = rule.leaves[1]
        widget = kwargs["type"].get_string_value()
        del kwargs["type"]
        getattr(self, "_add_%s_widget" % widget.lower())(**kwargs)  # create the widget
        return True

    def get_widgets(self):
        return self._widgets

    def build_callback(self, callback):
        parsers = self._parsers

        def new_callback(**kwargs):
            callback(
                **dict((name, parsers[name](value)) for (name, value) in kwargs.items())
            )

        return new_callback

    def _add_continuous_widget(
        self, symbol, label, default, minimum, maximum, evaluation
    ):
        minimum_value = minimum.to_python()
        maximum_value = maximum.to_python()
        if minimum_value > maximum_value:
            raise IllegalWidgetArguments(symbol)
        else:
            defval = min(max(default.to_python(), minimum_value), maximum_value)
            widget = _create_widget(
                FloatSlider, value=defval, min=minimum_value, max=maximum_value
            )
            self._add_widget(widget, symbol.get_name(), lambda x: from_python(x), label)

    def _add_discrete_widget(
        self, symbol, label, default, minimum, maximum, step, evaluation
    ):
        minimum_value = minimum.to_python()
        maximum_value = maximum.to_python()
        step_value = step.to_python()
        if (
            minimum_value > maximum_value
            or step_value <= 0
            or step_value > (maximum_value - minimum_value)
        ):
            raise IllegalWidgetArguments(symbol)
        else:
            default_value = min(max(default.to_python(), minimum_value), maximum_value)
            if all(isinstance(x, Integer) for x in [minimum, maximum, default, step]):
                widget = _create_widget(
                    IntSlider,
                    value=default_value,
                    min=minimum_value,
                    max=maximum_value,
                    step=step_value,
                )
            else:
                widget = _create_widget(
                    FloatSlider,
                    value=default_value,
                    min=minimum_value,
                    max=maximum_value,
                    step=step_value,
                )
            self._add_widget(widget, symbol.get_name(), lambda x: from_python(x), label)

    def _add_options_widget(self, symbol, options, default, label, evaluation):
        formatted_options = []
        for i, option in enumerate(options.leaves):
            data = evaluation.format_output(option, format="text")
            formatted_options.append((data, i))

        default_index = 0
        for i, option in enumerate(options.leaves):
            if option.same(default):
                default_index = i

        widget = _create_widget(
            ToggleButtons, options=formatted_options, value=default_index
        )
        self._add_widget(widget, symbol.get_name(), lambda j: options.leaves[j], label)

    def _add_widget(self, widget, name, parse, label):
        if not widget.description:
            widget.description = _manipulate_label(label)
        widget._kwarg = name  # see _interactive() above
        self._parsers[name] = parse
        self._widgets.append(widget)


class ManipulateOutput(Output):
    def max_stored_size(self, settings):
        return self.output.max_stored_size(settings)

    def out(self, out):
        return self.output.out(out)

    def clear_output(wait=False):
        raise NotImplementedError

    def display_data(self, result):
        raise NotImplementedError


class Manipulate(Builtin):
    """
    <dl>
    <dt>'Manipulate[$expr1$, {$u$, $u_min$, $u_max$}]'
        <dd>interactively compute and display an expression with different values of $u$.
    <dt>'Manipulate[$expr1$, {$u$, $u_min$, $u_max$, $du$}]'
        <dd>allows $u$ to vary between $u_min$ and $u_max$ in steps of $du$.
    <dt>'Manipulate[$expr1$, {{$u$, $u_init$}, $u_min$, $u_max$, ...}]'
        <dd>starts with initial value of $u_init$.
    <dt>'Manipulate[$expr1$, {{$u$, $u_init$, $u_lbl$}, ...}]'
        <dd>labels the $u$ controll by $u_lbl$.
    <dt>'Manipulate[$expr1$, {$u$, {$u_1$, $u_2$, ...}}]'
        <dd>sets $u$ to take discrete values $u_1$, $u_2$, ... .
    <dt>'Manipulate[$expr1$, {$u$, ...}, {$v$, ...}, ...]'
        <dd>control each of $u$, $v$, ... .
    </dl>

    >> Manipulate[N[Sin[y]], {y, 1, 20, 2}]
     : Manipulate[] only works inside a Jupyter notebook.
     = Manipulate[N[Sin[y]], {y, 1, 20, 2}]

    >> Manipulate[i ^ 3, {i, {2, x ^ 4, a}}]
     : Manipulate[] only works inside a Jupyter notebook.
     = Manipulate[i ^ 3, {i, {2, x ^ 4, a}}]

    >> Manipulate[x ^ y, {x, 1, 20}, {y, 1, 3}]
     : Manipulate[] only works inside a Jupyter notebook.
     = Manipulate[x ^ y, {x, 1, 20}, {y, 1, 3}]

    >> Manipulate[N[1 / x], {{x, 1}, 0, 2}]
     : Manipulate[] only works inside a Jupyter notebook.
     = Manipulate[N[1 / x], {{x, 1}, 0, 2}]

    >> Manipulate[N[1 / x], {{x, 1}, 0, 2, 0.1}]
     : Manipulate[] only works inside a Jupyter notebook.
     = Manipulate[N[1 / x], {{x, 1}, 0, 2, 0.1}]
    """

    # TODO: correct in the jupyter interface but can't be checked in tests
    """
    #> Manipulate[x, {x}]
     = Manipulate[x, {x}]

    #> Manipulate[x, {x, 1, 0}]
     : 'Illegal variable range or step parameters for `x`.
     = Manipulate[x, {x, 1, 0}]
    """

    attributes = ("HoldAll",)  # we'll call ReleaseHold at the time of evaluation below

    messages = {
        "jupyter": "Manipulate[] only works inside a Jupyter notebook.",
        "imathics": "Your IMathics kernel does not seem to support all necessary operations. "
        + "Please check that you have the latest version installed.",
        "widgetmake": 'Jupyter widget construction failed with "``".',
        "widgetargs": "Illegal variable range or step parameters for ``.",
        "widgetdisp": "Jupyter failed to display the widget.",
    }

    requires = ("ipywidgets",)

    def apply(self, expr, args, evaluation):
        "Manipulate[expr_, args__]"
        if (not _jupyter) or (not Kernel.initialized()) or (Kernel.instance() is None):
            return evaluation.message("Manipulate", "jupyter")

        instantiator = (
            _WidgetInstantiator()
        )  # knows about the arguments and their widgets

        for arg in args.get_sequence():
            try:
                if not instantiator.add(
                    arg, evaluation
                ):  # not a valid argument pattern?
                    return
            except IllegalWidgetArguments as e:
                return evaluation.message(
                    "Manipulate", "widgetargs", strip_context(str(e.var))
                )
            except JupyterWidgetError as e:
                return evaluation.message("Manipulate", "widgetmake", e.err)

        clear_output_callback = evaluation.output.clear
        display_data_callback = evaluation.output.display  # for pushing updates

        try:
            clear_output_callback(wait=True)
        except NotImplementedError:
            return evaluation.message("Manipulate", "imathics")

        def callback(**kwargs):
            clear_output_callback(wait=True)

            line_no = evaluation.definitions.get_line_no()

            vars = [
                Expression("Set", Symbol(name), value) for name, value in kwargs.items()
            ]
            evaluatable = Expression(
                "ReleaseHold", Expression("Module", Expression("List", *vars), expr)
            )

            result = evaluation.evaluate(evaluatable, timeout=settings.TIMEOUT)
            if result:
                display_data_callback(data=result.result, metadata={})

            evaluation.definitions.set_line_no(
                line_no
            )  # do not increment line_no for manipulate computations

        widgets = instantiator.get_widgets()
        if len(widgets) > 0:
            box = _interactive(
                instantiator.build_callback(callback), widgets
            )  # create the widget
            formatter = IPythonDisplayFormatter()
            if not formatter(box):  # make the widget appear on the Jupyter notebook
                return evaluation.message("Manipulate", "widgetdisp")

        return Symbol(
            "Null"
        )  # the interactive output is pushed via kernel.display_data_callback (see above)
