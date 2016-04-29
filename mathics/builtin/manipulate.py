#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import unicode_literals
from __future__ import absolute_import

from mathics.core.expression import Real, String
from mathics import settings
from mathics.core.evaluation import Evaluation

from mathics.builtin.base import Builtin
from mathics.core.expression import Expression, Symbol, Integer, from_python

try:
    from ipywidgets import (IntSlider, FloatSlider, ToggleButtons, Box, DOMWidget)
    _enabled = True
except ImportError:
    # fallback to non-Manipulate-enabled build if we don't have ipywidgets installed.
    _enabled = False

"""
A basic implementation of Manipulate[]. There is currently no support for Dynamic[] elements.
This implementation is basically a port from ipywidget.widgets.interaction for Mathics.
"""

def _interactive(interact_f, kwargs_widgets):
    # this is a modified version of interactive() in ipywidget.widgets.interaction

    container = Box(_dom_classes=['widget-interact'])
    container.children = [w for w in kwargs_widgets if isinstance(w, DOMWidget)]

    def call_f(name=None, old=None, new=None):
        kwargs = dict((widget._kwarg, widget.value) for widget in kwargs_widgets)
        try:
            interact_f(**kwargs)
        except Exception as e:
            container.log.warn("Exception in interact callback: %s", e, exc_info=True)

    for widget in kwargs_widgets:
        widget.on_trait_change(call_f, 'value')

    container.on_displayed(lambda _: call_f(None, None, None))

    return container


def _strip_namespace(name):
    return name.split('`')[-1]  # e.g. "Global`x" will turn into "x"


def _manipulate_label(x):
    if isinstance(x, String):
        return x.get_string_value()
    elif isinstance(x, Symbol):
        return _strip_namespace(x.get_name())
    else:
        return str(x)


class PatternDispatcher:
    '''A pattern matcher similar to the one for BuiltIns, but usable in any context (e.g. locally)'''

    def __init__(self):
        from mathics.core.parser import parse_builtin_rule
        from mathics.builtin.patterns import Pattern

        self._patterns = []
        for name in dir(self):
            if name.startswith('apply_'):
                function = getattr(self, name)
                self._patterns.append((Pattern.create(
                    parse_builtin_rule(function.__doc__)), function))

    def dispatch(self, expr, evaluation):
        from mathics.builtin.patterns import StopGenerator

        class StopGenerator_MatchQ(StopGenerator):
            pass

        try:
            for pattern, function in self._patterns:
                def yield_func(vars, rest):
                    function(**dict((_strip_namespace(key), value) for (key, value) in vars.items()))
                    raise StopGenerator_MatchQ(Symbol("True"))

                pattern.match(yield_func, expr, {}, evaluation)
        except StopGenerator_MatchQ:
            return True

        return False


class IllegalWidgetArguments(Exception):
    def __init__(self, var):
        super(IllegalWidgetArguments, self).__init__()
        self.var = var


class Manipulations(PatternDispatcher):
    def __init__(self, evaluation):
        super(Manipulations, self).__init__()
        self._evaluation = evaluation
        self._widgets = []  # the ipywidget widgets to control the manipulated variables
        self._parsers = {}  # lambdas to decode the widget values into Mathics expressions

    def get_widgets(self):
        return self._widgets

    def _add_widget(self, widget, name, parse, label):
        if not widget.description:
            widget.description = _manipulate_label(label)
        widget._kwarg = name  # see _interactive() above
        self._parsers[name] = parse
        self._widgets.append(widget)

    def build_callback(self, callback):
        parsers = self._parsers
        def new_callback(**kwargs):
            callback(**dict((name, parsers[name](value)) for (name, value) in kwargs.items()))
        return new_callback

    def _add_min_max_var(self, i, imin, imax, idefault, ilabel):
        imin_value = imin.to_python()
        imax_value = imax.to_python()
        if imin_value > imax_value:
            raise IllegalWidgetArguments(i)
        else:
            idefault_value = min(max(idefault.to_python(), imin_value), imax_value)
            widget = FloatSlider(value=idefault_value, min=imin_value, max=imax_value)
            self._add_widget(widget, i.get_name(), lambda x: from_python(x), ilabel)

    def _add_min_max_step_var(self, i, imin, imax, idefault, di, ilabel):
        imin_value = imin.to_python()
        imax_value = imax.to_python()
        di_value = di.to_python()
        if imin_value > imax_value or di_value <= 0 or di_value > (imax_value - imin_value):
            raise IllegalWidgetArguments(i)
        else:
            idefault_value = min(max(idefault.to_python(), imin_value), imax_value)
            if all(isinstance(x, Integer) for x in [imin, imax, idefault, di]):
                widget = IntSlider(value=idefault_value, min=imin_value, max=imax_value,
                                   step=di_value)
            else:
                widget = FloatSlider(value=idefault_value, min=imin_value, max=imax_value,
                                     step=di_value)
            self._add_widget(widget, i.get_name(), lambda x: from_python(x), ilabel)

    def _add_discrete_options_var(self, symbol, options, idefault, ilabel):
        formatted_options = []
        for i, option in enumerate(options.leaves):
            data = self._evaluation.format_all_outputs(option)
            formatted_options.append((data['text/plain'], i))

        default_index = 0
        for i, option in enumerate(options.leaves):
            if option.same(idefault):
                default_index = i

        widget = ToggleButtons(options=formatted_options, value=default_index)
        self._add_widget(widget, symbol.get_name(), lambda i: options.leaves[i], ilabel)

    def apply_min_max_var1(self, i, imin, imax):
        '{i_Symbol, imin_?RealNumberQ, imax_?RealNumberQ}'
        self._add_min_max_var(i, imin, imax, imin, i)

    def apply_min_max_var2(self, i, imin, imax, idefault):
        '{{i_Symbol, idefault_?RealNumberQ}, imin_?RealNumberQ, imax_?RealNumberQ}'
        self._add_min_max_var(i, imin, imax, idefault, i)

    def apply_min_max_var3(self, i, imin, imax, idefault, ilabel):
        '{{i_Symbol, idefault_?RealNumberQ, ilabel_}, imin_?RealNumberQ, imax_?RealNumberQ}'
        self._add_min_max_var(i, imin, imax, idefault, ilabel)

    def apply_min_max_step_var1(self, i, imin, imax, di):
        '{i_Symbol, imin_?RealNumberQ, imax_?RealNumberQ, di_?RealNumberQ}'
        self._add_min_max_step_var(i, imin, imax, imin, di, i)

    def apply_min_max_step_var2(self, i, imin, imax, di, idefault):
        '{{i_Symbol, idefault_?RealNumberQ}, imin_?RealNumberQ, imax_?RealNumberQ, di_?RealNumberQ}'
        self._add_min_max_step_var(i, imin, imax, idefault, di, i)

    def apply_min_max_step_var3(self, i, imin, imax, di, idefault, ilabel):
        '{{i_Symbol, idefault_?RealNumberQ, ilabel_}, imin_?RealNumberQ, imax_?RealNumberQ, di_?RealNumberQ}'
        self._add_min_max_step_var(i, imin, imax, idefault, di, ilabel)

    def apply_discrete_options_var1(self, i, options):
        '{i_Symbol, options_List}'
        if len(options.leaves) > 0:
            self._add_discrete_options_var(i, options, options.leaves[0], i)

    def apply_discrete_options_var2(self, i, options, idefault):
        '{{i_Symbol, idefault_}, options_List}'
        self._add_discrete_options_var(i, options, idefault, i)

    def apply_discrete_options_var3(self, i, options, idefault, ilabel):
        '{{i_Symbol, idefault_, ilabel_}, options_List}'
        self._add_discrete_options_var(i, options, idefault, ilabel)


class ListAnimate(Builtin):
    rules = {
        'ListAnimate[l_List]': 'Manipulate[Part[l, i], {i, 1, Length[l], 1}]'
    }


class Manipulate(Builtin):
    """
    <dl>
    <dt>'Manipulate[$expr1$, $arg1$, ...]'
        <dd>allows you to interactively compute and display an expression with different argument values.
    </dl>

    Manipulate[N[Sin[y]], {y, 1, 20, 2}]

    Manipulate[i^3, {i, {2, x^4, a}}]

    Manipulate[x^y, {x, 1, 20}, {y, 1, 3}]

    """

    attributes = ('HoldAll',)  # we'll call ReleaseHold at the time of evaluation below

    messages = {
        'noipywidget': 'Manipulate[] needs the ipywidgets module to work.',
        'widgetargs': 'Illegal variable range or step parameters for ``.'
    }

    def apply(self, expr, args, evaluation):
        'Manipulate[expr_, args__]'

        if not _enabled:
            evaluation.message('Manipulate', 'noipywidget')
            return Symbol('Null')

        try:
            manip = Manipulations(evaluation)  # knows about the arguments and their widgets

            for arg in args.get_sequence():
                try:
                    if not manip.dispatch(arg.evaluate(evaluation), evaluation):  # not a valid argument pattern?
                        return Expression(self.get_name(), expr, *args.get_sequence())  # identity
                except IllegalWidgetArguments as e:
                    evaluation.message('Manipulate', 'widgetargs', _strip_namespace(str(e.var)))

            clear_output_callback = evaluation.clear_output_callback
            display_data_callback = evaluation.display_data_callback  # for pushing updates

            def callback(**kwargs):
                clear_output_callback(wait=True)

                line_no = evaluation.definitions.get_line_no()

                new_evaluation = Evaluation(evaluation.definitions, result_callback=display_data_callback,
                                            out_callback=evaluation.out_callback)

                vars = [Expression('Set', Symbol(name), value) for name, value in kwargs.items()]
                evaluatable = Expression('ReleaseHold', Expression('Module', Expression('List', *vars), expr))
                new_evaluation.evaluate([evaluatable], timeout=settings.TIMEOUT)

                evaluation.definitions.set_line_no(line_no)  # do not increment line_no for manipulate computations

            widgets = manip.get_widgets()
            if len(widgets) > 0:
                box = _interactive(manip.build_callback(callback), widgets)  # create the widget

                # the following code is a boiled down version from IPython.core.formatters.IPythonDisplayFormatter.
                # note that '_ipython_display_' is a magic constant defined in print_method of IPythonDisplayFormatter.

                method = getattr(box, '_ipython_display_')
                if method is not None:
                    method()  # make the widget appear on the Jupyter notebook

            return Symbol('Null')  # the interactive output is pushed via kernel.display_data_callback (see above)
        except:
            import sys
            return String(repr(sys.exc_info()))