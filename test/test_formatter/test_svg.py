import re
from mathics.core.expression import Symbol, Integer0, Integer1, Expression
from mathics.core.evaluation import Evaluation
from mathics.session import MathicsSession
from mathics.builtin.inout import MakeBoxes
from mathics.core.formatter import lookup_method

session = MathicsSession(add_builtin=True, catch_interrupt=False)
evaluation = Evaluation(session.definitions)

GraphicsSymbol = Symbol("Graphics")
ListSymbol = Symbol("List")

svg_wrapper_pat = r"""\s*<svg width="[0-9.]+px" height="[0-9.]+px" xmlns:svg="http://www.w3.org/2000/svg"
\s*xmlns="http://www.w3.org/2000/svg"
\s*version="1\.1"
"""


def extract_svg_body(svg):
    matches = re.match(svg_wrapper_pat, svg)
    assert matches
    body = svg[len(matches.group(0)) :]
    assert matches
    view_inner_match = re.match(
        r"^\s+viewBox=.*\s+<!--GraphicsElements-->\s+(?:<!--.+-->\s+)?(.*)", body
    )
    assert view_inner_match
    inner_svg = view_inner_match.group(1)
    print(inner_svg)
    return inner_svg


def get_svg(expression):
    options = {}
    boxes = MakeBoxes(expression).evaluate(evaluation)

    # Would be nice to DRY this boilerplate from boxes_to_mathml

    leaves = boxes._leaves
    elements, calc_dimensions = boxes._prepare_elements(
        leaves, options=options, neg_y=True
    )
    xmin, xmax, ymin, ymax, w, h, width, height = calc_dimensions()
    data = (elements, xmin, xmax, ymin, ymax, w, h, width, height)

    format_fn = lookup_method(boxes, "svg")
    return format_fn(boxes, leaves, data=data, options=options)


def test_svg_circle():
    expression = Expression(
        GraphicsSymbol,
        Expression("Circle", Expression(ListSymbol, Integer0, Integer0)),
    )

    svg = get_svg(expression)
    inner_svg = extract_svg_body(svg)

    # Circles are implemented as ellipses with equal major and minor axes.
    # Check for that.
    matches = re.match(
        r'^<ellipse cx="(\S+)" cy="(\S+)" rx="(\S+)" ry="(\S+)" .*/>', inner_svg
    )
    assert matches
    assert matches.group(1) == matches.group(2) == matches.group(3)


def test_svg_point():
    expression = Expression(
        GraphicsSymbol,
        Expression("Point", Expression(ListSymbol, Integer0, Integer0)),
    )

    svg = get_svg(expression)
    inner_svg = extract_svg_body(svg)

    # Circles are implemented as ellipses with equal major and minor axes.
    # Check for that.
    print(inner_svg)
    matches = re.match(r'^<circle cx="(\S+)" cy="(\S+)"', inner_svg)
    assert matches
    assert matches.group(1) == matches.group(2)


def test_svg_arrowbox():
    expression = Expression(
        GraphicsSymbol,
        Expression(
            "Arrow",
            Expression(
                ListSymbol,
                Expression(ListSymbol, Integer0, Integer0),
                Expression(ListSymbol, Integer1, Integer1),
            ),
        ),
    )
    svg = get_svg(expression)
    inner_svg = extract_svg_body(svg)

    matches = re.match(r'^<polyline points="', inner_svg)
    # TODO: Could pick endpoint of this line and match with beginnign of arrow polygon below
    assert matches
    # arrow_polygon = inner_svg[len(matches.group(0)) - 1 :]
    # matches = re.match(r'^<polygon points=".+"\s+style=".*"\s*/>', arrow_polygon)
    # assert matches


def test_svg_bezier_curve():

    expression = Expression(
        GraphicsSymbol,
        Expression(
            "BezierCurve",
            Expression(
                ListSymbol,
                Expression(ListSymbol, Integer0, Integer0),
                Expression(ListSymbol, Integer1, Integer1),
            ),
        ),
    )
    svg = get_svg(expression)
    inner_svg = extract_svg_body(svg)

    matches = re.match(r'^<path d="', inner_svg)
    assert matches


if __name__ == "__main__":
    test_svg_bezier_curve()
