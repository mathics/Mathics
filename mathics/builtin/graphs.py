#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Graphs
"""

# uses GraphViz, if it's installed in your PATH (see pydotplus.graphviz.find_graphviz and http://www.graphviz.org).

from __future__ import unicode_literals
from __future__ import absolute_import
from __future__ import division

from mathics.builtin.base import Builtin, AtomBuiltin
from mathics.builtin.graphics import GraphicsBox
from mathics.builtin.randomnumbers import RandomEnv
from mathics.core.expression import Expression, Symbol, Atom, Real, Integer, system_symbols_dict, from_python
from mathics.core.util import robust_min

from itertools import permutations, islice
from collections import defaultdict
from math import sqrt, ceil

try:
    import networkx as nx
except ImportError:
    nx = {}


def _circular_layout(G):
    return nx.drawing.circular_layout(G, scale=1.5)


def _spectral_layout(G):
    return nx.drawing.spectral_layout(G, scale=2.0)


def _shell_layout(G):
    return nx.drawing.shell_layout(G, scale=2.0)


def _generic_layout(G, warn):
    try:
        import pydotplus

        if pydotplus.graphviz.find_graphviz():
            return nx.nx_pydot.graphviz_layout(G, prog='dot')
    except ImportError:
        pass

    warn('Could not find pydotplus/dot; graph layout quality might be low.')
    return nx.drawing.fruchterman_reingold_layout(G, pos=None, k=1.0)


def _path_layout(G, root):
    v = root
    x = 0
    y = 0

    k = 0
    d = 0

    pos = {}
    neighbors = G.neighbors(v)

    for _ in range(len(G)):
        pos[v] = (x, y)

        if not neighbors:
            break
        v = neighbors[0]
        neighbors = G.neighbors(v)

        if k == 0:
            if d < 1 or neighbors:
                d += 1
            x += d
        elif k == 1:
            y += d
        elif k == 2:
            if neighbors:
                d += 1
            x -= d
        elif k == 3:
            y -= d

        k = (k + 1) % 4

    return pos


def _auto_layout(G, warn):
    path_root = None

    for v, d in G.degree(G.nodes_iter()).items():
        if d == 1 and G.neighbors(v):
            path_root = v
        elif d > 2:
            path_root = None
            break

    if path_root is not None:
        return _path_layout(G, path_root)
    else:
        return _generic_layout(G, warn)


def _components(G):
    if isinstance(G, (nx.MultiDiGraph, nx.DiGraph)):
        return nx.strongly_connected_components(G)
    else:
        return nx.connected_components(G)


_default_minimum_distance = 0.3


def _min_distance(edges, pos):
    def distances():
        for e in edges:
            e1, e2 = e.leaves
            if e1 != e2:
                x1, y1 = pos[e1]
                x2, y2 = pos[e2]
                dx = x2 - x1
                dy = y2 - y1
                yield dx * dx + dy * dy

    d = robust_min(distances())
    if d is None:
        return _default_minimum_distance
    else:
        return sqrt(d)


def _pos_into_box(vertices, pos, min_box, max_box):
    new_pos = {}

    cx = (min_box[0] + max_box[0]) / 2
    cy = (min_box[1] + max_box[1]) / 2
    dx = max_box[0] - min_box[0]
    dy = max_box[1] - min_box[1]

    if len(vertices) == 1:
        for v in vertices:
            new_pos[v] = (cx, cy)
    else:
        points = pos.values()
        for p0 in points:
            x0, y0 = p0
            x1 = x0
            y1 = y0
            for p in points:
                x, y = p
                x0 = min(x0, x)
                y0 = min(y0, y)
                x1 = max(x1, x)
                y1 = max(y1, y)
            break

        zx = (x0 + x1) / 2
        zy = (y0 + y1) / 2
        s = 1.0 / max((x1 - x0) / dx, (y1 - y0) / dy)
        for k, p in pos.items():
            x, y = p
            new_pos[k] = (cx + (x - zx) * s, cy + (y - zy) * s)

    return new_pos


def _move_pos(pos, dx, dy):
    new_pos = {}
    for k, p in pos.items():
        x, y = p
        new_pos[k] = (x + dx, y + dy)
    return new_pos


_vertex_size_names = system_symbols_dict({
    'Large': 0.8,
    'Medium': 0.3,
    'Small': 0.2,
    'Tiny': 0.05,
})


def _vertex_size(expr):
    if isinstance(expr, Symbol):
        return _vertex_size_names.get(expr.get_name())
    else:
        return expr.round_to_float()


def _vertex_style(expr):
    return expr


def _edge_style(expr):
    return expr


def _parse_property(expr, attr_dict=None):
    if expr.has_form('Rule', 2):
        name, value = expr.leaves
        if isinstance(name, Symbol):
            if attr_dict is None:
                attr_dict = {}
            attr_dict[name.get_name()] = value
    elif expr.has_form('List', None):
        for item in expr.leaves:
            attr_dict = _parse_property(item, attr_dict)
    return attr_dict


class _NetworkXBuiltin(Builtin):
    requires = (
        'networkx',
    )

    options = {
        'VertexSize': '{}',
        'VertexStyle': '{}',
        'EdgeStyle': '{}',
        'EdgeWeight': '{}',
    }

    messages = {
        'graph': 'Expected a graph at position 1 in ``.',
        'inv': 'Vertex at position `1` in `2` must belong to the graph at position 1.',
    }

    def _build_graph(self, graph, evaluation, options, expr, quiet=False):
        head = graph.get_head_name()
        if head == 'System`Graph':
            return graph
        elif head == 'System`List':
            return _graph_from_list(graph.leaves, options)
        elif quiet == False:
            evaluation.message(self.get_name(), 'graph', expr)

    def _evaluate_atom(self, graph, options, compute):
        head = graph.get_head_name()
        if head == 'System`Graph':
            return compute(graph)
        elif head == 'System`List':
            return compute(_graph_from_list(graph.leaves, options))

    def _evaluate(self, graph, options, compute):
        head = graph.get_head_name()
        if head == 'System`Graph':
            return compute(graph.G)
        elif head == 'System`List':
            return compute(_graph_from_list(graph.leaves, options).G)
        else:
            evaluation.message(self.get_name(), 'graph', expr)

    def _evaluate_to_list(self, graph, options, compute):
        r = self._evaluate(graph, options, compute)
        if r:
            return Expression('List', *r)


class GraphBox(GraphicsBox):
    def boxes_to_text(self, leaves, **options):
        return '-Graph-'


class _Collection(object):
    def __init__(self, expressions, properties=None, index=None):
        self.expressions = expressions
        self.properties = properties if properties else None
        self.index = index

    def clone(self):
        properties = self.properties
        return _Collection(
            self.expressions[:],
            properties[:] if properties else None,
            None)

    def filter(self, expressions):
        index = self.get_index()
        return [expr for expr in expressions if expr in index]

    def extend(self, expressions, properties):
        if properties:
            if self.properties is None:
                self.properties = [None] * len(self.expressions)
            self.properties.extend(properties)
        self.expressions.extend(expressions)
        self.index = None
        return expressions

    def delete(self, expressions):
        index = self.get_index()
        trash = set(index[x] for x in expressions)
        deleted = [self.expressions[i] for i in trash]
        self.expressions = [x for i, x in enumerate(self.expressions) if i not in trash]
        self.properties = [x for i, x in enumerate(self.properties) if i not in trash]
        self.index = None
        return deleted

    def data(self):
        return self.expressions, list(self.get_properties())

    def get_index(self):
        index = self.index
        if index is None:
            index = dict((v, i) for i, v in enumerate(self.expressions))
            self.index = index
        return index

    def get_properties(self):
        if self.properties:
            for p in self.properties:
                yield p
        else:
            for _ in range(len(self.expressions)):
                yield None

    def get_sorted(self):
        index = self.get_index()
        return lambda c: sorted(c, key=lambda v: index[v])

    def get_property(self, item, name):
        properties = self.properties
        if properties is None:
            return None
        index = self.get_index()
        i = index.get(item)
        if i is None:
            return None
        p = properties[i]
        if p is None:
            return None
        return p.get(name)


def _count_edges(counts, edges, sign):
    n_directed, n_undirected = counts
    for edge in edges:
        if edge.get_head_name() == 'System`DirectedEdge':
            n_directed += sign
        else:
            n_undirected += sign
    return n_directed, n_undirected


class _EdgeCollection(_Collection):
    def __init__(self, expressions, properties=None, index=None, n_directed=0, n_undirected=0):
        super(_EdgeCollection, self).__init__(expressions, properties, index)
        self.counts = (n_directed, n_undirected)

    def is_mixed(self):
        n_directed, n_undirected = self.counts
        return n_directed > 0 and n_undirected > 0

    def clone(self):
        properties = self.properties
        n_directed, n_undirected = self.counts
        return _EdgeCollection(
            self.expressions[:],
            properties[:] if properties else None,
            None,  # index
            n_directed,
            n_undirected)

    def extend(self, expressions, properties):
        added = super(_EdgeCollection, self).extend(expressions, properties)
        self.counts = _count_edges(self.counts, added, 1)
        return added

    def delete(self, expressions):
        deleted = super(_EdgeCollection, self).delete(expressions)
        self.counts = _count_edges(self.counts, deleted, -1)
        return deleted


class _FullGraphRewrite(Exception):
    pass


def _normalize_edges(edges):
    for edge in edges:
        head_name = edge.get_head_name()
        if head_name == 'System`Property' and len(edge.leaves) == 2:
            expr, prop = edge.leaves
            yield Expression(edge.get_head(), list(_normalize_edges([expr]))[0], prop)
        elif head_name == 'System`Rule':
            yield Expression('System`DirectedEdge', *edge.leaves)
        else:
            yield edge


class Graph(Atom):
    def __init__(self, vertices, edges, G, layout, options, highlights=None, **kwargs):
        super(Graph, self).__init__(**kwargs)
        self.vertices = vertices
        self.edges = edges
        self.G = G
        self.layout = layout
        self.options = options
        self.highlights = highlights

    def empty(self):
        return len(self.G) == 0

    def is_mixed_graph(self):
        return self.edges.is_mixed()

    def is_multigraph(self):
        return isinstance(self.G, (nx.MultiDiGraph, nx.MultiGraph))

    def is_loop_free(self):
        return not self.G.nodes_with_selfloops()

    def add_vertices(self, new_vertices, new_vertex_properties):
        vertices = self.vertices.clone()
        vertices.extend(new_vertices, new_vertex_properties)
        G = self.G.copy()
        G.add_nodes_from(zip(new_vertices, new_vertex_properties))
        return Graph(vertices, self.edges, G, self.layout, self.options, self.highlights)

    def delete_vertices(self, vertices_to_delete):
        vertices_to_delete = set(vertices_to_delete)

        G = self.G.copy()
        G.remove_nodes_from(vertices_to_delete)

        vertices = self.vertices.clone()
        vertices.delete(vertices_to_delete)

        def edges_to_delete():
            for edge in self.edges.expressions:
                u, v = edge.leaves
                if u in vertices_to_delete or v in vertices_to_delete:
                    yield edge

        edges = self.edges.clone()
        edges.delete(edges_to_delete())

        return Graph(vertices, edges, G, self.layout, self.options, self.highlights)

    def add_edges(self, new_edges, new_edge_properties):
        G = self.G.copy()

        vertices = self.vertices.clone()
        vertex_index = self.vertices.get_index()

        multigraph = self.is_multigraph()
        directed = isinstance(G, (nx.MultiDiGraph, nx.DiGraph))

        edges = self.edges.clone()
        new_edges = list(_normalize_edges(new_edges))
        edges.extend(new_edges, new_edge_properties)

        def add_edge(u, v):
            vertex_u, attr_dict_u = _parse_item(u)
            vertex_v, attr_dict_v = _parse_item(v)

            if not multigraph and G.has_edge(vertex_u, vertex_v):
                raise _FullGraphRewrite

            G.add_edge(vertex_u, vertex_v)

            if vertex_u not in vertex_index:
                vertices.extend([vertex_u], [attr_dict_u])
            if vertex_v not in vertex_index:
                vertices.extend([vertex_v], [attr_dict_v])

        try:
            for edge in new_edges:
                if edge.has_form('DirectedEdge', 2):
                    u, v = edge.leaves
                    if directed:
                        add_edge(u, v)
                    else:
                        raise _FullGraphRewrite
                elif edge.has_form('UndirectedEdge', 2):
                    u, v = edge.leaves
                    if directed:
                        add_edge(u, v)
                        add_edge(v, u)
                    else:
                        add_edge(u, v)

            return Graph(vertices, edges, G, self.layout, self.options, self.highlights)
        except _FullGraphRewrite:
            return _create_graph(new_edges, new_edge_properties, from_graph=G)

    def delete_edges(self, edges_to_delete):
        G = self.G.copy()
        directed = isinstance(G, (nx.MultiDiGraph, nx.DiGraph))

        edges_to_delete = list(_normalize_edges(edges_to_delete))
        edges_to_delete = self.edges.filter(edges_to_delete)

        for edge in edges_to_delete:
            if edge.has_form('DirectedEdge', 2):
                if directed:
                    u, v = edge.leaves
                    G.remove_edge(u, v)
            elif edge.has_form('UndirectedEdge', 2):
                u, v = edge.leaves
                if directed:
                    G.remove_edge(u, v)
                    G.remove_edge(v, u)
                else:
                    G.remove_edge(u, v)

        edges = self.edges.clone()
        edges.delete(edges_to_delete)

        return Graph(self.vertices, edges, G, self.layout, self.options, self.highlights)

    def __str__(self):
        return '-Graph-'

    def with_highlight(self, highlights):
        return Graph(
            self.vertices, self.edges, self.G, self.layout, self.options, highlights)

    def do_copy(self):
        return Graph(
            self.vertices, self.edges, self.G, self.layout, self.options, self.highlights)

    def default_format(self, evaluation, form):
        return '-Graph-'

    def get_sort_key(self, pattern_sort=False):
        if pattern_sort:
            return super(Graph, self).get_sort_key(True)
        else:
            return hash(self)

    def same(self, other):
        return isinstance(other, Graph) and self.G == other.G
        # FIXME
        # self.properties == other.properties
        # self.options == other.options
        # self.highlights == other.highlights

    def to_python(self, *args, **kwargs):
        return self.G

    def __hash__(self):
        return hash(("Graph", self.G))  # FIXME self.properties, ...

    def _styling(self, name, elements, parse, default_value):
        expr = self.options.get(name)
        if expr is None:
            return lambda x: default_value

        values = {}
        if expr.has_form('List', None):
            if all(leaf.has_form('Rule', 2) for leaf in expr.leaves):
                for rule in expr.leaves:
                    v, r = rule.leaves
                    values[v] = parse(r) or default_value
            else:
                for v, r in zip(elements, expr.leaves):
                    values[v] = parse(r) or default_value
        else:
            default_value = parse(expr) or default_value

        return lambda x: values.get(x, default_value)

    def _primitives(self, pos, vertices, edges, minimum_distance, evaluation):
        highlights = self.highlights
        default_radius = 0.1

        vertex_size = self._styling(
            'System`VertexSize', vertices, _vertex_size, default_radius)

        vertex_style = self._styling(
            'System`VertexStyle', vertices, _vertex_style, None)

        edge_style = self._styling(
            'System`EdgeStyle', edges, _edge_style, None)

        if highlights:
            def highlighted(exprs):
                items = Expression('List', *exprs)
                listspec = Expression('List', Integer(1))

                matches = Expression('Replace', items, highlights, listspec).evaluate(evaluation)
                if matches.get_head_name() != 'System`List':
                    return
                if len(matches.leaves) != len(exprs):
                    return

                for expr, m in zip(exprs, matches.leaves):
                    if m.get_head_name() == 'System`Missing':
                        yield expr, None
                    else:
                        yield expr, m
        else:
            def highlighted(exprs):
                for expr in exprs:
                    yield expr, None

        def edge_primitives():
            yield Expression('AbsoluteThickness', 0.25)

            def edge_ids():
                vertex_index = self.vertices.get_index()
                for edge in edges:
                    v1, v2 = edge.leaves
                    i1 = vertex_index[v1]
                    i2 = vertex_index[v2]
                    flipped = i2 < i1
                    yield ((i2, i1) if flipped else (i1, i2)), flipped

            def bends():
                ids = list(edge_ids())
                multiplicites = defaultdict(int)
                for edge_id, _ in ids:
                    multiplicites[edge_id] += 1
                edge_numbers = {}
                for edge_id, flipped in ids:
                    m = multiplicites[edge_id]
                    if m == 1:
                        yield None
                    else:
                        i = edge_numbers.get(edge_id, 1)
                        z = -1 + 2 * ((i - 1) / (m - 1))
                        if flipped:
                            z = -z
                        yield z
                        edge_numbers[edge_id] = i + 1

            G = self.G

            for (edge, style), properties, bend in zip(highlighted(edges), self.edges.get_properties(), bends()):
                if edge.get_head_name() == 'System`DirectedEdge':
                    yield Expression('Arrowheads', 0.03)
                else:
                    yield Expression('Arrowheads', 0)

                v1, v2 = edge.leaves

                if v1.same(v2):  # self-loop?
                    x, y = pos[v1]
                    r = vertex_size(v1) * minimum_distance

                    # determine on which side to draw the loop. choose that side where we expect the least
                    # number of intersections with other edges connected to this vertex.
                    n_left_edges = 0
                    n_right_edges = 0
                    for v2 in G.neighbors(v1):
                        nx, ny = pos[v2]
                        if nx - x > 0:
                            n_right_edges += 1
                        else:
                            n_left_edges += 1

                    ly = 4. * r
                    lx = -ly if n_left_edges <= n_right_edges else ly
                    points = [
                        Expression('List', x, y),
                        Expression('List', x + lx, y - ly),
                        Expression('List', x + lx, y + ly),
                        Expression('List', x, y)]

                    arrow = Expression('Arrow', Expression('BezierCurve', Expression('List', *points)), r)
                else:
                    r1 = vertex_size(v1) * minimum_distance
                    r2 = vertex_size(v2) * minimum_distance

                    if bend is None:
                        p1 = pos[v1]
                        p2 = pos[v2]
                        q1 = Expression('List', *p1)
                        q2 = Expression('List', *p2)
                        arrow = Expression('Arrow', Expression('List', q1, q2), Expression('List', r1, r2))
                    else:
                        x1, y1 = pos[v1]
                        x2, y2 = pos[v2]

                        dx = x2 - x1
                        dy = y2 - y1
                        d = sqrt(dx * dx + dy * dy)
                        nx = -dy / d
                        ny = dx / d

                        z = 0.5 * minimum_distance * bend
                        points = [
                            Expression('List', x1, y1),
                            Expression('List', 0.5 * (x1 + x2) + nx * z, 0.5 * (y1 + y2) + ny * z),
                            Expression('List', x2, y2)]

                        curve =  Expression('BezierCurve', Expression('List', *points))
                        arrow = Expression('Arrow', curve, Expression('List', r1, r2))

                if style is None and properties is not None:
                    style = properties.get('System`EdgeStyle')

                if style is None:
                    style = edge_style(edge)

                if style is not None:
                    arrow = Expression('Style', arrow, style)

                yield arrow

        def vertex_primitives():
            for (v, style), properties in zip(highlighted(vertices), self.vertices.get_properties()):
                xy = pos.get(v)

                x, y = xy
                r = vertex_size(v) * minimum_distance

                disk = Expression(
                    'Disk',
                    Expression('List', x, y),
                    Expression('List', r, r))

                if style is None and properties is not None:
                    style = properties.get('System`VertexStyle')

                if style is None:
                    style = vertex_style(v)

                if style is not None:
                    yield Expression('Style', disk, style)
                else:
                    yield disk

                # yield Expression('FontSize', Expression('Scaled', r))
                # yield Expression('Text', v, Expression('List', x, y))

        vertex_face = Expression('FaceForm', Expression('RGBColor', .8, .8, .9))
        vertex_edge = Expression('EdgeForm', Expression('RGBColor', 0, 0, 0))

        edge_expression = Expression(
            'Style',
            Expression('List', *list(edge_primitives())),
            Expression('List'))
        vertex_expression = Expression(
            'Style',
            Expression('List', *list(vertex_primitives())),
            Expression('List', vertex_face, vertex_edge))

        return Expression('List', edge_expression, vertex_expression)

    def _layout(self, evaluation):
        G = self.G

        components = list(nx.connected_components(G.to_undirected()))
        if not components:  # empty graph?
            return []
        n_components = len(components)

        if n_components == 1:
            component_edges = [self.edges.expressions]
        else:
            vertex_component = {}
            for i, component in enumerate(components):
                for vertex in component:
                    vertex_component[vertex] = i

            component_edges = [[] for _ in range(n_components)]
            for edge in self.edges.expressions:
                component_edges[vertex_component[edge.leaves[0]]].append(edge)

        def boxes(box):
            minimum_distance = _default_minimum_distance
            stored_pos = []

            warnings = set()

            def warn(message):
                if message not in warnings:
                    warnings.add(message)
                    evaluation.print_out(message)

            for i, (vertices, edges) in enumerate(zip(components, component_edges)):
                if len(vertices) > 1:
                    base_pos = _auto_layout(G.subgraph(vertices), warn)
                else:
                    base_pos = None

                pos = _pos_into_box(vertices, base_pos, (0, 0), (1, 1))
                stored_pos.append(pos)

                minimum_distance = min(minimum_distance, _min_distance(edges, pos))

            for i, (vertices, edges, pos) in enumerate(zip(components, component_edges, stored_pos)):
                pos = _move_pos(pos, *box(i, minimum_distance))

                yield self._primitives(
                    pos, vertices, edges, minimum_distance, evaluation)

        if n_components <= 3:
            n = n_components
        else:
            n = int(ceil(sqrt(n_components)))

        def grid(i, d):
            x = i % n
            y = (n - 1) - i // n
            s = 1 + 1.1 * d
            return x * s, y * s

        return Expression('List', *list(boxes(grid)))

    def atom_to_boxes(self, form, evaluation):
        primitives = self._layout(evaluation)
        graphics = Expression('Graphics', primitives)
        graphics_box = Expression('MakeBoxes', graphics, form).evaluate(evaluation)
        return Expression('GraphBox', *graphics_box.leaves)

    def get_property(self, item, name):
        if item.get_head_name() in ('System`DirectedEdge', 'System`UndirectedEdge'):
            x = self.edges.get_property(item, name)
        if x is None:
            x = self.vertices.get_property(item, name)
        return x

    def update_weights(self, evaluation):
        weights = None
        G = self.G

        if self.is_multigraph():
            for u, v, k, data in G.edges_iter(data=True, keys=True):
                w = data.get('System`EdgeWeight')
                if w is not None:
                    w = w.evaluate(evaluation).to_mpmath()
                    G[u][v][k]['WEIGHT'] = w
                    weights = 'WEIGHT'
        else:
            for u, v, data in G.edges_iter(data=True):
                w = data.get('System`EdgeWeight')
                if w is not None:
                    w = w.evaluate(evaluation).to_mpmath()
                    G[u][v]['WEIGHT'] = w
                    weights = 'WEIGHT'

        return weights

    def coalesced_graph(self, evaluation):
        if isinstance(self.G, (nx.DiGraph, nx.Graph)):
            return self.G, 'WEIGHT'

        new_edges = defaultdict(lambda: 0)
        for u, v, data in self.G.edges_iter(data=True):
            w = data.get('System`EdgeWeight')
            if w is not None:
                w = w.evaluate(evaluation).to_mpmath()
            else:
                w = 1
            new_edges[(u, v)] += w

        if isinstance(self.G, nx.MultiDiGraph):
            new_graph = nx.DiGraph()
        else:
            new_graph = nx.Graph()

        # FIXME make sure vertex order is unchanged from self.G
        new_graph.add_edges_from(((u, v, {'WEIGHT': w}) for (u, v), w in new_edges.items()))

        return new_graph, 'WEIGHT'


def _is_connected(G):
    if len(G) == 0:  # empty graph?
        return True
    if isinstance(G, (nx.MultiDiGraph, nx.DiGraph)):
        return sum(1 for _ in (islice(nx.strongly_connected_components(G), 2))) == 1
    else:
        return nx.is_connected(G)


def _edge_weights(options):
    expr = options.get('System`EdgeWeight')
    if expr is None:
        return []
    if not expr.has_form('List', None):
        return []
    return expr.leaves


class _GraphParseError(Exception):
    pass


def _parse_item(x, attr_dict=None):
    if x.get_head_name() == 'System`Property' and len(x.leaves) == 2:
        expr, prop = x.leaves
        attr_dict = _parse_property(prop, attr_dict)
        return _parse_item(expr, attr_dict)
    else:
        return x, attr_dict


def _graph_from_list(rules, options):
    if not rules:
        return Graph(_Collection([]), _EdgeCollection([]), nx.Graph(), None, options)
    else:
        new_edges, new_edge_properties = zip(*[_parse_item(x) for x in rules])
        return _create_graph(new_edges, new_edge_properties, options)


def _create_graph(new_edges, new_edge_properties, options, from_graph=None):
    directed_edges = []
    undirected_edges = []

    if from_graph is not None:
        vertices, vertex_properties = from_graph.vertices.data()
        edges, edge_properties = from_graph.edges.data()

        for edge, attr_dict in zip(edges, edge_properties):
            u, v = edge.leaves
            if edge.get_head_name() == 'System`DirectedEdge':
                directed_edges.append((u, v, attr_dict))
            else:
                undirected_edges.append((u, v, attr_dict))

        multigraph = [from_graph.is_multigraph()]
    else:
        vertices = []
        vertex_properties = []
        edges = []
        edge_properties = []

        multigraph = [False]

    known_vertices = set(vertices)
    known_edges = set(edges)

    def add_vertex(x, attr_dict=None):
        if x.get_head_name() == 'System`Property' and len(x.leaves) == 2:
            expr, prop = x.leaves
            attr_dict = _parse_property(prop, attr_dict)
            return add_vertex(expr, attr_dict)
        elif x not in known_vertices:
            known_vertices.add(x)
            vertices.append(x)
            vertex_properties.append(attr_dict)
        return x

    def track_edges(*edges):
        if multigraph[0]:
            return
        previous_n_edges = len(known_edges)
        for edge in edges:
            known_edges.add(edge)
        if len(known_edges) < previous_n_edges + len(edges):
            multigraph[0] = True

    edge_weights = _edge_weights(options)
    use_directed_edges = options.get('System`DirectedEdges', Symbol('True')).is_true()

    directed_edge_head = Symbol('DirectedEdge' if use_directed_edges else 'UndirectedEdge')
    undirected_edge_head = Symbol('UndirectedEdge')

    def parse_edge(r, attr_dict):
        if r.is_atom():
            raise _GraphParseError

        name = r.get_head_name()
        leaves = r.leaves

        if len(leaves) != 2:
            raise _GraphParseError

        u, v = leaves

        u = add_vertex(u)
        v = add_vertex(v)

        if name in ('System`Rule', 'System`DirectedEdge'):
            edges_container = directed_edges
            head = directed_edge_head
            track_edges((u, v))
        elif name == 'System`UndirectedEdge':
            edges_container = undirected_edges
            head = undirected_edge_head
            track_edges((u, v), (v, u))
        else:
            raise _GraphParseError

        if head.get_name() == name:
            edges.append(r)
        else:
            edges.append(Expression(head, u, v))
        edge_properties.append(attr_dict)

        edges_container.append((u, v, attr_dict))

    try:
        def full_new_edge_properties():
            for i, (attr_dict, w) in enumerate(zip(new_edge_properties, edge_weights)):
                attr_dict = {} if attr_dict is None else attr_dict.copy()
                attr_dict['System`EdgeWeight'] = w
                yield attr_dict
            for _ in range(len(new_edge_properties) - len(edge_weights)):
                yield None

        for edge, attr_dict in zip(new_edges, full_new_edge_properties()):
            parse_edge(edge, attr_dict)
    except _GraphParseError:
        return

    empty_dict = {}
    if directed_edges:
        G = nx.MultiDiGraph() if multigraph[0] else nx.DiGraph()
        for u, v, attr_dict in directed_edges:
            attr_dict = attr_dict or empty_dict
            G.add_edge(u, v, **attr_dict)
        for u, v, attr_dict in undirected_edges:
            attr_dict = attr_dict or empty_dict
            G.add_edge(u, v, **attr_dict)
            G.add_edge(v, u, **attr_dict)
    else:
        G = nx.MultiGraph() if multigraph[0] else nx.Graph()
        for u, v, attr_dict in undirected_edges:
            attr_dict = attr_dict or empty_dict
            G.add_edge(u, v, **attr_dict)

    edge_collection = _EdgeCollection(
        edges, edge_properties,
        n_directed=len(directed_edges),
        n_undirected=len(undirected_edges))

    return Graph(
        _Collection(vertices, vertex_properties),
        edge_collection,
        G, None, options)


class Property(Builtin):
    pass


class PropertyValue(Builtin):
    '''
    >> g = Graph[{a <-> b, Property[b <-> c, SomeKey -> 123]}];
    >> PropertyValue[{g, b <-> c}, SomeKey]
     = 123
    >> PropertyValue[{g, b <-> c}, SomeUnknownKey]
     = $Failed
    '''

    requires = (
        'networkx',
    )

    def apply(self, graph, item, name, evaluation):
        'PropertyValue[{graph_Graph, item_}, name_Symbol]'
        value = graph.get_property(item, name.get_name())
        if value is None:
            return Symbol('$Failed')
        return value


class DirectedEdge(Builtin):
    '''
    <dl>
    <dt>'DirectedEdge[$u$, $v$]'
      <dd>a directed edge from $u$ to $v$.
    </dl>
    '''
    pass


class UndirectedEdge(Builtin):
    '''
    <dl>
    <dt>'UndirectedEdge[$u$, $v$]'
      <dd>an undirected edge between $u$ and $v$.
    </dl>
    '''
    pass


class GraphAtom(AtomBuiltin):
    '''
    >> Graph[{1->2, 2->3, 3->1}]
     = -Graph-

    >> Graph[{1->2, 2->3, 3->1}, EdgeStyle -> {Red, Blue, Green}]
     = -Graph-

    >> Graph[{1->2, Property[2->3, EdgeStyle -> Thick], 3->1}]
     = -Graph-

    >> Graph[{1->2, 2->3, 3->1}, VertexStyle -> {1 -> Green, 3 -> Blue}]
     = -Graph-

    >> Graph[x]
     = Graph[x]

    >> Graph[{1}]
     = Graph[{1}]

    >> Graph[{{1 -> 2}}]
     = Graph[{{1 -> 2}}]

    >> g = Graph[{1 -> 2, 2 -> 3}, DirectedEdges -> True];
    >> EdgeCount[g, _DirectedEdge]
     = 2
    >> g = Graph[{1 -> 2, 2 -> 3}, DirectedEdges -> False];
    >> EdgeCount[g, _DirectedEdge]
     = 0
    >> EdgeCount[g, _UndirectedEdge]
     = 2
    '''

    requires = (
        'networkx',
    )

    options = {
        'VertexSize': '{}',
        'VertexStyle': '{}',
        'EdgeStyle': '{}',
        'DirectedEdges': 'True',
    }

    def apply(self, graph, evaluation, options):
        'Graph[graph_List, OptionsPattern[%(name)s]]'
        return _graph_from_list(graph.leaves, options)


class PathGraph(_NetworkXBuiltin):
    '''
    >> PathGraph[{1, 2, 3}]
     = -Graph-
    '''

    def apply(self, l, evaluation, options):
        'PathGraph[l_List, OptionsPattern[%(name)s]]'
        leaves = l.leaves
        def edges():
            for u, v in zip(leaves, leaves[1:]):
                yield Expression('UndirectedEdge', u, v)

        return _graph_from_list(edges(), options)


class PathGraphQ(_NetworkXBuiltin):
    '''
    >> PathGraphQ[{1 -> 2, 2 -> 3}]
     = True

    >> PathGraphQ[{1 -> 2, 2 -> 3, 2 -> 4}]
     = False

    #> PathGraphQ[Graph[{}]]
     = False
    #> PathGraphQ[Graph[{1 -> 2, 3 -> 4}]]
     = False
    #> PathGraphQ[Graph[{1 -> 2, 2 -> 1}]]
     = True
    #> PathGraphQ[Graph[{}]]
     = False
    #> PathGraphQ["abc"]
     = False
    '''

    def apply(self, graph, expression, evaluation, options):
        'PathGraphQ[graph_, OptionsPattern[%(name)s]]'
        graph = self._build_graph(graph, evaluation, options, expression, quiet=True)
        if graph:
            if graph.empty():
                is_path = False
            else:
                G = graph.G
                is_path = _is_connected(G)
                if is_path:
                    is_path = all(d <= 2 for d in G.degree(graph.vertices.expressions).values())
            return Symbol('True' if is_path else 'False')
        else:
            return Symbol('False')


class MixedGraphQ(_NetworkXBuiltin):
    '''
    >> g = Graph[{1 -> 2, 2 -> 3}]; MixedGraphQ[g]
     = False

    >> g = Graph[{1 -> 2, 2 <-> 3}]; MixedGraphQ[g]
     = True

    #> g = Graph[{}]; MixedGraphQ[g]
     = False

    #> MixedGraphQ["abc"]
     = False

    #> g = Graph[{1 -> 2, 2 -> 3}]; MixedGraphQ[g]
     = False
    #> g = EdgeAdd[g, a <-> b]; MixedGraphQ[g]
     = True
    #> g = EdgeDelete[g, a <-> b]; MixedGraphQ[g]
     = False
    '''

    def apply(self, graph, expression, evaluation, options):
        '%(name)s[graph_, OptionsPattern[%(name)s]]'
        graph = self._build_graph(graph, evaluation, options, expression, quiet=True)
        if graph:
            return Symbol('True' if graph.is_mixed_graph() else 'False')
        else:
            return Symbol('False')


class MultigraphQ(_NetworkXBuiltin):
    '''
    >> g = Graph[{1 -> 2, 2 -> 3}]; MultigraphQ[g]
     = False

    >> g = Graph[{1 -> 2, 2 -> 3, 1 -> 2}]; MultigraphQ[g]
     = True

    #> g = Graph[{}]; MultigraphQ[g]
     = False

    #> MultigraphQ["abc"]
     = False
    '''

    def apply(self, graph, expression, evaluation, options):
        '%(name)s[graph_, OptionsPattern[%(name)s]]'
        graph = self._build_graph(graph, evaluation, options, expression, quiet=True)
        if graph:
            return Symbol('True' if graph.is_multigraph() else 'False')
        else:
            return Symbol('False')


class AcyclicGraphQ(_NetworkXBuiltin):
    '''
    >> g = Graph[{1 -> 2, 2 -> 3}]; AcyclicGraphQ[g]
     = False

    >> g = Graph[{1 -> 2, 2 -> 3, 5 -> 2, 3 -> 4, 3 -> 5}]; AcyclicGraphQ[g]
     = True

    #> g = Graph[{1 -> 2, 2 -> 3, 5 -> 2, 3 -> 4, 5 -> 3}]; AcyclicGraphQ[g]
     = False

    #> g = Graph[{1 -> 2, 2 -> 3, 5 -> 2, 3 -> 4, 5 <-> 3}]; AcyclicGraphQ[g]
     = True

    #> g = Graph[{1 <-> 2, 2 <-> 3, 5 <-> 2, 3 <-> 4, 5 <-> 3}]; AcyclicGraphQ[g]
     = True

    #> g = Graph[{}]; AcyclicGraphQ[{}]
     = False

    #> AcyclicGraphQ["abc"]
     = False
    '''

    def apply(self, graph, expression, evaluation, options):
        '%(name)s[graph_, OptionsPattern[%(name)s]]'
        graph = self._build_graph(graph, evaluation, options, expression, quiet=True)
        if graph:
            try:
                cycles = nx.find_cycle(graph.G)
            except nx.exception.NetworkXNoCycle:
                cycles = None
            return Symbol('True' if cycles else 'False')
        else:
            return Symbol('False')


class LoopFreeGraphQ(_NetworkXBuiltin):
    '''
    >> g = Graph[{1 -> 2, 2 -> 3}]; LoopFreeGraphQ[g]
     = True

    >> g = Graph[{1 -> 2, 2 -> 3, 1 -> 1}]; LoopFreeGraphQ[g]
     = False

    #> g = Graph[{}]; LoopFreeGraphQ[{}]
     = False

    #> LoopFreeGraphQ["abc"]
     = False
    '''

    def apply(self, graph, expression, evaluation, options):
        '%(name)s[graph_, OptionsPattern[%(name)s]]'
        graph = self._build_graph(graph, evaluation, options, expression, quiet=True)
        if graph:
            if graph.empty():
                return Symbol('False')
            else:
                return Symbol('True' if graph.is_loop_free() else 'False')
        else:
            return Symbol('False')


class DirectedGraphQ(_NetworkXBuiltin):
    '''
    >> g = Graph[{1 -> 2, 2 -> 3}]; DirectedGraphQ[g]
     = True

    >> g = Graph[{1 -> 2, 2 <-> 3}]; DirectedGraphQ[g]
     = False

    #> g = Graph[{}]; DirectedGraphQ[{}]
     = False

    #> DirectedGraphQ["abc"]
     = False
    '''

    def apply(self, graph, expression, evaluation, options):
        '%(name)s[graph_, OptionsPattern[%(name)s]]'
        graph = self._build_graph(graph, evaluation, options, expression, quiet=True)
        if graph:
            directed = isinstance(graph.G, (nx.MultiDiGraph, nx.DiGraph)) and not graph.is_mixed_graph()
            return Symbol('True' if directed else 'False')
        else:
            return Symbol('False')


class ConnectedGraphQ(_NetworkXBuiltin):
    '''
    >> g = Graph[{1 -> 2, 2 -> 3}]; ConnectedGraphQ[g]
     = False

    >> g = Graph[{1 -> 2, 2 -> 3, 3 -> 1}]; ConnectedGraphQ[g]
     = True

    >> g = Graph[{1 <-> 2, 2 <-> 3}]; ConnectedGraphQ[g]
     = True

    >> g = Graph[{1 <-> 2, 2 <-> 3, 4 <-> 5}]; ConnectedGraphQ[g]
     = False

    #> ConnectedGraphQ[Graph[{}]]
     = True

    #> ConnectedGraphQ["abc"]
     = False
    '''

    def apply(self, graph, expression, evaluation, options):
        '%(name)s[graph_, OptionsPattern[%(name)s]]'
        graph = self._build_graph(graph, evaluation, options, expression, quiet=True)
        if graph:
            return Symbol('True' if _is_connected(graph.G) else 'False')
        else:
            return Symbol('False')


class SimpleGraphQ(_NetworkXBuiltin):
    '''
    >> g = Graph[{1 -> 2, 2 -> 3, 3 <-> 4}]; SimpleGraphQ[g]
     = True

    >> g = Graph[{1 -> 2, 2 -> 3, 1 -> 1}]; SimpleGraphQ[g]
     = False

    >> g = Graph[{1 -> 2, 2 -> 3, 1 -> 2}]; SimpleGraphQ[g]
     = False

    #> SimpleGraphQ[Graph[{}]]
     = True

    #> SimpleGraphQ["abc"]
     = False
    '''

    def apply(self, graph, expression, evaluation, options):
        '%(name)s[graph_, OptionsPattern[%(name)s]]'
        graph = self._build_graph(graph, evaluation, options, expression, quiet=True)
        if graph:
            if graph.empty():
                return Symbol('True')
            else:
                simple = graph.is_loop_free() and not graph.is_multigraph()
                return Symbol('True' if simple else 'False')
        else:
            return Symbol('False')


class PlanarGraphQ(_NetworkXBuiltin):
    '''
    # see https://en.wikipedia.org/wiki/Planar_graph

    >> PlanarGraphQ[CompleteGraph[4]]
     = True

    >> PlanarGraphQ[CompleteGraph[5]]
     = False

    #> PlanarGraphQ[Graph[{}]]
     = False

    #> PlanarGraphQ["abc"]
     = False
    '''

    requires = _NetworkXBuiltin.requires + (
        'planarity',
    )

    def apply(self, graph, expression, evaluation, options):
        '%(name)s[graph_, OptionsPattern[%(name)s]]'
        graph = self._build_graph(graph, evaluation, options, expression, quiet=True)
        if graph:
            if graph.empty():
                return Symbol('False')
            else:
                import planarity
                return Symbol('True' if planarity.is_planar(graph.G) else 'False')
        else:
            return Symbol('False')


class ConnectedComponents(_NetworkXBuiltin):
    '''
    >> g = Graph[{1 -> 2, 2 -> 3, 3 <-> 4}]; ConnectedComponents[g]
     = {{3, 4}, {2}, {1}}

    >> g = Graph[{1 -> 2, 2 -> 3, 3 -> 1}]; ConnectedComponents[g]
     = {{1, 2, 3}}

    >> g = Graph[{1 <-> 2, 2 <-> 3, 3 -> 4, 4 <-> 5}]; ConnectedComponents[g]
     = {{4, 5}, {1, 2, 3}}
    '''

    def apply(self, graph, expression, evaluation, options):
        'ConnectedComponents[graph_, OptionsPattern[%(name)s]]'
        graph = self._build_graph(graph, evaluation, options, expression)
        if graph:
            vertices_sorted = graph.vertices.get_sorted()
            components = [Expression('List', *vertices_sorted(c)) for c in _components(graph.G)]
            return Expression('List', *components)


class WeaklyConnectedComponents(_NetworkXBuiltin):
    '''
    >> g = Graph[{1 -> 2, 2 -> 3, 3 <-> 4}]; WeaklyConnectedComponents[g]
     = {{1, 2, 3, 4}}

    >> g = Graph[{1 -> 2, 2 -> 3, 3 -> 1}]; WeaklyConnectedComponents[g]
     = {{1, 2, 3}}

    >> g = Graph[{1 <-> 2, 2 <-> 3, 3 -> 4, 4 <-> 5, 6 <-> 7, 7 <-> 8}]; WeaklyConnectedComponents[g]
     = {{1, 2, 3, 4, 5}, {6, 7, 8}}
    '''

    def apply(self, graph, expression, evaluation, options):
        'WeaklyConnectedComponents[graph_, OptionsPattern[%(name)s]]'
        graph = self._build_graph(graph, evaluation, options, expression)
        if graph:
            vertices_sorted = graph.vertices.get_sorted()
            return Expression(
                'List',
                *[Expression('List', *vertices_sorted(c)) for c in
                  nx.connected_components(graph.G.to_undirected())])


class FindVertexCut(_NetworkXBuiltin):
    '''
    <dl>
    <dt>'FindVertexCut[$g$]'
        <dd>finds a set of vertices of minimum cardinality that, if removed, renders $g$ disconnected.
    <dt>'FindVertexCut[$g$, $s$, $t$]'
        <dd>finds a vertex cut that disconnects all paths from $s$ to $t$.
    </dl>

    >> g = Graph[{1 -> 2, 2 -> 3}]; FindVertexCut[g]
     = {}

    >> g = Graph[{1 <-> 2, 2 <-> 3}]; FindVertexCut[g]
     = {2}

    >> g = Graph[{1 <-> 2, 2 <-> 3, 1 <-> x, x <-> 3}, 1, 3]; FindVertexCut[g]
     = {}

    #> FindVertexCut[Graph[{}]]
     = {}
    #> FindVertexCut[Graph[{}], 1, 2]
     : Vertex at position 2 in FindVertexCut[Graph[{}], 1, 2] must belong to the graph at position 1.
     = FindVertexCut[Graph[{}], 1, 2]
    '''

    def apply(self, graph, expression, evaluation, options):
        'FindVertexCut[graph_, OptionsPattern[%(name)s]]'
        graph = self._build_graph(graph, evaluation, options, expression)
        if graph:
            if graph.empty() or not _is_connected(graph.G):
                return Expression('List')
            else:
                return Expression('List', *nx.minimum_node_cut(graph.G))

    def apply_st(self, graph, s, t, expression, evaluation, options):
        'FindVertexCut[graph_, s_, t_, OptionsPattern[%(name)s]]'
        graph = self._build_graph(graph, evaluation, options, expression)
        if not graph:
            return

        G = graph.G
        if not G.has_node(s):
            evaluation.message(self.get_name(), 'inv', 2, expression)
        elif not G.has_node(t):
            evaluation.message(self.get_name(), 'inv', 3, expression)

        if graph.empty() or not _is_connected(graph.G):
            return Expression('List')
        else:
            return Expression('List', *nx.minimum_node_cut(G, s, t))


class HighlightGraph(_NetworkXBuiltin):
    '''

    '''

    def apply(self, graph, what, expression, evaluation, options):
        'HighlightGraph[graph_, what_List, OptionsPattern[%(name)s]]'
        default_highlight = [Expression('RGBColor', 1, 0, 0)]

        def parse(item):
            if item.get_head_name() == 'System`Rule':
                return Expression('DirectedEdge', *item.leaves)
            else:
                return item

        rules = []
        for item in what.leaves:
            if item.get_head_name() == 'System`Style':
                if len(item.leaves) >= 2:
                    rules.append((parse(item.leaves[0]), item.leaves[1:]))
            else:
                rules.append((parse(item), default_highlight))

        rules.append((Expression('Blank'), Expression('Missing')))

        graph = self._build_graph(graph, evaluation, options, expression)
        if graph:
            rule_exprs = Expression('List', *[Expression('Rule', *r) for r in rules])
            return graph.with_highlight(rule_exprs)


class _PatternList(_NetworkXBuiltin):
    def apply(self, graph, expression, evaluation, options):
        '%(name)s[graph_, OptionsPattern[%(name)s]]'
        graph = self._build_graph(graph, evaluation, options, expression)
        if graph:
            return Expression('List', *self._items(graph))

    def apply_patt(self, graph, patt, expression, evaluation, options):
        '%(name)s[graph_, patt_, OptionsPattern[%(name)s]]'
        graph = self._build_graph(graph, evaluation, options, expression)
        if graph:
            return Expression('Cases', Expression('List', *self._items(graph)), patt)


class _PatternCount(_NetworkXBuiltin):
    def apply(self, graph, expression, evaluation, options):
        '%(name)s[graph_, OptionsPattern[%(name)s]]'
        graph = self._build_graph(graph, evaluation, options, expression)
        if graph:
            return Integer(len(self._items(graph)))

    def apply_patt(self, graph, patt, expression, evaluation, options):
        '%(name)s[graph_, patt_, OptionsPattern[%(name)s]]'
        graph = self._build_graph(graph, evaluation, options, expression)
        if graph:
            return Expression('Length', Expression(
                'Cases', Expression('List', *self._items(graph)), patt))


class VertexCount(_PatternCount):
    '''
    >> VertexCount[{1 -> 2, 2 -> 3}]
     = 3

    >> VertexCount[{1 -> x, x -> 3}, _Integer]
     = 2
    '''

    def _items(self, graph):
        return graph.vertices.expressions


class VertexList(_PatternList):
    '''
    >> VertexList[{1 -> 2, 2 -> 3}]
     = {1, 2, 3}

    >> VertexList[{a -> c, c -> b}]
     = {a, c, b}
    '''

    def _items(self, graph):
        return graph.vertices.expressions


class EdgeCount(_PatternCount):
    '''
    >> EdgeCount[{1 -> 2, 2 -> 3}]
     = 2
    '''

    def _items(self, graph):
        return graph.edges.expressions


class EdgeList(_PatternList):
    '''
    >> EdgeList[{1 -> 2, 2 <-> 3}]
     = {DirectedEdge[1, 2], UndirectedEdge[2, 3]}
    '''

    def _items(self, graph):
        return graph.edges.expressions


class EdgeConnectivity(_NetworkXBuiltin):
    '''
    >> EdgeConnectivity[{1 <-> 2, 2 <-> 3}]
     = 1

    >> EdgeConnectivity[{1 -> 2, 2 -> 3}]
     = 0

    >> EdgeConnectivity[{1 -> 2, 2 -> 3, 3 -> 1}]
     = 1

    >> EdgeConnectivity[{1 <-> 2, 2 <-> 3, 1 <-> 3}]
     = 2

    >> EdgeConnectivity[{1 <-> 2, 3 <-> 4}]
     = 0

    #> EdgeConnectivity[Graph[{}]]
     = EdgeConnectivity[-Graph-]
    '''

    def apply(self, graph, expression, evaluation, options):
        '%(name)s[graph_, OptionsPattern[%(name)s]]'
        graph = self._build_graph(graph, evaluation, options, expression)
        if graph and not graph.empty():
            return Integer(nx.edge_connectivity(graph.G))

    def apply_st(self, graph, s, t, expression, evaluation, options):
        '%(name)s[graph_, s_, t_, OptionsPattern[%(name)s]]'
        graph = self._build_graph(graph, evaluation, options, expression)
        if graph and not graph.empty():
            return Integer(nx.edge_connectivity(graph.G, s, t))


class VertexConnectivity(_NetworkXBuiltin):
    '''
    >> VertexConnectivity[{1 <-> 2, 2 <-> 3}]
     = 1

    >> VertexConnectivity[{1 -> 2, 2 -> 3}]
     = 0

    >> VertexConnectivity[{1 -> 2, 2 -> 3, 3 -> 1}]
     = 1

    >> VertexConnectivity[{1 <-> 2, 2 <-> 3, 1 <-> 3}]
     = 2

    >> VertexConnectivity[{1 <-> 2, 3 <-> 4}]
     = 0

    #> VertexConnectivity[Graph[{}]]
     = EdgeConnectivity[-Graph-]
    '''

    def apply(self, graph, expression, evaluation, options):
        '%(name)s[graph_, OptionsPattern[%(name)s]]'
        graph = self._build_graph(graph, evaluation, options, expression)
        if graph and not graph.empty():
            if not _is_connected(graph.G):
                return Integer(0)
            else:
                return Integer(nx.node_connectivity(graph.G))

    def apply_st(self, graph, s, t, expression, evaluation, options):
        '%(name)s[graph_, s_, t_, OptionsPattern[%(name)s]]'
        graph = self._build_graph(graph, evaluation, options, expression)
        if graph and not graph.empty():
            if not _is_connected(graph.G):
                return Integer(0)
            else:
                return Integer(nx.node_connectivity(graph.G, s, t))


class _Centrality(_NetworkXBuiltin):
    pass


class BetweennessCentrality(_Centrality):
    '''
    >> g = Graph[{a -> b, b -> c, d -> c, d -> a, e -> c, d -> b}]; BetweennessCentrality[g]
     = {0., 1., 0., 0., 0.}

    >> g = Graph[{a -> b, b -> c, c -> d, d -> e, e -> c, e -> a}]; BetweennessCentrality[g]
     = {3., 3., 6., 6., 6.}
    '''

    def apply(self, graph, expression, evaluation, options):
        '%(name)s[graph_, OptionsPattern[%(name)s]]'
        graph = self._build_graph(graph, evaluation, options, expression)
        if graph:
            weight = graph.update_weights(evaluation)
            centrality = nx.betweenness_centrality(graph.G, normalized=False, weight=weight)
            return Expression('List', *[Real(centrality.get(v, 0.)) for v in graph.vertices.expressions])


class ClosenessCentrality(_Centrality):
    '''
    >> g = Graph[{a -> b, b -> c, d -> c, d -> a, e -> c, d -> b}]; ClosenessCentrality[g]
     = {0.666667, 1., 0., 1., 1.}

    >> g = Graph[{a -> b, b -> c, c -> d, d -> e, e -> c, e -> a}]; ClosenessCentrality[g]
     = {0.4, 0.4, 0.4, 0.5, 0.666667}
    '''

    def apply(self, graph, expression, evaluation, options):
        '%(name)s[graph_, OptionsPattern[%(name)s]]'
        graph = self._build_graph(graph, evaluation, options, expression)
        if graph:
            weight = graph.update_weights(evaluation)
            centrality = nx.closeness_centrality(graph.G, normalized=False, distance=weight)
            return Expression('List', *[Real(centrality.get(v, 0.)) for v in graph.vertices.expressions])


class DegreeCentrality(_Centrality):
    '''
    >> g = Graph[{a -> b, b <-> c, d -> c, d -> a, e <-> c, d -> b}]; DegreeCentrality[g]
     = {2, 4, 5, 3, 2}

    >> g = Graph[{a -> b, b <-> c, d -> c, d -> a, e <-> c, d -> b}]; DegreeCentrality[g, "In"]
     = {1, 3, 3, 0, 1}

    >> g = Graph[{a -> b, b <-> c, d -> c, d -> a, e <-> c, d -> b}]; DegreeCentrality[g, "Out"]
     = {1, 1, 2, 3, 1}
    '''

    def _from_dict(self, graph, centrality):
        s = len(graph.G) - 1  # undo networkx's normalization
        return Expression('List', *[Integer(s * centrality.get(v, 0)) for v in graph.vertices.expressions])

    def apply(self, graph, expression, evaluation, options):
        '%(name)s[graph_, OptionsPattern[%(name)s]]'
        graph = self._build_graph(graph, evaluation, options, expression)
        if graph:
            return self._from_dict(graph, nx.degree_centrality(graph.G))

    def apply_in(self, graph, expression, evaluation, options):
        '%(name)s[graph_, "In", OptionsPattern[%(name)s]]'
        graph = self._build_graph(graph, evaluation, options, expression)
        if graph:
            return self._from_dict(graph, nx.in_degree_centrality(graph.G))

    def apply_out(self, graph, expression, evaluation, options):
        '%(name)s[graph_, "Out", OptionsPattern[%(name)s]]'
        graph = self._build_graph(graph, evaluation, options, expression)
        if graph:
            return self._from_dict(graph, nx.out_degree_centrality(graph.G))


class _ComponentwiseCentrality(_Centrality):
    def _centrality(self, g, weight):
        raise NotImplementedError

    def _compute(self, graph, evaluation, reverse=False, normalized=True, **kwargs):
        vertices = graph.vertices.expressions
        G, weight = graph.coalesced_graph(evaluation)
        if reverse:
            G = G.reverse()

        components = list(_components(G))
        components = [c for c in components if len(c) > 1]

        result = [0] * len(vertices)
        for bunch in components:
            g = G.subgraph(bunch)
            centrality = self._centrality(g, weight, **kwargs)
            values = [centrality.get(v, 0) for v in vertices]
            if normalized:
                s = sum(values) * len(components)
            else:
                s = 1
            if s > 0:
                for i, x in enumerate(values):
                    result[i] += x / s
        return Expression('List', *[Real(x) for x in result])



class EigenvectorCentrality(_ComponentwiseCentrality):
    '''
    >> g = Graph[{a -> b, b -> c, c -> d, d -> e, e -> c, e -> a}]; EigenvectorCentrality[g, "In"]
     = {0.16238, 0.136013, 0.276307, 0.23144, 0.193859}

    >> EigenvectorCentrality[g, "Out"]
     = {0.136013, 0.16238, 0.193859, 0.23144, 0.276307}

    >> g = Graph[{a <-> b, b <-> c, c <-> d, d <-> e, e <-> c, e <-> a}]; EigenvectorCentrality[g]
     = {0.162435, 0.162435, 0.240597, 0.193937, 0.240597}

    >> g = Graph[{a <-> b, b <-> c, a <-> c, d <-> e, e <-> f, f <-> d, e <-> d}]; EigenvectorCentrality[g]
     = {0.166667, 0.166667, 0.166667, 0.183013, 0.183013, 0.133975}

    >> g = Graph[{a -> b, b -> c, c -> d, b -> e, a -> e}]; EigenvectorCentrality[g]
     = {0.166667, 0.166667, 0.166667, 0.183013, 0.183013, 0.133975}

    >> g = Graph[{a -> b, b -> c, c -> d, b -> e, a -> e, c -> a}]; EigenvectorCentrality[g]
     = {0.333333, 0.333333, 0.333333, 0., 0.}
    '''

    def _centrality(self, g, weight):
        return nx.eigenvector_centrality(g, max_iter=10000, tol=1.0e-7, weight=weight)

    def apply(self, graph, expression, evaluation, options):
        '%(name)s[graph_, OptionsPattern[%(name)s]]'
        graph = self._build_graph(graph, evaluation, options, expression)
        if graph:
            return self._compute(graph, evaluation)


    def apply_in_out(self, graph, dir, expression, evaluation, options):
        '%(name)s[graph_, dir_String, OptionsPattern[%(name)s]]'
        py_dir = dir.get_string_value()
        if py_dir not in ('In', 'Out'):
            return
        graph = self._build_graph(graph, evaluation, options, expression)
        if graph:
            return self._compute(graph, evaluation, py_dir == 'Out')


class KatzCentrality(_ComponentwiseCentrality):
    '''
    >> g = Graph[{a -> b, b -> c, c -> d, d -> e, e -> c, e -> a}]; KatzCentrality[g, 0.2]
     = {1.25202, 1.2504, 1.5021, 1.30042, 1.26008}

    >> g = Graph[{a <-> b, b <-> c, a <-> c, d <-> e, e <-> f, f <-> d, e <-> d}]; KatzCentrality[g, 0.1]
     = {1.25, 1.25, 1.25, 1.41026, 1.41026, 1.28205}
    '''

    rules = {
        'KatzCentrality[g_, alpha_]': 'KatzCentrality[g, alpha, 1]',
    }

    def _centrality(self, g, weight, alpha, beta):
        return nx.katz_centrality(g, alpha=alpha, beta=beta, normalized=False, weight=weight)

    def apply(self, graph, alpha, beta, expression, evaluation, options):
        '%(name)s[graph_, alpha_, beta_, OptionsPattern[%(name)s]]'
        graph = self._build_graph(graph, evaluation, options, expression)
        if graph:
            py_alpha = alpha.to_mpmath()
            py_beta = beta.to_mpmath()
            if py_alpha is None or py_beta is None:
                return
            return self._compute(graph, evaluation, normalized=False, alpha=py_alpha, beta=py_beta)


class PageRankCentrality(_Centrality):
    '''
    >> g = Graph[{a -> d, b -> c, d -> c, d -> a, e -> c, d -> c}]; PageRankCentrality[g, 0.2]
     = {0.184502, 0.207565, 0.170664, 0.266605, 0.170664}
    '''

    def apply_alpha_beta(self, graph, alpha, expression, evaluation, options):
        '%(name)s[graph_, alpha_, OptionsPattern[%(name)s]]'
        graph = self._build_graph(graph, evaluation, options, expression)
        if graph:
            py_alpha = alpha.to_mpmath()
            if py_alpha is None:
                return
            G, weight = graph.coalesced_graph(evaluation)
            centrality = nx.pagerank(G, alpha=py_alpha, weight=weight, tol=1.0e-7)
            return Expression('List', *[Real(centrality.get(v, 0)) for v in graph.vertices.expressions])


class HITSCentrality(_Centrality):
    '''
    >> g = Graph[{a -> d, b -> c, d -> c, d -> a, e -> c}]; HITSCentrality[g]
     = {{0.292893, 0., 0., 0.707107, 0.}, {0., 1., 0.707107, 0., 0.707107}}
    '''

    def apply(self, graph, expression, evaluation, options):
        '%(name)s[graph_, OptionsPattern[%(name)s]]'
        graph = self._build_graph(graph, evaluation, options, expression)
        if graph:
            G, _ = graph.coalesced_graph(evaluation)  # FIXME warn if weight > 1

            tol = 1.e-14
            _, a = nx.hits(G, normalized=True, tol=tol)
            h, _ = nx.hits(G, normalized=False, tol=tol)

            def _crop(x):
                return 0 if x < tol else x

            vertices = graph.vertices.expressions
            return Expression(
                'List',
                Expression('List', *[Real(_crop(a.get(v, 0))) for v in vertices]),
                Expression('List', *[Real(_crop(h.get(v, 0))) for v in vertices]))


class VertexDegree(_Centrality):
    '''
    >> VertexDegree[{1 <-> 2, 2 <-> 3, 2 <-> 4}]
     = {1, 3, 1, 1}
    '''

    def apply(self, graph, evaluation, options):
        '%(name)s[graph_, OptionsPattern[%(name)s]]'
        def degrees(graph):
            degrees = graph.G.degree(graph.vertices.expressions)
            return Expression('List', *[Integer(degrees.get(v, 0)) for v in graph.vertices.expressions])
        return self._evaluate_atom(graph, options, degrees)


class FindShortestPath(_NetworkXBuiltin):
    '''
    >> FindShortestPath[{1 <-> 2, 2 <-> 3, 3 <-> 4, 2 <-> 4, 4 -> 5}, 1, 5]
     = {1, 2, 4, 5}

    >> FindShortestPath[{1 <-> 2, 2 <-> 3, 3 <-> 4, 4 -> 2, 4 -> 5}, 1, 5]
     = {1, 2, 3, 4, 5}

    >> FindShortestPath[{1 <-> 2, 2 <-> 3, 4 -> 3, 4 -> 2, 4 -> 5}, 1, 5]
     = {}

    >> g = Graph[{1 -> 2, 2 -> 3, 1 -> 3}, EdgeWeight -> {0.5, a, 3}];
    >> a = 0.5; FindShortestPath[g, 1, 3]
     = {1, 2, 3}
    >> a = 10; FindShortestPath[g, 1, 3]
     = {1, 3}

    #> FindShortestPath[{}, 1, 2]
     : Vertex at position 2 in FindShortestPath[Graph[{}], 1, 2] must belong to the graph at position 1.
     = FindShortestPath[{}, 1, 2]

    #> FindShortestPath[{1 -> 2}, 1, 3]
     : Vertex at position 3 in FindShortestPath[{1 -> 2}, 1, 3] must belong to the graph at position 1.
     = FindShortestPath[{1 -> 2}, 1, 3]
    '''

    def apply_s_t(self, graph, s, t, expression, evaluation, options):
        '%(name)s[graph_, s_, t_, OptionsPattern[%(name)s]]'
        graph = self._build_graph(graph, evaluation, options, expression)
        if not graph:
            return
        G = graph.G
        if not G.has_node(s):
            evaluation.message(self.get_name(), 'inv', 2, expression)
        elif not G.has_node(t):
            evaluation.message(self.get_name(), 'inv', 3, expression)
        else:
            try:
                weight = graph.update_weights(evaluation)
                return Expression('List', *list(nx.shortest_path(G, source=s, target=t, weight=weight)))
            except nx.exception.NetworkXNoPath:
                return Expression('List')


class GraphDistance(_NetworkXBuiltin):
    '''
    >> GraphDistance[{1 <-> 2, 2 <-> 3, 3 <-> 4, 2 <-> 4, 4 -> 5}, 1, 5]
     = 3

    >> GraphDistance[{1 <-> 2, 2 <-> 3, 3 <-> 4, 4 -> 2, 4 -> 5}, 1, 5]
     = 4

    >> GraphDistance[{1 <-> 2, 2 <-> 3, 4 -> 3, 4 -> 2, 4 -> 5}, 1, 5]
     = Infinity

    >> GraphDistance[{1 <-> 2, 2 <-> 3, 3 <-> 4, 2 <-> 4, 4 -> 5}, 3]
     = {2, 1, 0, 1, 2}

    >> GraphDistance[{1 <-> 2, 3 <-> 4}, 3]
     = {Infinity, Infinity, 0, 1}

    #> GraphDistance[{}, 1, 1]
     : Vertex at position 2 in GraphDistance[{}, 1, 1] must belong to the graph at position 1.
     = GraphDistance[{}, 1, 1]
    #> GraphDistance[{1 -> 2}, 3, 4]
     : Vertex at position 2 in GraphDistance[{1 -> 2}, 3, 4] must belong to the graph at position 1.
     = GraphDistance[{1 -> 2}, 3, 4]
    '''

    def apply_s(self, graph, s, expression, evaluation, options):
        '%(name)s[graph_, s_, OptionsPattern[%(name)s]]'
        graph = self._build_graph(graph, evaluation, options, expression)
        if graph:
            weight = graph.update_weights(evaluation)
            d = nx.shortest_path_length(graph.G, source=s, weight=weight)
            inf = Expression('DirectedInfinity', 1)
            return Expression('List', *[d.get(v, inf) for v in graph.vertices.expressions])

    def apply_s_t(self, graph, s, t, expression, evaluation, options):
        '%(name)s[graph_, s_, t_, OptionsPattern[%(name)s]]'
        graph = self._build_graph(graph, evaluation, options, expression)
        if not graph:
            return
        G = graph.G
        if not G.has_node(s):
            evaluation.message(self.get_name(), 'inv', 2, expression)
        elif not G.has_node(t):
            evaluation.message(self.get_name(), 'inv', 3, expression)
        else:
            try:
                weight = graph.update_weights(evaluation)
                return from_python(nx.shortest_path_length(graph.G, source=s, target=t, weight=weight))
            except nx.exception.NetworkXNoPath:
                return Expression('DirectedInfinity', 1)


class CompleteGraph(_NetworkXBuiltin):
    '''
    >> CompleteGraph[8]
     = -Graph-

    #> CompleteGraph[0]
     : Expected a positive integer at position 1 in CompleteGraph[0].
     = CompleteGraph[0]
    '''

    messages = {
        'ilsmp': 'Expected a positive integer at position 1 in ``.',
    }

    def apply(self, n, expression, evaluation, options):
        '%(name)s[n_Integer, OptionsPattern[%(name)s]]'
        py_n = n.get_int_value()

        if py_n < 1:
            evaluation.message(self.get_name(), 'ilsmp', expression)
            return

        vertices = [Integer(i) for i in range(py_n)]
        edges = [Expression('UndirectedEdge', Integer(e1), Integer(e2))
                 for e1, e2, in permutations(range(py_n), 2)]

        G = nx.Graph()
        G.add_nodes_from(vertices)
        G.add_edges_from(e.leaves for e in edges)

        return Graph(
            _Collection(vertices),
            _EdgeCollection(edges, n_undirected=len(edges)),
            G, _circular_layout, options)

    def apply_multipartite(self, n, evaluation, options):
        '%(name)s[n_List, OptionsPattern[%(name)s]]'
        if all(isinstance(i, Integer) for i in n.leaves):
            return Graph(nx.complete_multipartite_graph(*[i.get_int_value() for i in n.leaves]))


def _convert_networkx_graph(G, options):
    mapping = dict((v, Integer(i)) for i, v in enumerate(G.nodes_iter()))
    G = nx.relabel_nodes(G, mapping)
    edges = [Expression('System`UndirectedEdge', u, v) for u, v in G.edges_iter()]
    return Graph(
        _Collection(G.nodes()),
        _EdgeCollection(edges, n_undirected=len(edges)),
        G, None, options)


class RandomGraph(_NetworkXBuiltin):
    def apply_nm(self, n, m, expression, evaluation, options):
        '%(name)s[{n_Integer, m_Integer}, OptionsPattern[%(name)s]]'
        py_n = n.get_int_value()
        py_m = m.get_int_value()
        with RandomEnv(evaluation) as rand:
            seed = rand.randint(0, 2 ** 63 - 1)
            G = nx.gnm_random_graph(py_n, py_m, seed=seed)
            return _convert_networkx_graph(G, options)


class VertexAdd(_NetworkXBuiltin):
    '''
    >> g1 = Graph[{1 -> 2, 2 -> 3}];
    >> g2 = VertexAdd[g1, 4]
     = -Graph-
    >> g3 = VertexAdd[g2, {5, 10}]
     = -Graph-
    >> VertexAdd[{a -> b}, c];
     = -Graph-
    '''

    def apply(self, graph, what, expression, evaluation, options):
        '%(name)s[graph_, what_, OptionsPattern[%(name)s]]'
        graph = self._build_graph(graph, evaluation, options, expression)
        if graph:
            if what.get_head_name() == 'System`List':
                return graph.add_vertices(*zip(*[_parse_item(x) for x in what.leaves]))
            else:
                return graph.add_vertices(*zip(*[_parse_item(what)]))


class VertexDelete(_NetworkXBuiltin):
    '''
    >> g1 = Graph[{1 -> 2, 2 -> 3, 3 -> 4}];
    >> VertexDelete[g1, 3]
     = -Graph-
    >> VertexDelete[{a -> b, b -> c, c -> d, d -> a}, {a, c}]
     = -Graph-
    >> VertexDelete[{1 -> 2, 2 -> 3, 3 -> 4, 4 -> 6, 6 -> 8, 8 -> 2}, _?OddQ]
    '''

    def apply(self, graph, what, expression, evaluation, options):
        '%(name)s[graph_, what_, OptionsPattern[%(name)s]]'
        graph = self._build_graph(graph, evaluation, options, expression)
        if graph:
            from mathics.builtin import pattern_objects

            head_name = what.get_head_name()
            if head_name in pattern_objects:
                cases = Expression('Cases', Expression(
                    'List', *graph.vertices.expressions), what).evaluate(evaluation)
                if cases.get_head_name() == 'System`List':
                    return graph.delete_vertices(cases.leaves)
            elif head_name == 'System`List':
                return graph.delete_vertices(what.leaves)
            else:
                return graph.add_edges([what])


class EdgeAdd(_NetworkXBuiltin):
    '''
    >> EdgeAdd[{1->2,2->3},3->1]
     = -Graph-
    '''

    def apply(self, graph, what, expression, evaluation, options):
        '%(name)s[graph_, what_, OptionsPattern[%(name)s]]'
        graph = self._build_graph(graph, evaluation, options, expression)
        if graph:
            if what.get_head_name() == 'System`List':
                return graph.add_edges(*zip(*[_parse_item(x) for x in what.leaves]))
            else:
                return graph.add_edges(*zip(*[_parse_item(what)]))


class EdgeDelete(_NetworkXBuiltin):
    '''
    >> Length[EdgeList[EdgeDelete[{a -> b, b -> c, c -> d}, b -> c]]]
     = 2

    >> Length[EdgeList[EdgeDelete[{a -> b, b -> c, c -> b, c -> d}, b <-> c]]]
     = 4

    >> Length[EdgeList[EdgeDelete[{a -> b, b <-> c, c -> d}, b -> c]]]
     = 3

    >> Length[EdgeList[EdgeDelete[{a -> b, b <-> c, c -> d}, c -> b]]]
     = 3

    >> Length[EdgeList[EdgeDelete[{a -> b, b <-> c, c -> d}, b <-> c]]]
     = 2

    >> EdgeDelete[{4<->5,5<->7,7<->9,9<->5,2->4,4->6,6->2}, _UndirectedEdge]
     = -Graph-
    '''

    def apply(self, graph, what, expression, evaluation, options):
        '%(name)s[graph_, what_, OptionsPattern[%(name)s]]'
        graph = self._build_graph(graph, evaluation, options, expression)
        if graph:
            from mathics.builtin import pattern_objects

            head_name = what.get_head_name()
            if head_name in pattern_objects:
                cases = Expression('Cases', Expression(
                    'List', *graph.edges.expressions), what).evaluate(evaluation)
                if cases.get_head_name() == 'System`List':
                    return graph.delete_edges(cases.leaves)
            elif head_name == 'System`List':
                return graph.delete_edges(what.leaves)
            else:
                return graph.delete_edges([what])
