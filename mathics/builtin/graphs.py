#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Graphs
"""

# uses GraphViz, if it's installed in the PATH (see pydotplus.graphviz.find_graphviz and http://www.graphviz.org).
# export PATH="$PATH:/Users/bernhard/dev/homebrew/bin"

from __future__ import unicode_literals
from __future__ import absolute_import
from __future__ import division

from mathics.builtin.base import Builtin, AtomBuiltin
from mathics.core.expression import Expression, Symbol, Atom, String, Integer

import itertools

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


def _generic_layout(G):
    pos = None

    try:
        import pydotplus

        if pydotplus.graphviz.find_graphviz():
            pos = nx.nx_pydot.graphviz_layout(G)
    except ImportError:
        pass

    return nx.drawing.fruchterman_reingold_layout(G, pos=pos, k=1.0)


class _NetworkXBuiltin(Builtin):
    requires = (
        'networkx',
    )

    messages = {
        'graph': 'Expected a graph at position 1 in ``.',
    }

    def _build_graph(self, graph, evaluation, expr):
        head = graph.get_head_name()
        if head == 'System`Graph':
            return graph
        elif head == 'System`List':
            return _graph_from_list(graph.leaves)
        else:
            evaluation.message(self.get_name(), 'graph', expr)

    def _evaluate_atom(self, graph, compute):
        head = graph.get_head_name()
        if head == 'System`Graph':
            return compute(graph)
        elif head == 'System`List':
            return compute(_graph_from_list(graph.leaves))

    def _evaluate(self, graph, compute):
        head = graph.get_head_name()
        if head == 'System`Graph':
            return compute(graph.G)
        elif head == 'System`List':
            return compute(_graph_from_list(graph.leaves).G)
        else:
            evaluation.message(self.get_name(), 'graph', expr)

    def _evaluate_to_list(self, graph, compute):
        r = self._evaluate(graph, compute)
        if r:
            return Expression('List', *r)


class Graph(Atom):
    def __init__(self, vertices, edges, G, layout, highlights=None, **kwargs):
        super(Graph, self).__init__(**kwargs)
        self.vertices = vertices
        self.edges = edges
        self.G = G
        self.layout = layout
        self.highlights = highlights

    def __str__(self):
        return '-Graph-'

    def with_highlight(self, highlights):
        return Graph(self.vertices, self.edges,
                     self.G, self.layout, highlights)

    def do_copy(self):
        return Graph(self.vertices, self.edges,
                     self.G, self.layout, self.highlights)

    def default_format(self, evaluation, form):
        return '-Graph-'

    def get_sort_key(self, pattern_sort=False):
        if pattern_sort:
            return super(Graph, self).get_sort_key(True)
        else:
            return hash(self)

    def same(self, other):
        return isinstance(other, Graph) and self.G == other.graph

    def to_python(self, *args, **kwargs):
        return self.G

    def __hash__(self):
        return hash(("Graph", self.G))

    def atom_to_boxes(self, form, evaluation):
        G = self.G
        highlights = self.highlights

        pos = self.layout(G)

        nodelist = G.nodes()
        edgelist = G.edges()

        r = 0.1
        directed = isinstance(G, nx.DiGraph)
        edge = 'DirectedEdge' if directed else 'UndirectedEdge'

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


        def primitives():
            yield Expression('AbsoluteThickness', 0.1)

            if directed:
                yield Expression('Arrowheads', 0.04)
            else:
                yield Expression('Arrowheads', 0)

            # FIXME handle multigraphs with multiple same edges
            # FIXME needs curves in Graphics

            for e, style in highlighted(self.edges):
                e1, e2 = e.leaves

                p1 = pos[e1]
                p2 = pos[e2]

                q1 = Expression('List', *p1)
                q2 = Expression('List', *p2)
                arrow = Expression('Arrow', Expression('List', q1, q2), Expression('List', r, r))

                if style is not None:
                    arrow = Expression('Style', arrow, style)

                yield arrow

            for v, style in highlighted(self.vertices):
                x, y = pos[v]

                if style is not None:
                    disk = Expression('Disk', Expression('List', x, y), Expression('List', r, r))
                    yield Expression('Style', disk, style)

                yield Expression('Circle', Expression('List', x, y), Expression('List', r, r))

                # yield Expression('FontSize', Expression('Scaled', r))
                yield Expression('Text', v, Expression('List', x, y))

        graphics = Expression('Graphics', Expression('List', *list(primitives())))
        return Expression('MakeBoxes', graphics, form).evaluate(evaluation)


def _graph_from_list(rules):
    known_vertices = set()
    vertices = []
    edges = []

    def add_vertex(x):
        if x not in known_vertices:
            known_vertices.add(x)
            vertices.append(x)

    directed_edges = []
    undirected_edges = []

    for r in rules:
        name = r.get_head_name()

        if name == 'System`Rule' or name == 'System`DirectedEdge':
            edges_container = directed_edges
            head = 'System`DirectedEdge'
        elif name == 'System`UndirectedEdge':
            edges_container = undirected_edges
            head = 'System`UndirectedEdge'
        else:
            return

        if len(r.leaves) != 2:
            return

        u, v = r.leaves

        add_vertex(u)
        add_vertex(v)

        if head == name:
            edges.append(r)
        else:
            edges.append(Expression(head, u, v))

        edges_container.append((u, v))

    if directed_edges:
        G = nx.MultiDiGraph()
        for u, v in directed_edges:
            G.add_edge(u, v)
        for u, v in undirected_edges:
            G.add_edge(u, v)
            G.add_edge(v, u)
    else:
        G = nx.MultiGraph()
        for u, v in undirected_edges:
            G.add_edge(u, v)

    if _is_path(G):
        layout = _spectral_layout
    else:
        layout = _generic_layout

    return Graph(vertices, edges, G, layout)


class DirectedEdge(Builtin):
    pass


class UndirectedEdge(Builtin):
    pass


def _is_path(G):
    return all(d <= 2 for d in G.degree(range(G.number_of_nodes())))


class GraphAtom(AtomBuiltin):
    """
    Graph[{1->2, 2->3, 3->1}]
    """

    requires = (
        'networkx',
    )

    def apply(self, graph, evaluation):
        'Graph[graph_List]'
        return _graph_from_list(graph.leaves)


class HighlightGraph(_NetworkXBuiltin):
    '''

    '''

    def apply(self, graph, what, evaluation):
        'HighlightGraph[graph_, what_List]'
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

        graph = self._build_graph(graph, evaluation, lambda: Expression('HighlightGraph', graph, what))
        if graph:
            rule_exprs = Expression('List', *[Expression('Rule', *r) for r in rules])
            return graph.with_highlight(rule_exprs)


class PathGraph(_NetworkXBuiltin):
    def apply(self, vertices, evaluation):
        'PathGraph[vertices_List]'
        pass


class _PatternList():
    def apply(self, graph, evaluation):
        '%(name)s[graph_]'
        graph = self._build_graph(graph, evaluation, lambda: Expression(self.get_name(), graph))
        if graph:
            return Expression('List', *self._items(graph))

    def apply_patt(self, graph, patt, evaluation):
        '%(name)s[graph_, patt_]'
        graph = self._build_graph(graph, evaluation, lambda: Expression(self.get_name(), graph, patt))
        if graph:
            return Expression('Cases', Expression('List', *self._items(graph)), patt)


class _PatternCount():
    def apply(self, graph, evaluation):
        '%(name)s[graph_]'
        graph = self._build_graph(graph, evaluation, lambda: Expression(self.get_name(), graph))
        if graph:
            return Integer(len(self._items(graph)))

    def apply_patt(self, graph, patt, evaluation):
        '%(name)s[graph_, patt_]'
        graph = self._build_graph(graph, evaluation, lambda: Expression(self.get_name(), graph, patt))
        if graph:
            return Expression('Length', Expression('Cases', Expression('List', *self._items(graph)), patt))


class VertexCount(_PatternCount):
    def _items(self, graph):
        return graph.vertices


class VertexList(_PatternList):
    def _items(self, graph):
        return graph.vertices


class EdgeCount(_PatternCount):
    def _items(self, graph):
        return graph.edges


class EdgeList(_PatternList):
    def _items(self, graph):
        return graph.edges


class _Centrality(_NetworkXBuiltin):
    pass


class BetweennessCentrality(_Centrality):
    def apply(self, graph, evaluation):
        '%(name)s[graph_]'
        graph = self._build_graph(graph, evaluation, lambda: Expression('BetweennessCentrality', graph))
        if graph:
            return Expression('List', nx.betweenness_centrality(graph.G))


class ClosenessCentrality(_Centrality):
    def apply(self, graph, evaluation):
        '%(name)s[graph_]'
        return self._evaluate_to_list(graph, nx.closeness_centrality)


class DegreeCentrality(_Centrality):
    def apply(self, graph, evaluation):
        '%(name)s[graph_]'
        return self._evaluate_to_list(graph, nx.degree_centrality)

    def apply_in(self, graph, evaluation):
        '%(name)s[graph_, "In"]'
        return self._evaluate_to_list(graph, nx.in_degree_centrality)

    def apply_out(self, graph, evaluation):
        '%(name)s[graph_, "Out"]'
        return self._evaluate_to_list(graph, nx.out_degree_centrality)


class EigenvectorCentrality(_Centrality):
    def apply(self, graph, evaluation):
        '%(name)s[graph_]'
        return self._evaluate_to_list(graph, nx.eigenvector_centrality)


class KatzCentrality(_Centrality):
    def apply_alpha(self, graph, alpha, evaluation):
        '%(name)s[graph_, alpha_]'
        py_alpha=alpha.to_number()
        return self._evaluate_to_list(graph, lambda G: nx.katz_centrality(G, alpha=py_alpha))

    def apply_alpha_beta(self, graph, alpha, beta, evaluation):
        '%(name)s[graph_, alpha_, beta_]'
        py_alpha = alpha.to_number()
        py_beta = beta.to_number()
        return self._evaluate_to_list(graph, lambda G: nx.katz_centrality(G, alpha=py_alpha, beta=py_beta))


class PageRankCentrality(_Centrality):
    def apply_alpha(self, graph, alpha, evaluation):
        '%(name)s[graph_, alpha_]'
        py_alpha=alpha.to_number()
        return self._evaluate_to_list(graph, lambda G: nx.pagerank(G, alpha=py_alpha))

    def apply_alpha_beta(self, graph, alpha, beta, evaluation):
        '%(name)s[graph_, alpha_, beta_]'
        py_alpha = alpha.to_number()
        py_beta = beta.to_number()
        return self._evaluate_to_list(graph, lambda G: nx.pagerank(G, alpha=py_alpha, beta=py_beta))


class HITSCentrality(_Centrality):
    def apply(self, graph, evaluation):
        '%(name)s[graph_]'
        return self._evaluate_to_list(graph, nx.hits)


def _dict_to_list(G, results, default):
    for x in G.nodes_iter():
        if x in results:
            yield results[x]
        else:
            yield default


class VertexDegree(_Centrality):
    def apply(self, graph, evaluation):
        '%(name)s[graph_]'
        def degrees(G):
            n = G.number_of_nodes()
            return list(_dict_to_list(G, G.degree(range(n)), 0))
        return self._evaluate_to_list(graph, degrees)


class FindShortestPath(_NetworkXBuiltin):
    def apply_s_t(self, graph, s, t, evaluation):
        '%(name)s[graph_, s_, t_]'
        try:
            return self._evaluate_to_list(graph, lambda G: nx.shortest_path(G, source=s, target=t))
        except nx.exception.NetworkXNoPath:
            return Expression('List')


class GraphDistance(_NetworkXBuiltin):
    def apply_s(self, graph, s, evaluation):
        '%(name)s[graph_, s_]'
        def distances(G):
            p = nx.single_source_shortest_path_length(G, source=s)
            return list(_dict_to_list(G, p, Expression('DirectedInfinity', 1)))
        return self._evaluate_to_list(graph, distances)

    def apply_s_t(self, graph, s, t, evaluation):
        '%(name)s[graph_, s_, t_]'
        try:
            return self._evaluate_to_list(graph, lambda G: nx.shortest_path_length(G, source=s, target=t))
        except nx.exception.NetworkXNoPath:
            return Expression('DirectedInfinity', 1)


class CompleteGraph(_NetworkXBuiltin):
    def apply(self, n, evaluation):
        '%(name)s[n_Integer]'
        py_n = n.get_int_value()

        vertices = [Integer(i) for i in range(py_n)]
        edges = [Expression(
            'UndirectedEdge', Integer(e1), Integer(e2)) for e1, e2, in itertools.permutations(range(py_n), 2)]

        G = nx.Graph()
        G.add_nodes_from(vertices)
        G.add_edges_from(e.leaves for e in edges)

        return Graph(vertices, edges, G, _circular_layout)

    def apply_multipartite(self, n, evaluation):
        '%(name)s[n_List]'
        if all(isinstance(i, Integer) for i in n.leaves):
            return Graph(nx.complete_multipartite_graph(*[i.get_int_value() for i in n.leaves]))


class PetersenGraph(_NetworkXBuiltin):
    def apply(self, n, k, evaluation):
        '%(name)s[n_Integer, k_Integer]'
        pass
