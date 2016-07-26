#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Graphs
"""

from __future__ import unicode_literals
from __future__ import absolute_import
from __future__ import division

from mathics.builtin.base import Builtin
from mathics.core.expression import Expression, Symbol
from math import sqrt

class _NetworkXBuiltin(Builtin):
    requires = (
        'networkx',
    )


class Graph(_NetworkXBuiltin):
    """
    Graph[{1->2,2->3,3->1}]
    """

    def apply(self, graph, evaluation):
        'Graph[graph_List]'

        import networkx as nx
        G = nx.Graph()

        for r in graph.leaves:
            if r.get_head_name() != 'System`Rule' or len(r.leaves) != 2:
                return
            a, b = r.leaves
            G.add_edge(a, b)

        pos = nx.drawing.spring_layout(G)
        nodelist = G.nodes()
        edgelist = G.edges()

        def primitives():
            r = 0.1

            yield Expression('AbsoluteThickness', 0.1)

            for v in nodelist:
                x, y = pos[v]
                yield Expression('Circle', Expression('List', x, y), Expression('List', r, r))
                yield Expression('FontSize', Expression('Scaled', r))

                # form = Expression('MakeBoxes', v, Symbol('OutputForm')).evaluate(evaluation)
                yield Expression('Text', v, Expression('List', x, y))

            for e1, e2 in ((pos[e[0]], pos[e[1]]) for e in edgelist):
                x1, y1 = e1
                x2, y2 = e2
                dx = x2 - x1
                dy = y2 - y1
                l = sqrt(dx * dx + dy * dy)
                ndx = dx / l
                ndy = dy / l
                rdx = ndx * r
                rdy = ndy * r

                # FIXME use Arrow[]

                yield Expression('Line', Expression('List',
                                                    Expression('List', x1 + rdx, y1 + rdy),
                                                    Expression('List', x2 - rdx, y2 - rdy)))

        return Expression('Graphics', Expression('List', *list(primitives())))
