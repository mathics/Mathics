#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import division

import random
from heapq import nsmallest
from itertools import chain
import bisect
import math

# publications used for this file:

# [Kurita1991] Takito Kurita: "An Efficient Agglomerative Clustering Algorithm Using A Heap", Pattern Recognition,
# Volume 24 Issue 3, 1991.
# [Ng1994] Raymond T. Ng, Jiawei Han: Efficient and Effective Clustering Methods for Spatial Data Mining.
# VLDB 1994: 144-155
# [Rangel2016] E. Rangel, W. Hendrix, A. Agrawal, W.-keng Liao, and A. Choudhary, “AGORAS: A Fast Algorithm for
# Estimating Medoids in Large Datasets,” in Proceedings of International Conference on Computational Science (ICCS),
# 2016.
# [Hamerly2003] Greg Hamerly, Charles Elkan, Learning the k in k-means. In proceedings of the seventeenth
# annual conference on neural information processing systems (NIPS), pages 281-288, December 2003.
# [Desgraupes2013] Bernard Desgraupes, Package clusterCrit for R: Clustering Indices, available online at
# https://cran.r-project.org/web/packages/clusterCrit/vignettes/clusterCrit.pdf
# [Greutert2003] Andreas Greutert. Methoden zur Schätzung der Clusteranzahl. Ausgabe: 29. Oktober 2003.
# Diplomarbeit. ETH Zürich.

# for agglomerative clustering, we use [Kurita1991].

# for hierarchical clustering, we use ClaraNS (see [Ng1994]), but we start local searches not with a random sample,
# but with a seed computed via AGORAS (see [Rangel2016]). for clustering without knowing the number of clusters k,
# we use the approach described in [Hamerly2003]. clustering quality is measures using McClain-Rao's measure (see
# [Desgraupes2013]).

# we modify ClaraNS by keeping track of the randomly picked swaps and not trying the same swap twice if no
# improvement was made in the meantime.


def _index(i, j):  # i > j, returns j + sum(1, 2, ..., i - 1)
    # x x x
    # a x x
    # b c x

    # a = 0, b = 1, c = 2
    return j + ((i - 1) * i) // 2


def _components(clusters, n):
    components = [0] * n
    for i, c in enumerate(clusters):
        component_index = i + 1
        for j in c:
            components[j] = component_index
    return components


class PrecomputedDistances(object):
    def __init__(self, distances):
        self._distances = distances

    def distance(self, i, j):
        return self._distances[_index(max(i, j), min(i, j))]

    def matrix(self):
        return self._distances


class LazyDistances(object):
    def __init__(self):
        self._computed = {}

    def distance(self, i, j):
        computed = self._computed
        index = _index(max(i, j), min(i, j))
        d = computed.get(index, None)
        if d is None:
            # we are probably in the middle of a (seeded) random
            # computation, and we do not want the distance function
            # to mess with that state.
            random_state = random.getstate()
            try:
                d = self._compute_distance(i, j)
            finally:
                random.setstate(random_state)
            computed[index] = d
        return d

    def matrix(self):
        raise ValueError('LazyDistances does not support matrix()')

    def _compute_distance(self, i, j):
        raise ValueError('_compute_distance was not implemented')


def _shuffled_range(n):
    # returns all numbers from [0, ..., n - 1] in random order, never returning any number twice.

    # basically a Fisher-Yates shuffle that does not materialize the data, which means it's especially suited for
    # k << n, where k is the count of random numbers actually retrieved.

    # performs surprisingly well even if all random numbers are retrieved. getting all available numbers from
    # shuffled_range(10^7) only takes only 70% longer than generating just the raw 10 million random numbers.

    a = dict()

    def nth(k):
        return a.get(k, k)

    for i in range(n - 1):
        ai = nth(i)

        j = random.randint(i, n - 1)
        aj = nth(j)

        a[j] = ai
        yield aj

    yield nth(n - 1)


def _shuffled_tuples(*t):
    # like _shuffled_range, but not returning integers, but tuples
    # of integers within given ranges.

    # we map the tuple values to a linear index, then use _shuffled_range.

    # e.g. for t = (2, 3), we map:
    # 0 -> (0, 0); 1 -> (1, 0); 2 -> (0, 1)
    # 3 -> (1, 1); 4 -> (0, 2); 5 -> (1, 2)

    n = 1
    for x in t:
        n *= x
    indices = iter(_shuffled_range(n))

    while True:
        n = next(indices)
        r = []
        for x in t:
            r.append(n % x)
            n = n // x
        yield r


def _unmapped(u, v, distance):
    # maps each object in u to the most similar object in v
    # and returns a version of v from which all unmapped objects
    # have been removed.

    mapped = [False] * len(v)
    for uu in u:
        min_d = None
        min_i = None
        for i, vv in enumerate(v):
            if uu == vv:
                d = 0
            else:
                d = distance(uu, vv)
            if min_d is None or d < min_d:
                min_d = d
                min_i = i
        mapped[min_i] = True
    return [vv for m, vv in zip(mapped, v) if m]


def _agoras(n, k, distance):  # see [Rangel2016]
    gamma = 0.577215664901532  # Euler-Mascheroni constant
    m = int(math.ceil(k * (math.log(k) + gamma)))
    r = k * math.log(k) + gamma * k
    p = tuple(range(n))

    if r > n:
        # if k (and thus r) is very large in regard to n, we fall
        # back to simple sampling, since AGORAS is slow then.
        return random.sample(p, k)

    while True:
        s = [random.sample(p, min(int(r), n)) for _ in range(m)]
        for i in range(m - 1):
            s[i + 1] = _unmapped(s[i], s[i + 1], distance)
            if len(s[i + 1]) < k:
                r += r * (m - (i + 1)) / m
                break
        if len(s[-1]) > k:
            r *= 0.95
        elif len(s[-1]) == k:
            return s[-1]


class SplitCriterion(object):
    def should_split(self, siblings, merged, unmerged, distance):
        raise NotImplementedError()

    def _approximate_mcclain_rao(self, clusters, distance):
        # gives a measure of how good the current clustering is, and if it should be split further or left as is.
        # what we actually calculate here is a bit like a simplified, PAM suitable version of the McClain-Rao index;
        # it would not make sense to use a criterion needing O(n^2) computations, if the actual clustering is O(n).
        # see [Desgraupes2013] for similar measures that inspired this one and an overview of other measures. For
        # an even more general overview, see [Greutert2003].

        w = self._approximate_within_distance(clusters, distance)
        if w is None:
            return None

        b = self._approximate_between_distance(clusters, distance)
        return w / b

    def _approximate_within_distance(self, clusters, distance):
        s_w = 0
        n_w = 0

        # s_w is the sum of within-cluster (point to its medoid) distances.
        for c in clusters:
            s, n = c.within(distance)
            s_w += s
            n_w += n

        if n_w <= len(clusters):  # only single-medoid clusters remain.
            return None

        return s_w / n_w

    def _approximate_between_distance(self, clusters, distance):
        # s_b is the sum of between-cluster (medoid to medoid) distances.
        s_b = 0
        n_b = len(clusters)

        for z, c in enumerate(clusters):
            for d in clusters[:z]:
                s_b += distance(c.medoid, d.medoid)
                n_b += 1

        return s_b / n_b


class ConvergingCriterion(SplitCriterion):
    def __init__(self, granularity, last_criterion=None, last_ratio=None):
        self._granularity = granularity
        self._last_criterion = last_criterion
        self._last_ratio = last_ratio

    def should_split(self, siblings, merged, unmerged, distance):
        # the following might seem convoluted, but it turns out to be a good
        # measure. it checks if the solution converges by checking that the
        # rate of improvement gets larger and larger as we split smaller and
        # smaller areas. "ratio" measures how much "within-to-between-ness"
        # improvement we got with a particular split, and the smaller this
        # value gets, the larger the improvement for "within-to-between-ness"
        # is. if the improvement is larger or equal from the improvement we
        # got from the previous split, it's a sign for converging, and we
        # accept it. if the improvement is smaller (i.e. "ratio" is larger")
        # than before, we assume the split is bad. when the improvement falls
        # below a certain absolute value "limit", we also stop.

        criterion = self._approximate_mcclain_rao(siblings + unmerged, distance)
        last_criterion = self._last_criterion
        last_ratio = self._last_ratio

        limit = 0.25 * self._granularity

        if criterion is None:
            ratio = limit + 1.  # always stop
        elif last_criterion is None:
            ratio = 0.  # first call, depth=0
        elif last_criterion < 1e-15:
            ratio = limit + 1.  # always stop
        else:
            ratio = criterion / last_criterion
        if not last_ratio:
            last_ratio = limit

        return ratio <= min(limit, last_ratio), ConvergingCriterion(self._granularity, criterion, ratio)


class _Cluster:
    def __init__(self, medoid, members):
        self.medoid = medoid
        self.members = members
        self._within = None

    def translate(self, i_to_i0):
        return _Cluster(i_to_i0[self.medoid], [i_to_i0[i] for i in self.members])

    def within(self, distance):
        if self._within is None:
            s_w = 0
            n_w = 0

            # s_w is the sum of within-cluster (point to its medoid) distances.
            m = self.medoid
            for i in self.members:
                if i != m:
                    s_w += distance(i, m)
                n_w += 1

                self._within = (s_w, n_w)

        return self._within

class _Medoids:
    def __init__(self, clusterer, k):
        distance = clusterer._distance_lambda()
        n = clusterer._n

        self._n = n
        self._k = k
        self._distance = distance
        self._reset_random_swap()

        self._selected = _agoras(n, k, distance)
        self._selected.sort()

        self._clusters = [None] * n
        self._cost = sum(self._update_clusters(self._unselected))
        self._debug = clusterer.debug

    def clusters(self):
        clusters = self._clusters
        solution = []
        for i in self._selected:
            members = [j for j in self._unselected if clusters[j][0] == i]
            # preserve the original order of the elements by inserting i.
            bisect.insort(members, i)
            solution.append(_Cluster(i, members))
        return solution

    @property
    def _unselected(self):
        selected = self._selected
        k = 0

        z = selected[k]
        k += 1
        for i in range(self._n):
            if i == z:
                if k < len(selected):
                    z = selected[k]
                    k += 1
                else:
                    z = -1
            else:
                yield i

    def _reset_random_swap(self):
        self._random_swap = iter(_shuffled_tuples(self._k, self._n - self._k))

    def _next_random_swap(self):
        n_i, h = next(self._random_swap)

        for j in self._selected:
            if h >= j:
                h += 1

        return self._selected[n_i], h

    def _update_clusters(self, a):
        selected = self._selected
        clusters = self._clusters
        distance = self._distance

        for j in a:
            s1, s2 = nsmallest(2, [(distance(i, j), i) for i in selected])
            d1, i1 = s1
            d2, i2 = s2
            assert i1 != i2
            clusters[j] = (i1, i2)
            yield d1

    def cost(self):
        return self._cost

    def swap(self):
        try:
            i, h = self._next_random_swap()
        except StopIteration:
            self._debug('all swaps tried')
            return False

        # try to swap medoid i with non-medoid h
        clusters = self._clusters
        distance = self._distance

        # initialize t
        t = min(distance(i, j) for j in chain(self._selected, [h]) if j != i)  # i attaches to a medoid
        t -= distance(h, clusters[h][0])  # h detaches from its medoid
        self._debug('eval swap t:%f' % t, i, h)

        # we differentiate fast_updates and slow_updates. fast_updates
        # need to update only the second-nearest medoid. slow_updates
        # need to update the nearest and second-nearest medoid.
        fast_updates = []

        for j in self._unselected:
            if j == h:
                continue
            # see [Ng1994] for a description of the following calculations
            n1, n2 = clusters[j]  # nearest two medoids
            dh = distance(j, h)  # d(Oj, Oh)
            if n1 == i:  # is j in cluster i?
                d2 = distance(j, n2)  # d(Oj, Oj,2)
                if dh >= d2:  # case (1); j does not go to h
                    t += d2 - distance(j, i)
                    fast_updates.append((j, n2))
                else:  # case (2); j goes to h
                    t += dh - distance(j, i)
                    fast_updates.append((j, h))
            else:
                k = clusters[j][0]
                d2 = distance(j, k)
                if dh >= d2:  # case (3)
                    # j stays in current cluster. second nearest medoid
                    # to j might change with the introduction of h though.
                    fast_updates.append((j, k))
                else:  # case (4)
                    t += dh - d2
                    fast_updates.append((j, h))

        if t < 0:  # swap is an improvement?
            self._debug('ACCEPT swap t:%f' % t, i, h)
            self._cost += t

            selected = self._selected
            del selected[selected.index(i)]
            bisect.insort(selected, h)

            slow_updates = [i]  # update i's distances to medoids
            for j, m in fast_updates:
                if m == i:
                    slow_updates.append(j)
                else:
                    min_d = None
                    min_k = None
                    for k in selected:
                        if k != m:
                            d = distance(k, j)
                            if min_d is None or d < min_d:
                                min_d = d
                                min_k = k
                    assert m != min_k and min_k is not None
                    clusters[j] = (m, min_k)

            # update other second distances.
            for _ in self._update_clusters(slow_updates):
                pass  # ignore distances (no influence on cost here).

            # we swapped, so we want to allow previously used partners.
            self._reset_random_swap()

            return True
        else:
            return False


class _Clusterer:
    debug_output = False

    def __init__(self, n, i_to_i0, medoid0, siblings, p0, d0):
        self._n = n
        self._i_to_i0 = i_to_i0
        self._medoid0 = medoid0
        self._siblings = siblings
        self._p0 = p0
        self._d0 = d0

    def _distance_lambda(self):
        d0 = self._d0
        i_to_i0 = self._i_to_i0

        def distance(i, j):
            return d0(i_to_i0[i], i_to_i0[j])

        return distance

    def debug(self, s, *i):
        if self.debug_output:
            print(s, [self._p0[self._i_to_i0[j]] for j in i])

    def with_k(self, k):
        # this implements CLARANS as described in [Ng1994]. we modify it
        # by doing an AGORAS seed in _Medoids().

        n = self._n
        # [Ng1994] recommends the following values
        num_local = 2  # number of local minima to search for
        max_neighbours = min(max(0.0125 * k * (n - k), 250), k * (n - k))

        min_cost_medoids = None
        for i in range(num_local):
            medoids = _Medoids(self, k)

            self.debug('new local %f' % medoids.cost())
            j = 1
            while j <= max_neighbours:
                if medoids.swap():
                    self.debug('NEW MEDOIDS (%f)' % medoids.cost(), *medoids._selected)
                    j = 1
                else:
                    j += 1

            self.debug('end local %f' % medoids.cost())
            if min_cost_medoids is None or medoids.cost() < min_cost_medoids.cost():
                min_cost_medoids = medoids

        return min_cost_medoids.clusters()

    def without_k(self, criterion):
        # we estimate k using the approach described in [Hamerly2003].

        # this chunks the points (0, 1, ..., n - 1) into clusters. each point i
        # corresponds to a global point index i_to_i0[i]. d0 is a distance function
        # that takes two global point indices.

        n = self._n
        i_to_i0 = self._i_to_i0

        if n < 2:
            return [[i_to_i0[0]]]

        clusters = self.with_k(2)
        clusters0 = [c.translate(i_to_i0) for c in clusters]

        if self.debug_output:
            print([[self._p0[i] for i in c.members]
                   for c in self._siblings + clusters0])

        merged = _Cluster(self._medoid0, list(chain(*[c.members for c in clusters0])))

        split, new_criterion = criterion.should_split(
            self._siblings, merged, clusters0, self._d0)

        if not split:
            return [[i_to_i0[i] for i in range(n)]]
        else:
            r = []
            for i, c in enumerate(clusters):
                t = clusters0[i].members
                d = clusters0[1 - i]

                sub = _Clusterer(len(t), t, clusters0[i].medoid, self._siblings + [d], self._p0, self._d0)
                r.extend(sub.without_k(new_criterion))
            return r


def optimize(p, k, distances, mode='clusters', seed=12345, granularity=1.):
    if k == 1:
        return [p]

    random_state = random.getstate()
    try:
        # we want the same output on every call on the same data, so we use
        # a fixed random seed at this point.
        random.seed(seed)

        clusterer = _Clusterer(
            len(p), tuple(range(len(p))), None, [], p, distances.distance)

        if k is None:
            criterion = ConvergingCriterion(granularity)
            clusters = clusterer.without_k(criterion)
        else:
            clusters = [c.members for c in clusterer.with_k(k)]

        # sort clusters by order of their first element in the original list.
        clusters = sorted(clusters, key=lambda c: c[0])

        if mode == 'clusters':
            return list(map(lambda c: map(lambda i: p[i], c), clusters))
        elif mode == 'components':
            return _components(clusters, len(p))
        else:
            raise ValueError('illegal mode %s' % mode)
    finally:
        random.setstate(random_state)


class MergeCriterion(object):
    def __init__(self, distances, n):
        self.distances = distances
        self.groups = list(map(lambda i: [i], range(n)))
        self.b = sum(self.distances)
        self.w = 0
        self.k = self.n = n

        self.n_w = 0  # (i, i) distances
        self.n_b = len(distances)

    def try_merge(self, i, j, d):
        raise NotImplementedError()

    def _update(self, i, j):
        groups = self.groups
        distances = self.distances
        d = 0
        for x in groups[i]:
            for y in groups[j]:
                d += distances[_index(max(x, y), min(x, y))]

        n = len(groups[i]) * len(groups[j])
        self.n_w += n
        self.n_b -= n

        self.b -= d
        self.w += d

    def _merge(self, i, j):
        self._update(i, j)

        self.groups[i].extend(self.groups[j])
        self.groups[j] = None
        self.k -= 1

    def _average_within_distance(self):
        return self.w / self.n_w

    def _average_between_distance(self):
        return self.b / self.n_b


class CloserThanAverageCriterion(MergeCriterion):
    def __init__(self, distances, n, granularity):
        super(CloserThanAverageCriterion, self).__init__(distances, n)
        self._limit = 0.25 * granularity

    def try_merge(self, i, j, d):
        if self.k > 2:
            # the distance between the merged clusters gets compared to the
            # average of all existing cluster distances.

            self._merge(i, j)
            cluster_distance = self._average_between_distance()
        else:
            # if there is only one cluster left after the merge, we cannot
            # calculate the between distance as above. we go for "within"
            # instead, which is the only measure of global distances left.

            cluster_distance = self._average_within_distance()
            self._merge(i, j)

        return d / cluster_distance <= self._limit


def agglomerate(points, k, distances, mode='clusters', merge_limit=None, granularity=1.):
    # this is an implementation of heap-based clustering as described
    # by [Kurita1991].

    # runs in O(N^2 log(N)) when N items are clustered, with memory
    # of O(N^2).

    # two notes on this implementation:

    # (1) we work with 0-based indices, whereas Kurita's pseudocode
    # uses 1-based indices.

    # (2) when comparing heap elements h[i], h[j], we actually
    # compare tuples of (distance, index, pair of elements), which
    # guarantees that clusterings will produce the same results
    # no matter how the input data is ordered, and that h[i] != h[j]
    # as long as i != j.

    # parameters:
    # points: list of points to cluster
    # k: number of clusters to form or None for automatic detection
    # distances: an instance of PrecomputedDistances
    # mode: 'clusters' returns clusters, 'dominant' returns one dominant
    # representant of each cluster only, 'components' returns the index of
    # the cluster each element is in for each element.
    # merge_limit: do not merge points above this distance; if the merged
    # distance falls above this limit, the clustering is stopped and the
    # best clustering so far is returned.

    clusters = [[i] for i in range(len(points))]

    def shiftdown(s, heap, where):
        e = len(heap) - 1

        i = s
        j = 2 * i + 1  # i.e. 2 * (i + 1) - 1
        if j > e:
            return

        x, p, _ = xp = heap[i]

        while j <= e:
            if j < e and heap[j] > heap[j + 1]:
                j += 1
            if xp <= heap[j]:
                break

            xx, pp, _ = heap[i] = heap[j]
            where[pp] = i

            i = j
            j = 2 * i + 1

        heap[i] = xp
        where[p] = i

    def shiftup(s, heap, where):
        i = s
        j = (i + 1) // 2 - 1
        if j < 0:
            return

        x, p, _ = xp = heap[i]

        while j >= 0:
            if heap[j] <= xp:
                break

            xx, pp, _ = heap[i] = heap[j]
            where[pp] = i

            i = j
            j = (i + 1) // 2 - 1

        heap[i] = xp
        where[p] = i

    def update(s, xp, heap, where):
        xxpp = heap[s]
        assert xp != xxpp

        if xp > xxpp:
            heap[s] = xp
            shiftdown(s, heap, where)
        else:
            heap[s] = xp
            shiftup(s, heap, where)

    def remove(s, heap, where):
        if s == len(heap) - 1:
            heap = heap[:-1]
        else:
            xx, pp, _ = xxpp = heap[-1]
            heap = heap[:-1]
            where[pp] = s
            update(s, xxpp, heap, where)

        return heap

    def unmerged_pairs(i, j, n):
        # yields all unmerged pairs (1, i), (2, i), ...
        # except for (i, i) and (i, j)

        # example of unmerged_pairs() output (none merged yet):
        # n = 4, i = 1, j = 3
        # unmerged_pairs(1, 3, 4) -> (1, 0), (2, 1)
        # unmerged_pairs(3, 1, 4) -> (3, 0), (3, 2)

        for r in range(i):  # all (i, _)
            if r != j and clusters[r]:  # r is not yet merged?
                yield _index(i, r)

        # skip (i, i)

        for r in range(i + 1, n):  # all (_, i)
            if r != j and clusters[r]:  # r is not yet merged?
                yield _index(r, i)

    def reduce():
        n = len(points)
        triangular_distance_matrix = distances.matrix()

        if k is None:
            criterion = CloserThanAverageCriterion(
                triangular_distance_matrix, n, granularity)
            n_clusters_target = 1
        else:
            criterion = None
            n_clusters_target = k

        pairs = [(points[i], points[j]) for i in range(n) for j in range(i)]
        lookup = [(i, j) for i in range(n) for j in range(i)]

        where = list(range(len(triangular_distance_matrix)))
        heap = [(d, z, u) for d, z, u in zip(
            triangular_distance_matrix, where, pairs)]

        for s in range(len(heap) // 2 - 1, -1, -1):  # ..., 1, 0
            shiftdown(s, heap, where)

        n_clusters = n

        if mode == 'dominant':
            dominant = list(range(n))
            result = dominant
        elif mode in ('clusters', 'components'):
            dominant = None
            result = clusters
        else:
            raise ValueError('illegal mode %s' % mode)

        while len(heap) > 0 and n_clusters > n_clusters_target:
            d, p, _ = heap[0]
            if merge_limit is not None and d > merge_limit:
                break

            i, j = lookup[p]

            if i > j:
                i, j = j, i  # merge later chunk to earlier one to preserve order

            if criterion and not criterion.try_merge(i, j, d):
                break

            heap = remove(where[p], heap, where)  # remove distance (i, j)

            for a, b in zip(unmerged_pairs(i, j, n), unmerged_pairs(j, i, n)):
                y, py, u = heap[where[b]]
                heap = remove(where[b], heap, where)

                x, px, _ = heap[where[a]]
                if y < x:  # compare only values here, and not tuples (x, p)
                    update(where[a], (y, px, u), heap, where)

            if dominant:
                if len(clusters[j]) > len(clusters[i]):
                    dominant[i] = dominant[j]
                dominant[j] = None

            clusters[i].extend(clusters[j])
            clusters[j] = None

            n_clusters -= 1

        if mode == 'components':
            return _components([c for c in clusters if c], n)
        elif mode == 'clusters':
            return [[points[i] for i in sorted(c)] for c in result if c]
        else:
            raise ValueError('illegal mode %s' % mode)

    return reduce()
