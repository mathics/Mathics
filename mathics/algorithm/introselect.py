#!/usr/bin/env python
# -*- coding: utf-8 -*-

# relevant publications:

# [1] Comm. ACM 4 (7): 321-322.
# [2] Musser, David R. (1997). "Introspective Sorting and Selection Algorithms".
#     Software: Practice and Experience (Wiley) 27 (8): 983–993.
# [3] Blum, M.; Floyd, R. W.; Pratt, V. R.; Rivest, R. L.; Tarjan, R. E. (August 1973).
#     "Time bounds for selection". Journal of Computer and System Sciences 7 (4): 448–461.

# the idea of introselect() is described in [2] and [1]
# the idea of bfprt() is described https://en.wikipedia.org/wiki/Median_of_medians and [3]

from heapq import nsmallest


def _median3(v):
    index_a = 0
    index_b = len(v) - 1
    index_c = index_b // 2

    a = v[index_a]
    b = v[index_b]
    c = v[index_c]

    if a < b:
        if b < c:
            return index_b  # a < b < c
        elif a < c:
            return index_c  # a < c <= b
        else:
            return index_a  # c <= a < b
    else:  # b <= a
        if c < b:
            return index_b  # c < b <= a
        elif c < a:
            return index_c  # b <= c < a
        else:
            return index_a  # b <= a <= c


def _median5(a):  # for len(a) <= 5
    median = len(a) // 2
    return nsmallest(median + 1, [(x, i) for i, x in enumerate(a)])[median][1]


def _partition(a, f):
    # this is a pythonized version of Hoare's original Algorithm 63 "Partition"
    # without the random element, from [1]

    n = len(a) - 1
    i = 0
    j = n
    pivot = a[f]

    while True:
        while i < n and a[i] <= pivot:
            i += 1
        while j > 0 and a[j] >= pivot:
            j -= 1
        if i >= j:
            break
        a[i], a[j] = a[j], a[i]
        i += 1
        j -= 1

    # the following step tries to make sure the original pivot element is
    # in [i, j]. this is not strictly necessary, but it might pinpoint the
    # sort position of one more element at this point, which is good.

    if f > i:
        a[i], a[f] = a[f], a[i]
        i += 1
    elif f < j:
        a[f], a[j] = a[j], a[f]
        j -= 1

    # leaving here, we know three things (see Hoare's original description):

    # (1) a[r] <= pivot for r <= j
    # (2) a[r] = pivot for j < r < i
    # (3) a[r] >= pivot for r >= i

    # note that the elements a[r] for j < r < i have already reached their
    # final sorting position in the array, i.e. if we look for the location
    # of one of these, we're done.

    return j, i


def bfprt(a, k):  # changes a
    while True:
        if len(a) == 1:
            return a[0]

        median_indices = [_median5(a[i:i + 5]) for i in range(0, len(a), 5)]
        median_values = [a[5 * b + i] for b, i in enumerate(median_indices)]
        pivot = bfprt(median_values[:], len(median_values) // 2)
        pivot_block_index = [id(x) for x in median_values].index(id(pivot))
        pivot_index = 5 * pivot_block_index + median_indices[pivot_block_index]

        i, j = _partition(a, pivot_index)
        if k <= i:
            a = a[:i + 1]
        elif k >= j:
            a = a[j:]
            k -= j
        else:
            return a[k]


def introselect(a, k):  # changes a
    depth = len(a).bit_length() * 2  # see algorithm INTROSORT in [2], page 5

    while len(a) >= 3 and depth > 0:
        p = _median3(a)

        # see Algorithm 65 FIND in [1]
        i, j = _partition(a, p)
        if k <= i:
            a = a[:i + 1]
        elif k >= j:
            a = a[j:]
            k -= j
        else:
            return a[k]

        depth -= 1

    if depth <= 0 and len(a) > 3:
        return bfprt(a, k)
    else:
        return sorted(a)[k]


if __name__ == "__main__":
    import random

    def test_algorithm(l, r_max, name, f):
        a = [random.randint(-r_max, r_max) for _ in range(l)]
        b = sorted(a)
        c = [f(a[:], i) for i in range(len(a))]
        if b == c:
            print('OK %s r: %d l: %d' % (name, r_max, l))
            return True
        else:
            print('FAIL %s r: %d l: %d' % (name, r_max, l))
            print(a, b, c)
            return False

    def test_configuration(l_max, r_max):
        for l in range(l_max):
            if not test_algorithm(l, r_max, 'bfprt', bfprt):
                return False

        for l in range(l_max):
            if not test_algorithm(l, r_max, 'introselect', introselect):
                return False

        return True

    def test_range(l_max):
        # we test two cases: many same elements, and few same elements.
        return test_configuration(l_max, 10) and test_configuration(200, 1000)

    def test_median(median, l_min, l_max, repeat):
        for length in range(l_min, l_max + 1):
            for i in range(repeat):
                a = [random.randint(-10, 10) for _ in range(length)]
                b = sorted(a)
                index = median(a)
                if a[index] != b[len(b) // 2]:
                    print('FAIL median', a, b)
                    return False
                else:
                    print('OK median %d %d' % (length, i))
        return True

    def test_medians(repeat):
        return test_median(_median3, 3, 3, repeat) and test_median(_median5, 1, 5, repeat)

    if test_medians(10000) and test_range(200):
        print('ALL OK.')
    else:
        print('ABORTED WITH FAILURE.')
