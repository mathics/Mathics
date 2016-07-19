#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import unicode_literals


try:
    from functools import lru_cache
except:
    def lru_cache(maxsize=128, typed=False):
        # TODO
        def wraps(f):
            f.cache_info = lambda: 'CacheInfo()'
            return f
        return wraps
