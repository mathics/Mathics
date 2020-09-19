#!/usr/bin/env python3
# -*- coding: utf-8 -*-

try:
    import llvmlite
    has_llvmlite = True
except ImportError:
    has_llvmlite = False


if has_llvmlite:
    from .ir import IRGenerator
    from .compile import _compile
    from .base import CompileArg, CompileError
    from .types import *
