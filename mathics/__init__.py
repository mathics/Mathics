# -*- coding: utf8 -*-

import mathics.core # load core to init sage

#try:
#    from sage import all
#    # load Sage right on start of Mathics to prevent error in mathicsserver:
#    # "ValueError: signal only works in main thread"
#except ImportError:
#    pass

#import sys
#sys.setrecursionlimit(3000)

def print_version(is_server):
    from mathics.optional import sage_version
    import sympy
    import mpmath
    import gmpy
    try:
        import django
        from django.conf import settings
        version = settings.VERSION
        django_version = django.get_version()        
    except ImportError:
        from mathics import settings
        version = settings.VERSION
        django_version = None
    
    print u"\nMathics %s" % settings.VERSION
    #print u"Copyright (c) 2009-2010 by Jan Pöschko"
    if sage_version is not None:
        print u"on %s" % sage_version
    libs = []
    if django_version is not None and is_server:
        libs.append("Django %s" % django_version)
    libs += ["SymPy %s" % sympy.__version__, "mpmath %s" % mpmath.__version__,
        "GMPY %s" % gmpy.version()]
    print u"using %s" % ", ".join(libs)
    
def print_license():
    print u"""
Copyright (C) 2011 Jan Pöschko
This program comes with ABSOLUTELY NO WARRANTY.
This is free software, and you are welcome to redistribute it
under certain conditions.
See the documentation for the full license.
"""