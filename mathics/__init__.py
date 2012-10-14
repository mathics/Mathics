# -*- coding: utf8 -*-

import mathics.core # load core to init sage

# force utf8 encoding
import sys, codecs
writer = codecs.getwriter("utf-8")
sys.stdout = writer(sys.stdout)

def get_version():
    version = {}
    
    from mathics.optional import sage_version
    import sympy
    import mpmath
    try:
        import gmpy
        has_GMPY = True
    except ImportError:
        has_GMPY = False
    try:
        import django
        from django.conf import settings
        version['mathics'] = settings.VERSION
        version['django'] = django.get_version()        
    except ImportError:
        from mathics import settings
        version['mathics'] = settings.VERSION
    if sage_version is not None:
        version['sage'] = sage_version
    version['sympy'] = sympy.__version__
    version['mpmath'] = mpmath.__version__
    if has_GMPY:
        version['gmpy'] = gmpy.version()
    return version

def get_version_string(is_server, newlines=False):
    version = get_version()
    result = []
    result.append(u"Mathics %s" % version['mathics'])
    if 'sage' in version:
        result.append(u"on %s" % version['sage'])
    libs = []
    if 'django' in version and is_server:
        libs.append("Django %s" % version['django'])
    if 'gmpy' in version:
        libs.append("GMPY %s" % version['gmpy'])
    libs += ["SymPy %s" % version['sympy'], "mpmath %s" % version['mpmath']]
    result.append(u"using %s" % ", ".join(libs))
    return ("\n" if newlines else " ").join(result)

def print_version(is_server):
    print "\n" + get_version_string(is_server, newlines=True)
    
def print_license():
    print u"""
Copyright (C) 2011 Jan Pöschko
This program comes with ABSOLUTELY NO WARRANTY.
This is free software, and you are welcome to redistribute it
under certain conditions.
See the documentation for the full license.
"""
