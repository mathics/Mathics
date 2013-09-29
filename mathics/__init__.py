# -*- coding: utf8 -*-

# force utf8 encoding
import sys
import codecs
writer = codecs.getwriter("utf-8")
sys.stdout = writer(sys.stdout)


__version__ = "0.6.0rc1"


def get_version():
    version = {}

    import sympy
    import sympy.mpmath as mpmath

    from django.core.exceptions import ImproperlyConfigured

    try:
        import django
        from django.conf import settings
        version['django'] = django.get_version()
    except (ImportError, ImproperlyConfigured):
        pass
    version['mathics'] = __version__
    version['sympy'] = sympy.__version__
    version['mpmath'] = mpmath.__version__
    version['python'] = sys.subversion[0] + " " + sys.version.split('\n')[0]
    return version


def get_version_string(is_server, newlines=False):
    version = get_version()
    result = []
    result.append(u"Mathics %s" % version['mathics'])
    result.append(u"on %s" % version['python'])
    libs = []
    if 'django' in version and is_server:
        libs.append("Django %s" % version['django'])
    libs += ["SymPy %s" % version['sympy'], "mpmath %s" % version['mpmath']]
    result.append(u"using %s" % ", ".join(libs))
    return ("\n" if newlines else " ").join(result)


def print_version(is_server):
    print "\n" + get_version_string(is_server, newlines=True)


def print_license():
    print u"""
Copyright (C) 2011-2013 The Mathics Team.
This program comes with ABSOLUTELY NO WARRANTY.
This is free software, and you are welcome to redistribute it
under certain conditions.
See the documentation for the full license.
"""
