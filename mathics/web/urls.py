#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import unicode_literals

from django.conf.urls import *

urlpatterns = patterns(
    'mathics.web.views',
    ('^$', 'main_view'),
    ('^ajax/query/$', 'query'),
    ('^ajax/login/$', 'login'),
    ('^ajax/logout/$', 'logout'),
    ('^ajax/save/$', 'save'),
    ('^ajax/open/$', 'open'),
    ('^ajax/getworksheets/$', 'get_worksheets'),
    ('^(?P<ajax>(?:ajax/)?)doc/$', 'doc'),
    ('^ajax/doc/search/$', 'doc_search'),
    ('^(?P<ajax>(?:ajax/)?)doc/(?P<part>[\w-]+)/$',
     'doc_part'),
    ('^(?P<ajax>(?:ajax/)?)doc/(?P<part>[\w-]+)/(?P<chapter>[\w-]+)/$',
     'doc_chapter'),
    ('^(?P<ajax>(?:ajax/)?)doc/(?P<part>[\w-]+)/(?P<chapter>[\w-]+)/'
     '(?P<section>[$\w-]+)/$', 'doc_section'),
)
