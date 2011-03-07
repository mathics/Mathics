# -*- coding: UTF-8 -*-

u"""
    Mathics: a general-purpose computer algebra system
    Copyright (C) 2011 Jan Pöschko

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
"""

from django.conf.urls.defaults import *

urlpatterns = patterns('mathics.web.views',
    ('^$', 'main_view'),
    ('^test/$', 'test_view'),
    ('^ajax/query/$', 'query'),
    ('^ajax/login/$', 'login'),
    ('^ajax/logout/$', 'logout'),
    ('^ajax/save/$', 'save'),
    ('^ajax/open/$', 'open'),
    ('^ajax/getworksheets/$', 'get_worksheets'),
    ('^(?P<ajax>(?:ajax/)?)doc/$', 'doc'),
    ('^ajax/doc/search/$', 'doc_search'),
    ('^(?P<ajax>(?:ajax/)?)doc/(?P<part>[\w-]+)/$', 'doc_part'),
    ('^(?P<ajax>(?:ajax/)?)doc/(?P<part>[\w-]+)/(?P<chapter>[\w-]+)/$', 'doc_chapter'),
    ('^(?P<ajax>(?:ajax/)?)doc/(?P<part>[\w-]+)/(?P<chapter>[\w-]+)/(?P<section>[\w-]+)/$', 'doc_section'),
)