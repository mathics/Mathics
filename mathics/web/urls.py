#!/usr/bin/env python3
# -*- coding: utf-8 -*-


from django.conf.urls import url
from mathics.web.views import (
    query,
    main_view,
    login,
    logout,
    save,
    open,
    get_worksheets,
    doc_search,
    doc_part,
    doc_chapter,
    doc_section,
    doc,
)

urlpatterns = [
    # 'mathics.web.views',
    url("^$", main_view),
    url("^ajax/query/$", query),
    url("^ajax/login/$", login),
    url("^ajax/logout/$", logout),
    url("^ajax/save/$", save),
    url("^ajax/open/$", open),
    url("^ajax/getworksheets/$", get_worksheets),
    url("^(?P<ajax>(?:ajax/)?)doc/$", doc),
    url("^ajax/doc/search/$", doc_search),
    url("^(?P<ajax>(?:ajax/)?)doc/(?P<part>[\w-]+)/$", doc_part),
    url("^(?P<ajax>(?:ajax/)?)doc/(?P<part>[\w-]+)/(?P<chapter>[\w-]+)/$", doc_chapter),
    url(
        "^(?P<ajax>(?:ajax/)?)doc/(?P<part>[\w-]+)/(?P<chapter>[\w-]+)/"
        "(?P<section>[$\w-]+)/$",
        doc_section,
    ),
]
