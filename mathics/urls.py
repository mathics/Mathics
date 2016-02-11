#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import unicode_literals

from __future__ import absolute_import
from django.conf.urls import *

from django.conf import settings

# Uncomment the next two lines to enable the admin:
# from django.contrib import admin
# admin.autodiscover()

handler404 = 'mathics.web.views.error_404_view'
handler500 = 'mathics.web.views.error_500_view'

urlpatterns = patterns(
    '',
    (r'^media/(?P<path>.*)$', 'django.views.static.serve', {
        'document_root': settings.MEDIA_ROOT, 'show_indexes': False}),
    (r'^', include('mathics.web.urls')),

    # Uncomment the admin/doc line below and add 'django.contrib.admindocs'
    # to INSTALLED_APPS to enable admin documentation:
    # (r'^admin/doc/', include('django.contrib.admindocs.urls')),

    # Uncomment the next line to enable the admin:
    # (r'^admin/(.*)', admin.site.root),
)
