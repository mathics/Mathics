#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import unicode_literals
from __future__ import absolute_import

from django import template
from django.utils.safestring import mark_safe
from django.utils.html import escape
import json

from mathics.doc.doc import escape_html

register = template.Library()


@register.filter
def link(object, ajax):
    if object:
        href = object.href(ajax)
        return mark_safe('<a href="%s">%s</a>' % (
            escape(href), object.get_title_html()))
    else:
        return ''


@register.filter
def js(data):
    return mark_safe(json.dumps(data))
