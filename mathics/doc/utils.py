#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import unicode_literals
from __future__ import absolute_import

import re
import unicodedata

from django.template.defaultfilters import register, stringfilter
from django.utils import six
from django.utils.functional import allow_lazy
from django.utils.safestring import mark_safe


def slugify_symbol(value):
    value = unicodedata.normalize('NFKD', value).encode('ascii', 'ignore').decode('ascii')
    value = re.sub('[^$`\w\s-]', '', value).strip().lower()
    return mark_safe(re.sub('[-\s`]+', '-', value))
slugify_symbol = allow_lazy(slugify_symbol, six.text_type)


@register.filter(is_safe=True)
@stringfilter
def slugify(value):
    """
    Converts to lowercase, removes non-word characters apart from '$',
    and converts spaces to hyphens. Also strips leading and trailing
    whitespace.

    Based on the Django version, but modified to preserve '$'.
    """
    return slugify_symbol(value)
