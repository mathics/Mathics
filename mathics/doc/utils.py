#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import re
import unicodedata

from django.template.defaultfilters import register, stringfilter
from django.utils.functional import keep_lazy_text
from django.utils.safestring import mark_safe

@keep_lazy_text
def slugify_symbol(value):
    value = unicodedata.normalize('NFKD', value).encode('ascii', 'ignore').decode('ascii')
    value = re.sub('[^$`\w\s-]', '', value).strip().lower()
    return mark_safe(re.sub('[-\s`]+', '-', value))


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
