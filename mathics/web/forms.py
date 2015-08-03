#!/usr/bin/env python
# -*- coding: utf-8 -*-

u"""
    Mathics: a general-purpose computer algebra system
    Copyright (C) 2011-2013 The Mathics Team

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

from django import forms


class AjaxForm(forms.Form):
    def as_json(self, general_errors=[]):
        field_errors = dict((key, [unicode(error) for error in errors])
                            for key, errors in self.errors.items())
        gen_errors = general_errors + self.non_field_errors()
        result = {}
        if field_errors:
            result['fieldErrors'] = field_errors
        if gen_errors:
            result['generalErrors'] = gen_errors
        if hasattr(self, 'cleaned_data'):
            result['values'] = self.cleaned_data
        return result


class LoginForm(AjaxForm):
    email = forms.EmailField(max_length=80)
    password = forms.CharField(
        required=False, max_length=40, widget=forms.PasswordInput,
        help_text=(
            u'<p class="helptext">Leave this field empty '
            u"if you don't have an account yet,\n"
            u"or if you have forgotten your pass&shy;word.\n"
            u"A new password will be sent to your e-mail address.</p>")
    )


class SaveForm(AjaxForm):
    name = forms.CharField(
        required=True, max_length=30,
        help_text=('<p class="helptext">Worksheet names '
                   'are not case-sensitive.</p>')
    )
