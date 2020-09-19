#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from django import forms


class AjaxForm(forms.Form):
    def as_json(self, general_errors=[]):
        field_errors = dict((key, [str(error) for error in errors])
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
            '<p class="helptext">Leave this field empty '
            "if you don't have an account yet,\n"
            "or if you have forgotten your pass&shy;word.\n"
            "A new password will be sent to your e-mail address.</p>")
    )


class SaveForm(AjaxForm):
    name = forms.CharField(
        required=True, max_length=30,
        help_text=('<p class="helptext">Worksheet names '
                   'are not case-sensitive.</p>')
    )
