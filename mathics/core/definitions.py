# -*- coding: utf8 -*-

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

import pickle
import traceback
import sys
import os


def get_file_time(file):
    try:
        return os.stat(file).st_mtime
    except OSError:
        return 0


def valuesname(name):
    " 'NValues' -> 'n' "

    if name == 'Messages':
        return 'messages'
    else:
        return name[:-6].lower()


class Definitions(object):
    def __init__(self, add_builtin=False, builtin_filename=None):
        super(Definitions, self).__init__()
        self.builtin = {}
        self.user = {}
        self.autoload_stage = False

        if add_builtin:
            from mathics.builtin import modules, contribute
            from mathics.core.expression import builtin_evaluation
            from mathics.core.evaluation import Evaluation
            from mathics.settings import ROOT_DIR

            loaded = False
            if builtin_filename is not None:
                builtin_dates = [get_file_time(
                    module.__file__) for module in modules]
                builtin_time = max(builtin_dates)
                if get_file_time(builtin_filename) > builtin_time:
                    builtin_file = open(builtin_filename, 'r')
                    self.builtin = pickle.load(builtin_file)
                    loaded = True
            if not loaded:
                contribute(self)
                if builtin_filename is not None:
                    builtin_file = open(builtin_filename, 'w')
                    pickle.dump(self.builtin, builtin_file, -1)

            self.autoload_stage = True
            for root, dirs, files in os.walk(   # noqa
                os.path.join(ROOT_DIR, 'autoload')):

                for f in filter(lambda x: x.endswith('.m'), files):
                    with open(os.path.join(root, f)) as stream:
                        evaluation = Evaluation(
                            stream.read(), self, timeout=30)
            self.autoload_stage = False

    def get_builtin_names(self):
        return set(self.builtin)

    def get_user_names(self):
        return set(self.user)

    def get_names(self):
        return self.get_builtin_names() | self.get_user_names()

    def get_definition(self, name):
        user = self.user.get(name, None)
        builtin = self.builtin.get(name, None)

        if builtin:
            context = 'System`'
        else:
            context = 'Global`'

        if user is None and builtin is None:
            return Definition(name=name, context=context)
        if builtin is None:
            user.context = context
            return user
        if user is None:
            builtin.context = context
            return builtin

        if user:
            attributes = user.attributes
        elif builtin:
            attributes = builtin.attributes
        else:
            attributes = set()
        if not user:
            user = Definition(name=name)
        if not builtin:
            builtin = Definition(name=name)
        options = builtin.options.copy()
        options.update(user.options)
        formatvalues = builtin.formatvalues.copy()
        for form, rules in user.formatvalues.iteritems():
            if form in formatvalues:
                formatvalues[form].extend(rules)
            else:
                formatvalues[form] = rules

        return Definition(name=name,
                          ownvalues=user.ownvalues + builtin.ownvalues,
                          downvalues=user.downvalues + builtin.downvalues,
                          subvalues=user.subvalues + builtin.subvalues,
                          upvalues=user.upvalues + builtin.upvalues,
                          formatvalues=formatvalues,
                          messages=user.messages + builtin.messages,
                          attributes=attributes,
                          options=options,
                          nvalues=user.nvalues + builtin.nvalues,
                          defaultvalues=user.defaultvalues +
                          builtin.defaultvalues,
                          context=context,
                          )

    def get_attributes(self, name):
        return self.get_definition(name).attributes

    def get_ownvalues(self, name):
        return self.get_definition(name).ownvalues

    def get_downvalues(self, name):
        return self.get_definition(name).downvalues

    def get_subvalues(self, name):
        return self.get_definition(name).subvalues

    def get_upvalues(self, name):
        return self.get_definition(name).upvalues

    def get_formats(self, name, format=''):
        formats = self.get_definition(name).formatvalues
        result = formats.get(format, []) + formats.get('', [])
        result.sort()
        return result

    def get_nvalues(self, name):
        return self.get_definition(name).nvalues

    def get_defaultvalues(self, name):
        return self.get_definition(name).defaultvalues

    def get_value(self, name, pos, pattern, evaluation):
        rules = self.get_definition(name).get_values_list(valuesname(pos))
        for rule in rules:
            result = rule.apply(pattern, evaluation)
            if result is not None:
                return result

    def get_user_definition(self, name, create=True):
        if self.autoload_stage:
            existing = self.builtin.get(name)
            if existing is None:
                if not create:
                    return None
                self.builtin[name] = Definition(name=name, attributes=set())
                return self.builtin[name]

        existing = self.user.get(name)
        if existing:
            return existing
        else:
            if not create:
                return None
            builtin = self.builtin.get(name)
            if builtin:
                attributes = builtin.attributes
            else:
                attributes = set()
            self.user[name] = Definition(name=name, attributes=attributes)
            return self.user[name]

    def reset_user_definition(self, name):
        del self.user[name]

    def add_user_definition(self, name, definition):
        self.user[name] = definition

    def set_attribute(self, name, attribute):
        definition = self.get_user_definition(name)
        definition.attributes.add(attribute)

    def set_attributes(self, name, attributes):
        definition = self.get_user_definition(name)
        definition.attributes = set(attributes)

    def clear_attribute(self, name, attribute):
        definition = self.get_user_definition(name)
        if attribute in definition.attributes:
            definition.attributes.remove(attribute)

    def add_rule(self, name, rule, position=None):
        if position is None:
            return self.get_user_definition(name).add_rule(rule)
        else:
            return self.get_user_definition(name).add_rule_at(rule, position)

    def add_format(self, name, rule, form=''):
        definition = self.get_user_definition(name)
        if isinstance(form, tuple) or isinstance(form, list):
            forms = form
        else:
            forms = [form]
        for form in forms:
            if form not in definition.formatvalues:
                definition.formatvalues[form] = []
            insert_rule(definition.formatvalues[form], rule)

    def add_nvalue(self, name, rule):
        definition = self.get_user_definition(name)
        definition.add_rule_at(rule, 'n')

    def add_default(self, name, rule):
        definition = self.get_user_definition(name)
        definition.add_rule_at(rule, 'default')

    def add_message(self, name, rule):
        definition = self.get_user_definition(name)
        definition.add_rule_at(rule, 'messages')

    def set_values(self, name, values, rules):
        pos = valuesname(values)
        definition = self.get_user_definition(name)
        definition.set_values_list(pos, rules)

    def get_options(self, name):
        return self.get_definition(name).options

    def reset_user_definitions(self):
        self.user = {}

    def get_user_definitions(self):
        return pickle.dumps(self.user, protocol=pickle.HIGHEST_PROTOCOL)

    def set_user_definitions(self, definitions):
        if definitions:
            self.user = pickle.loads(definitions)
        else:
            self.user = {}

    def get_ownvalue(self, name):
        ownvalues = self.get_definition(name).ownvalues
        if ownvalues:
            return ownvalues[0]
        return None

    def set_ownvalue(self, name, value):
        from expression import Symbol
        from rules import Rule

        self.add_rule(name, Rule(Symbol(name), value))

    def set_options(self, name, options):
        definition = self.get_user_definition(name)
        definition.options = options

    def unset(self, name, expr):
        definition = self.get_user_definition(name)
        return definition.remove_rule(expr)


def get_tag_position(pattern, name):
    if pattern.get_name() == name:
        return 'own'
    elif pattern.is_atom():
        return None
    else:
        head_name = pattern.get_head_name()
        if head_name == name:
            return 'down'
        elif head_name == 'Condition' and len(pattern.leaves) > 0:
            return get_tag_position(pattern.leaves[0], name)
        elif pattern.get_lookup_name() == name:
            return 'sub'
        else:
            for leaf in pattern.leaves:
                if leaf.get_lookup_name() == name:
                    return 'up'
        return None


def insert_rule(values, rule):
    for index, existing in enumerate(values):
        if existing.pattern.same(rule.pattern):
            del values[index]
            break
    values.insert(0, rule)
    values.sort()


class Definition(object):
    def __init__(self, name, rules=None, ownvalues=None, downvalues=None,
                 subvalues=None, upvalues=None, formatvalues=None,
                 messages=None, attributes=(), options=None, nvalues=None,
                 defaultvalues=None, builtin=None, context='Global`'):

        super(Definition, self).__init__()
        self.name = name

        if rules is None:
            rules = []
        if ownvalues is None:
            ownvalues = []
        if downvalues is None:
            downvalues = []
        if subvalues is None:
            subvalues = []
        if upvalues is None:
            upvalues = []
        if formatvalues is None:
            formatvalues = {}
        if options is None:
            options = {}
        if nvalues is None:
            nvalues = []
        if defaultvalues is None:
            defaultvalues = []
        if messages is None:
            messages = []

        self.ownvalues = ownvalues
        self.downvalues = downvalues
        self.subvalues = subvalues
        self.upvalues = upvalues
        for rule in rules:
            self.add_rule(rule)
        self.formatvalues = dict((
            name, list) for name, list in formatvalues.items())
        self.messages = messages
        self.attributes = set(attributes)
        self.options = options
        self.nvalues = nvalues
        self.defaultvalues = defaultvalues
        self.builtin = builtin
        self.context = context

    def get_values_list(self, pos):
        if pos == 'messages':
            return self.messages
        else:
            return getattr(self, '%svalues' % pos)

    def set_values_list(self, pos, rules):
        if pos == 'messages':
            self.messages = rules
        else:
            setattr(self, '%svalues' % pos, rules)

    def add_rule_at(self, rule, position):
        values = self.get_values_list(position)
        insert_rule(values, rule)
        return True

    def add_rule(self, rule):
        pos = get_tag_position(rule.pattern, self.name)
        if pos:
            return self.add_rule_at(rule, pos)
        return False

    def remove_rule(self, lhs):
        position = get_tag_position(lhs, self.name)
        if position:
            values = self.get_values_list(position)
            for index, existing in enumerate(values):
                if existing.pattern.expr.same(lhs):
                    del values[index]
                    return True
        return False

    def __repr__(self):
        return (
            '<Definition: name: %s, '
            'downvalues: %s, formats: %s, attributes: %s>') % (
                self.name, self.downvalues, self.formatvalues, self.attributes)
