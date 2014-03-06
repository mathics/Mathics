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

import cPickle as pickle
import os
import base64

from mathics.core.expression import ensure_context
from mathics.core.expression import Symbol
from mathics.settings import DEBUG_CONTEXTS


def get_file_time(file):
    try:
        return os.stat(file).st_mtime
    except OSError:
        return 0


def valuesname(name):
    " 'NValues' -> 'n' "

    assert name.startswith('System`'), name
    if name == 'System`Messages':
        return 'messages'
    else:
        return name[7:-6].lower()


class Definitions(object):
    def __init__(self, add_builtin=False, builtin_filename=None):
        super(Definitions, self).__init__()
        self.builtin = {}
        self.user = {}
        self.autoload_stage = False
        self.current_context = 'Global`'
        self.context_path = ['Global`', 'System`']

        if add_builtin:
            from mathics.builtin import modules, contribute
            from mathics.core.evaluation import Evaluation
            from mathics.settings import ROOT_DIR

            loaded = False
            if builtin_filename is not None:
                builtin_dates = [get_file_time(module.__file__)
                                 for module in modules]
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
                        Evaluation(stream.read(), self, timeout=30)
            self.autoload_stage = False

    def get_builtin_names(self):
        return set(self.builtin)

    def get_user_names(self):
        return set(self.user)

    def get_names(self):
        return self.get_builtin_names() | self.get_user_names()

    def lookup_name(self, name):
        """
        Determine the full name (including context) for a symbol name.

        - If the name begins with a context mark, it's in the context
          given by $Context.
        - Otherwise, if it contains a context mark, it's already fully
          specified.
        - Otherwise, it doesn't contain a context mark: try $Context,
          then each element of $ContextPath, taking the first existing
          symbol.
        - Otherwise, it's a new symbol in $Context.
        """

        assert isinstance(name, basestring)

        if '`' in name:
            if name.startswith('`'):
                return self.current_context + name.lstrip('`')
            return name

        with_context = self.current_context + name
        if not self.have_definition(with_context):
            for ctx in self.context_path:
                n = ctx + name
                if self.have_definition(n):
                    return n
        return with_context

    def shorten_name(self, name_with_ctx):
        if DEBUG_CONTEXTS:
            return name_with_ctx

        def in_ctx(name, ctx):
            return name.startswith(ctx) and '`' not in name[len(ctx):]

        if in_ctx(name_with_ctx, self.current_context):
            return name_with_ctx[len(self.current_context):]
        for ctx in self.context_path:
            if in_ctx(name_with_ctx, ctx):
                return name_with_ctx[len(ctx):]
        return name_with_ctx

    def have_definition(self, name):
        return self.get_definition(name, only_if_exists=True) is not None

    def get_definition(self, name, only_if_exists=False):
        name = self.lookup_name(name)
        user = self.user.get(name, None)
        builtin = self.builtin.get(name, None)

        if user is None and builtin is None:
            return None if only_if_exists else Definition(name=name)
        if builtin is None:
            return user
        if user is None:
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
        assert isinstance(name, basestring)
        assert '`' in name
        rules = self.get_definition(name).get_values_list(valuesname(pos))
        for rule in rules:
            result = rule.apply(pattern, evaluation)
            if result is not None:
                return result

    def get_user_definition(self, name, create=True):
        assert not isinstance(name, Symbol)

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
        assert not isinstance(name, Symbol)
        del self.user[self.lookup_name(name)]

    def add_user_definition(self, name, definition):
        assert not isinstance(name, Symbol)
        self.user[self.lookup_name(name)] = definition

    def set_attribute(self, name, attribute):
        definition = self.get_user_definition(self.lookup_name(name))
        definition.attributes.add(attribute)

    def set_attributes(self, name, attributes):
        definition = self.get_user_definition(self.lookup_name(name))
        definition.attributes = set(attributes)

    def clear_attribute(self, name, attribute):
        definition = self.get_user_definition(self.lookup_name(name))
        if attribute in definition.attributes:
            definition.attributes.remove(attribute)

    def add_rule(self, name, rule, position=None):
        name = self.lookup_name(name)
        if position is None:
            return self.get_user_definition(name).add_rule(rule)
        else:
            return self.get_user_definition(name).add_rule_at(rule, position)

    def add_format(self, name, rule, form=''):
        definition = self.get_user_definition(self.lookup_name(name))
        if isinstance(form, tuple) or isinstance(form, list):
            forms = form
        else:
            forms = [form]
        for form in forms:
            if form not in definition.formatvalues:
                definition.formatvalues[form] = []
            insert_rule(definition.formatvalues[form], rule)

    def add_nvalue(self, name, rule):
        definition = self.get_user_definition(self.lookup_name(name))
        definition.add_rule_at(rule, 'n')

    def add_default(self, name, rule):
        definition = self.get_user_definition(self.lookup_name(name))
        definition.add_rule_at(rule, 'default')

    def add_message(self, name, rule):
        definition = self.get_user_definition(self.lookup_name(name))
        definition.add_rule_at(rule, 'messages')

    def set_values(self, name, values, rules):
        pos = valuesname(values)
        definition = self.get_user_definition(self.lookup_name(name))
        definition.set_values_list(pos, rules)

    def get_options(self, name):
        return self.get_definition(self.lookup_name(name)).options

    def reset_user_definitions(self):
        self.user = {}

    def get_user_definitions(self):
        return base64.b64encode(pickle.dumps(self.user, protocol=pickle.HIGHEST_PROTOCOL))

    def set_user_definitions(self, definitions):
        if definitions:
            self.user = pickle.loads(base64.b64decode(definitions))
        else:
            self.user = {}

    def get_ownvalue(self, name):
        ownvalues = self.get_definition(self.lookup_name(name)).ownvalues
        if ownvalues:
            return ownvalues[0]
        return None

    def set_ownvalue(self, name, value):
        from expression import Symbol
        from rules import Rule

        name = self.lookup_name(name)
        self.add_rule(name, Rule(Symbol(name), value))

    def set_options(self, name, options):
        definition = self.get_user_definition(self.lookup_name(name))
        definition.options = options

    def unset(self, name, expr):
        definition = self.get_user_definition(self.lookup_name(name))
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
        elif head_name == 'System`Condition' and len(pattern.leaves) > 0:
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
                 defaultvalues=None, builtin=None):

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
        self.formatvalues = dict((name, list)
                                 for name, list in formatvalues.items())
        self.messages = messages
        self.attributes = set(attributes)
        for a in self.attributes:
            assert '`' in a, "%s attribute %s has no context" % (name, a)
        self.options = options
        self.nvalues = nvalues
        self.defaultvalues = defaultvalues
        self.builtin = builtin

    def get_values_list(self, pos):
        assert pos.isalpha()
        if pos == 'messages':
            return self.messages
        else:
            return getattr(self, '%svalues' % pos)

    def set_values_list(self, pos, rules):
        assert pos.isalpha()
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
