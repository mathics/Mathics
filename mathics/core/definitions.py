#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import unicode_literals
from __future__ import absolute_import

import six
import six.moves.cPickle as pickle

import os
import base64
import re

from mathics.core.expression import (Expression, Symbol, String, ensure_context,
                                     fully_qualified_symbol_name)
from mathics.core.characters import letters, letterlikes


names_wildcards = "@*"
base_names_pattern = r'((?![0-9])([0-9${0}{1}{2}])+)'.format(letters, letterlikes, names_wildcards)
full_names_pattern = r'(`?{0}(`{0})*)'.format(base_names_pattern)


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
                    builtin_file = open(builtin_filename, 'rb')
                    self.builtin = pickle.load(builtin_file)
                    loaded = True
            if not loaded:
                contribute(self)
                if builtin_filename is not None:
                    builtin_file = open(builtin_filename, 'wb')
                    pickle.dump(self.builtin, builtin_file, -1)

            for root, dirs, files in os.walk(   # noqa
                os.path.join(ROOT_DIR, 'autoload')):

                for path in [os.path.join(root, f) for f in files
                             if f.endswith('.m')]:
                    Expression('Get', String(path)).evaluate(Evaluation(self))

            # Move any user definitions created by autoloaded files to
            # builtins, and clear out the user definitions list. This
            # means that any autoloaded definitions become shared
            # between users and no longer disappear after a Quit[].
            #
            # Autoloads that accidentally define a name in Global`
            # could cause confusion, so check for this.
            #
            if any([name.startswith('Global`') for name in self.user]):
                raise ValueError("autoload defined a Global` symbol")
            self.builtin.update(self.user)
            self.user = {}

    def get_current_context(self):
        # It's crucial to specify System` in this get_ownvalue() call,
        # otherwise we'll end up back in this function and trigger
        # infinite recursion.
        context_rule = self.get_ownvalue('System`$Context')
        context = context_rule.replace.get_string_value()
        assert context is not None, "$Context somehow set to an invalid value"
        return context

    def get_context_path(self):
        context_path_rule = self.get_ownvalue('System`$ContextPath')
        context_path = context_path_rule.replace
        assert context_path.has_form('System`List', None)
        context_path = [c.get_string_value() for c in context_path.leaves]
        assert not any([c is None for c in context_path])
        return context_path

    def set_current_context(self, context):
        assert isinstance(context, six.string_types)
        self.set_ownvalue('System`$Context', String(context))

    def set_context_path(self, context_path):
        assert isinstance(context_path, list)
        assert all([isinstance(c, six.string_types) for c in context_path])
        self.set_ownvalue('System`$ContextPath',
                          Expression('System`List',
                                     *[String(c) for c in context_path]))

    def get_builtin_names(self):
        return set(self.builtin)

    def get_user_names(self):
        return set(self.user)

    def get_names(self):
        return self.get_builtin_names() | self.get_user_names()

    def get_accessible_contexts(self):
        "Return the contexts reachable though $Context or $ContextPath."
        accessible_ctxts = set(self.get_context_path())
        accessible_ctxts.add(self.get_current_context())
        return accessible_ctxts

    def get_matching_names(self, pattern):
        """
        Return a list of the symbol names matching a string pattern.

        A pattern containing a context mark (of the form
        "ctx_pattern`short_pattern") matches symbols whose context and
        short name individually match the two patterns. A pattern
        without a context mark matches symbols accessible through
        $Context and $ContextPath whose short names match the pattern.

        '*' matches any sequence of symbol characters or an empty
        string. '@' matches a non-empty sequence of symbol characters
        which aren't uppercase letters. In the context pattern, both
        '*' and '@' match context marks.
        """

        if re.match(full_names_pattern, pattern) is None:
            # The pattern contained characters which weren't allowed
            # in symbols and aren't valid wildcards. Hence, the
            # pattern can't match any symbols.
            return []

        # If we get here, there aren't any regexp metacharacters in
        # the pattern.

        if '`' in pattern:
            ctx_pattern, short_pattern = pattern.rsplit('`', 1)
            ctx_pattern = ((ctx_pattern + '`')
                           .replace('@', '[^A-Z`]+')
                           .replace('*', '.*')
                           .replace('$', r'\$'))
        else:
            short_pattern = pattern
            # start with a group matching the accessible contexts
            ctx_pattern = "(?:%s)" % "|".join(
                re.escape(c) for c in self.get_accessible_contexts())

        short_pattern = (short_pattern
                         .replace('@', '[^A-Z]+')
                         .replace('*', '[^`]*')
                         .replace('$', r'\$'))
        regex = re.compile('^' + ctx_pattern + short_pattern + '$')

        return [name for name in self.get_names() if regex.match(name)]

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

        assert isinstance(name, six.string_types)

        # Bail out if the name we're being asked to look up is already
        # fully qualified.
        if fully_qualified_symbol_name(name):
            return name

        current_context = self.get_current_context()

        if '`' in name:
            if name.startswith('`'):
                return current_context + name.lstrip('`')
            return name

        with_context = current_context + name
        if not self.have_definition(with_context):
            for ctx in self.get_context_path():
                n = ctx + name
                if self.have_definition(n):
                    return n
        return with_context

    def shorten_name(self, name_with_ctx):
        if '`' not in name_with_ctx:
            return name_with_ctx

        def in_ctx(name, ctx):
            return name.startswith(ctx) and '`' not in name[len(ctx):]

        if in_ctx(name_with_ctx, self.get_current_context()):
            return name_with_ctx[len(self.get_current_context()):]
        for ctx in self.get_context_path():
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
        for form, rules in six.iteritems(user.formatvalues):
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
        assert isinstance(name, six.string_types)
        assert '`' in name
        rules = self.get_definition(name).get_values_list(valuesname(pos))
        for rule in rules:
            result = rule.apply(pattern, evaluation)
            if result is not None:
                return result

    def get_user_definition(self, name, create=True):
        assert not isinstance(name, Symbol)

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
        if six.PY2:
            return base64.encodestring(pickle.dumps(self.user, protocol=2)).decode('ascii')
        else:
            return base64.encodebytes(pickle.dumps(self.user, protocol=2)).decode('ascii')

    def set_user_definitions(self, definitions):
        if definitions:
            if six.PY2:
                self.user = pickle.loads(base64.decodestring(definitions.encode('ascii')))
            else:
                self.user = pickle.loads(base64.decodebytes(definitions.encode('ascii')))
        else:
            self.user = {}

    def get_ownvalue(self, name):
        ownvalues = self.get_definition(self.lookup_name(name)).ownvalues
        if ownvalues:
            return ownvalues[0]
        return None

    def set_ownvalue(self, name, value):
        from .expression import Symbol
        from .rules import Rule

        name = self.lookup_name(name)
        self.add_rule(name, Rule(Symbol(name), value))

    def set_options(self, name, options):
        definition = self.get_user_definition(self.lookup_name(name))
        definition.options = options

    def unset(self, name, expr):
        definition = self.get_user_definition(self.lookup_name(name))
        return definition.remove_rule(expr)

    def get_line(self):
        'returns current line number'
        line = self.get_definition('$Line').ownvalues
        if line:
            return line[0].replace.get_int_value()
        else:
            return 1    # user may have deleted $Line (e.g. by calling Quit[])


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
        s = '<Definition: name: {}, downvalues: {}, formats: {}, attributes: {}>'.format(
            self.name, self.downvalues, self.formatvalues, self.attributes)
        return s.encode('unicode_escape')
