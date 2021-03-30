#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import pickle

import os
import base64
import re
import bisect

from collections import defaultdict

import typing

from mathics.core.expression import (
    Expression,
    Symbol,
    String,
    fully_qualified_symbol_name,
    strip_context,
)
from mathics_scanner.tokeniser import full_names_pattern

type_compiled_pattern = type(re.compile("a.a"))


def get_file_time(file) -> float:
    try:
        return os.stat(file).st_mtime
    except OSError:
        return 0


def valuesname(name) -> str:
    " 'NValues' -> 'n' "

    assert name.startswith("System`"), name
    if name == "System`Messages":
        return "messages"
    else:
        return name[7:-6].lower()


class PyMathicsLoadException(Exception):
    def __init__(self, module):
        self.name = module + " is not a valid pymathics module"
        self.module = module


class Definitions(object):
    def __init__(
        self, add_builtin=False, builtin_filename=None, extension_modules=[]
    ) -> None:
        super(Definitions, self).__init__()
        self.builtin = {}
        self.user = {}
        self.pymathics = {}
        self.definitions_cache = {}
        self.lookup_cache = {}
        self.proxy = defaultdict(set)
        self.now = 0  # increments whenever something is updated
        self._packages = []

        if add_builtin:
            from mathics.builtin import modules, contribute
            from mathics.core.evaluation import Evaluation
            from mathics.settings import ROOT_DIR

            loaded = False
            if builtin_filename is not None:
                builtin_dates = [get_file_time(module.__file__) for module in modules]
                builtin_time = max(builtin_dates)
                if get_file_time(builtin_filename) > builtin_time:
                    builtin_file = open(builtin_filename, "rb")
                    self.builtin = pickle.load(builtin_file)
                    loaded = True
            if not loaded:
                contribute(self)
                for module in extension_modules:
                    try:
                        loaded_module = self.load_pymathics_module(
                            module, remove_on_quit=False
                        )
                    except PyMathicsLoadException as e:
                        raise
                    except ImportError as e:
                        raise

                if builtin_filename is not None:
                    builtin_file = open(builtin_filename, "wb")
                    pickle.dump(self.builtin, builtin_file, -1)

            # Load symbols from the autoload folder
            for root, dirs, files in os.walk(os.path.join(ROOT_DIR, "autoload")):
                for path in [os.path.join(root, f) for f in files if f.endswith(".m")]:
                    Expression("Get", String(path)).evaluate(Evaluation(self))

            # Move any user definitions created by autoloaded files to
            # builtins, and clear out the user definitions list. This
            # means that any autoloaded definitions become shared
            # between users and no longer disappear after a Quit[].
            #
            # Autoloads that accidentally define a name in Global`
            # could cause confusion, so check for this.
            #
            for name in self.user:
                if name.startswith("Global`"):
                    raise ValueError("autoload defined %s." % name)
            self.builtin.update(self.user)
            self.user = {}
            self.clear_cache()

    def load_pymathics_module(self, module, remove_on_quit=True):
        """
        Loads Mathics builtin objects and their definitions
        from an external Python module in the pymathics module namespace.
        """
        import importlib
        from mathics.builtin import is_builtin, builtins_by_module, Builtin

        # Ensures that the pymathics module be reloaded
        import sys

        if module in sys.modules:
            loaded_module = importlib.reload(sys.modules[module])
        else:
            loaded_module = importlib.import_module(module)

        builtins_by_module[loaded_module.__name__] = []
        vars = set(
            loaded_module.__all__
            if hasattr(loaded_module, "__all__")
            else dir(loaded_module)
        )

        newsymbols = {}
        if not ("pymathics_version_data" in vars):
            raise PyMathicsLoadException(module)
        for name in vars - set(("pymathics_version_data", "__version__")):
            var = getattr(loaded_module, name)
            if (
                hasattr(var, "__module__")
                and is_builtin(var)
                and not name.startswith("_")
                and var.__module__[: len(loaded_module.__name__)]
                == loaded_module.__name__
            ):  # nopep8
                instance = var(expression=False)
                if isinstance(instance, Builtin):
                    if not var.context:
                        var.context = "Pymathics`"
                    symbol_name = instance.get_name()
                    builtins_by_module[loaded_module.__name__].append(instance)
                    newsymbols[symbol_name] = instance

        for name in newsymbols:
            luname = self.lookup_name(name)
            self.user.pop(name, None)

        for name, item in newsymbols.items():
            if name != "System`MakeBoxes":
                item.contribute(self, is_pymodule=True)

        onload = loaded_module.pymathics_version_data.get("onload", None)
        if onload:
            onload(self)

        return loaded_module

    def clear_pymathics_modules(self):
        from mathics.builtin import builtins, builtins_by_module

        for key in list(builtins_by_module.keys()):
            if not key.startswith("mathics."):
                del builtins_by_module[key]
        for key in self.pymathics:
            del self.pymathics[key]

        self.pymathics = {}
        return None

    def clear_cache(self, name=None):
        # the definitions cache (self.definitions_cache) caches (incomplete and complete) names -> Definition(),
        # e.g. "xy" -> d and "MyContext`xy" -> d. we need to clear this cache if a Definition() changes (which
        # would happen if a Definition is combined from a builtin and a user definition and some content in the
        # user definition is updated) or if the lookup rules change and we could end up at a completely different
        # Definition.

        # the lookup cache (self.lookup_cache) caches what lookup_name() does. we only need to update this if some
        # change happens that might change the result lookup_name() calculates. we do not need to change it if a
        # Definition() changes.

        # self.proxy keeps track of all the names we cache. if we need to clear the caches for only one name, e.g.
        # 'MySymbol', then we need to be able to look up all the entries that might be related to it, e.g. 'MySymbol',
        # 'A`MySymbol', 'C`A`MySymbol', and so on. proxy identifies symbols using their stripped name and thus might
        # give us symbols in other contexts that are actually not affected. still, this is a safe solution.

        if name is None:
            self.definitions_cache = {}
            self.lookup_cache = {}
            self.proxy = defaultdict(set)
        else:
            definitions_cache = self.definitions_cache
            lookup_cache = self.lookup_cache
            tail = strip_context(name)
            for k in self.proxy.pop(tail, []):
                definitions_cache.pop(k, None)
                lookup_cache.pop(k, None)

    def clear_definitions_cache(self, name) -> None:
        definitions_cache = self.definitions_cache
        tail = strip_context(name)
        for k in self.proxy.pop(tail, []):
            definitions_cache.pop(k, None)

    def has_changed(self, maximum, symbols):
        # timestamp for the most recently changed part of a given expression.
        for name in symbols:
            symb = self.get_definition(name, only_if_exists=True)
            if symb is None:
                # symbol doesn't exist so it was never changed
                pass
            else:
                changed = getattr(symb, "changed", None)
                if changed is None:
                    # must be system symbol
                    symb.changed = 0
                elif changed > maximum:
                    return True

        return False

    def get_current_context(self):
        # It's crucial to specify System` in this get_ownvalue() call,
        # otherwise we'll end up back in this function and trigger
        # infinite recursion.
        context_rule = self.get_ownvalue("System`$Context")
        context = context_rule.replace.get_string_value()
        assert context is not None, "$Context somehow set to an invalid value"
        return context

    def get_context_path(self):
        context_path_rule = self.get_ownvalue("System`$ContextPath")
        context_path = context_path_rule.replace
        assert context_path.has_form("System`List", None)
        context_path = [c.get_string_value() for c in context_path.leaves]
        assert not any([c is None for c in context_path])
        return context_path

    def set_current_context(self, context) -> None:
        assert isinstance(context, str)
        self.set_ownvalue("System`$Context", String(context))
        self.clear_cache()

    def set_context_path(self, context_path) -> None:
        assert isinstance(context_path, list)
        assert all([isinstance(c, str) for c in context_path])
        self.set_ownvalue(
            "System`$ContextPath",
            Expression("System`List", *[String(c) for c in context_path]),
        )
        self.clear_cache()

    def get_builtin_names(self):
        return set(self.builtin)

    def get_user_names(self):
        return set(self.user)

    def get_pymathics_names(self):
        return set(self.pymathics)

    def get_names(self):
        return (
            self.get_builtin_names()
            | self.get_pymathics_names()
            | self.get_user_names()
        )

    def get_accessible_contexts(self):
        "Return the contexts reachable though $Context or $ContextPath."
        accessible_ctxts = set(self.get_context_path())
        accessible_ctxts.add(self.get_current_context())
        return accessible_ctxts

    def get_matching_names(self, pattern) -> typing.List[str]:
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
        if isinstance(pattern, type_compiled_pattern):
            regex = pattern
        else:
            if re.match(full_names_pattern, pattern) is None:
                # The pattern contained characters which weren't allowed
                # in symbols and aren't valid wildcards. Hence, the
                # pattern can't match any symbols.
                return []

            # If we get here, there aren't any regexp metacharacters in
            # the pattern.

            if "`" in pattern:
                ctx_pattern, short_pattern = pattern.rsplit("`", 1)
                if ctx_pattern == "":
                    ctx_pattern = "System`"
                else:
                    ctx_pattern = (
                        (ctx_pattern + "`")
                        .replace("@", "[^A-Z`]+")
                        .replace("*", ".*")
                        .replace("$", r"\$")
                    )
            else:
                short_pattern = pattern
                # start with a group matching the accessible contexts
                ctx_pattern = "(?:%s)" % "|".join(
                    re.escape(c) for c in self.get_accessible_contexts()
                )

            short_pattern = (
                short_pattern.replace("@", "[^A-Z]+")
                .replace("*", "[^`]*")
                .replace("$", r"\$")
            )
            regex = re.compile("^" + ctx_pattern + short_pattern + "$")

        return [name for name in self.get_names() if regex.match(name)]

    def lookup_name(self, name) -> str:
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

        cached = self.lookup_cache.get(name, None)
        if cached is not None:
            return cached

        assert isinstance(name, str)

        # Bail out if the name we're being asked to look up is already
        # fully qualified.
        if fully_qualified_symbol_name(name):
            return name

        current_context = self.get_current_context()

        if "`" in name:
            if name.startswith("`"):
                return current_context + name.lstrip("`")
            return name

        with_context = current_context + name
        if not self.have_definition(with_context):
            for ctx in self.get_context_path():
                n = ctx + name
                if self.have_definition(n):
                    return n
        return with_context

    def get_package_names(self) -> typing.List[str]:
        packages = self.get_ownvalue("System`$Packages")
        packages = packages.replace
        assert packages.has_form("System`List", None)
        packages = [c.get_string_value() for c in packages.leaves]
        return packages

        # return sorted({name.split("`")[0] for name in self.get_names()})

    def shorten_name(self, name_with_ctx) -> str:
        if "`" not in name_with_ctx:
            return name_with_ctx

        def in_ctx(name, ctx):
            return name.startswith(ctx) and "`" not in name[len(ctx) :]

        if in_ctx(name_with_ctx, self.get_current_context()):
            return name_with_ctx[len(self.get_current_context()) :]
        for ctx in self.get_context_path():
            if in_ctx(name_with_ctx, ctx):
                return name_with_ctx[len(ctx) :]
        return name_with_ctx

    def have_definition(self, name) -> bool:
        return self.get_definition(name, only_if_exists=True) is not None

    def get_definition(self, name, only_if_exists=False) -> "Definition":
        definition = self.definitions_cache.get(name, None)
        if definition is not None:
            return definition

        original_name = name
        name = self.lookup_name(name)
        user = self.user.get(name, None)
        pymathics = self.pymathics.get(name, None)
        builtin = self.builtin.get(name, None)

        candidates = [user] if user else []
        builtin_instance = None
        if pymathics:
            builtin_instance = pymathics
            candidates.append(pymathics)
        if builtin:
            candidates.append(builtin)
            if builtin_instance is None:
                builtin_instance = builtin

        definition = candidates[0] if len(candidates) == 1 else None
        if len(candidates) > 0 and not definition:
            attributes = (
                user.attributes
                if user
                else (
                    pymathics.attributes
                    if pymathics
                    else (builtin.attributes if builtin else set())
                )
            )
            upvalues = ([],)
            messages = ([],)
            nvalues = ([],)
            defaultvalues = ([],)
            options = {}
            formatvalues = {
                "": [],
            }
            # Merge definitions
            its = [c for c in candidates]
            while its:
                curr = its.pop()
                options.update(curr.options)
                for form, rules in curr.formatvalues.items():
                    if form in formatvalues:
                        formatvalues[form].extend(rules)
                    else:
                        formatvalues[form] = rules
            # Build the new definition
            definition = Definition(
                name=name,
                ownvalues=sum((c.ownvalues for c in candidates), []),
                downvalues=sum((c.downvalues for c in candidates), []),
                subvalues=sum((c.subvalues for c in candidates), []),
                upvalues=sum((c.upvalues for c in candidates), []),
                formatvalues=formatvalues,
                messages=sum((c.messages for c in candidates), []),
                attributes=attributes,
                options=options,
                nvalues=sum((c.nvalues for c in candidates), []),
                defaultvalues=sum((c.defaultvalues for c in candidates), []),
                builtin=builtin_instance,
            )

        if definition is not None:
            self.proxy[strip_context(original_name)].add(original_name)
            self.definitions_cache[original_name] = definition
            self.lookup_cache[original_name] = name
        elif not only_if_exists:
            definition = Definition(name=name)

        return definition

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

    def get_formats(self, name, format=""):
        formats = self.get_definition(name).formatvalues
        result = formats.get(format, []) + formats.get("", [])
        result.sort()
        return result

    def get_nvalues(self, name):
        return self.get_definition(name).nvalues

    def get_defaultvalues(self, name):
        return self.get_definition(name).defaultvalues

    def get_value(self, name, pos, pattern, evaluation):
        assert isinstance(name, str)
        assert "`" in name
        rules = self.get_definition(name).get_values_list(valuesname(pos))
        for rule in rules:
            result = rule.apply(pattern, evaluation)
            if result is not None:
                return result

    def get_user_definition(self, name, create=True) -> typing.Optional["Definition"]:
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
            self.clear_cache(name)
            return self.user[name]

    def mark_changed(self, definition) -> None:
        self.now += 1
        definition.changed = self.now

    def reset_user_definition(self, name) -> None:
        assert not isinstance(name, Symbol)
        fullname = self.lookup_name(name)
        del self.user[fullname]
        self.clear_cache(fullname)
        # TODO fix changed

    def add_user_definition(self, name, definition) -> None:
        assert not isinstance(name, Symbol)
        self.mark_changed(definition)
        fullname = self.lookup_name(name)
        self.user[fullname] = definition
        self.clear_cache(fullname)

    def set_attribute(self, name, attribute) -> None:
        definition = self.get_user_definition(self.lookup_name(name))
        definition.attributes.add(attribute)
        self.mark_changed(definition)
        self.clear_definitions_cache(name)

    def set_attributes(self, name, attributes) -> None:
        definition = self.get_user_definition(self.lookup_name(name))
        definition.attributes = set(attributes)
        self.mark_changed(definition)
        self.clear_definitions_cache(name)

    def clear_attribute(self, name, attribute) -> None:
        definition = self.get_user_definition(self.lookup_name(name))
        if attribute in definition.attributes:
            definition.attributes.remove(attribute)
        self.mark_changed(definition)
        self.clear_definitions_cache(name)

    def add_rule(self, name, rule, position=None):
        definition = self.get_user_definition(self.lookup_name(name))
        if position is None:
            result = definition.add_rule(rule)
        else:
            result = definition.add_rule_at(rule, position)
        self.mark_changed(definition)
        self.clear_definitions_cache(name)
        return result

    def add_format(self, name, rule, form="") -> None:
        definition = self.get_user_definition(self.lookup_name(name))
        if isinstance(form, tuple) or isinstance(form, list):
            forms = form
        else:
            forms = [form]
        for form in forms:
            if form not in definition.formatvalues:
                definition.formatvalues[form] = []
            insert_rule(definition.formatvalues[form], rule)
        self.mark_changed(definition)
        self.clear_definitions_cache(name)

    def add_nvalue(self, name, rule) -> None:
        definition = self.get_user_definition(self.lookup_name(name))
        definition.add_rule_at(rule, "n")
        self.mark_changed(definition)
        self.clear_definitions_cache(name)

    def add_default(self, name, rule) -> None:
        definition = self.get_user_definition(self.lookup_name(name))
        definition.add_rule_at(rule, "default")
        self.mark_changed(definition)
        self.clear_definitions_cache(name)

    def add_message(self, name, rule) -> None:
        definition = self.get_user_definition(self.lookup_name(name))
        definition.add_rule_at(rule, "messages")
        self.mark_changed(definition)
        self.clear_definitions_cache(name)

    def set_values(self, name, values, rules) -> None:
        pos = valuesname(values)
        definition = self.get_user_definition(self.lookup_name(name))
        definition.set_values_list(pos, rules)
        self.mark_changed(definition)
        self.clear_definitions_cache(name)

    def get_options(self, name):
        return self.get_definition(self.lookup_name(name)).options

    def reset_user_definitions(self) -> None:
        self.user = {}
        self.clear_cache()
        # TODO changed

    def get_user_definitions(self):
        return base64.encodebytes(pickle.dumps(self.user, protocol=2)).decode("ascii")

    def set_user_definitions(self, definitions) -> None:
        if definitions:
            self.user = pickle.loads(base64.decodebytes(definitions.encode("ascii")))
        else:
            self.user = {}
        self.clear_cache()

    def get_ownvalue(self, name):
        ownvalues = self.get_definition(self.lookup_name(name)).ownvalues
        if ownvalues:
            return ownvalues[0]
        return None

    def set_ownvalue(self, name, value) -> None:
        from .expression import Symbol
        from .rules import Rule

        name = self.lookup_name(name)
        self.add_rule(name, Rule(Symbol(name), value))
        self.clear_cache(name)

    def set_options(self, name, options) -> None:
        definition = self.get_user_definition(self.lookup_name(name))
        definition.options = options
        self.mark_changed(definition)
        self.clear_definitions_cache(name)

    def unset(self, name, expr):
        definition = self.get_user_definition(self.lookup_name(name))
        result = definition.remove_rule(expr)
        self.mark_changed(definition)
        self.clear_definitions_cache(name)
        return result

    def get_config_value(self, name, default=None):
        "Infinity -> None, otherwise returns integer."
        value = self.get_definition(name).ownvalues
        if value:
            try:
                value = value[0].replace
            except AttributeError:
                return None
            if value.get_name() == "System`Infinity" or value.has_form(
                "DirectedInfinity", 1
            ):
                return None

            return int(value.get_int_value())
        else:
            return default

    def set_config_value(self, name, new_value) -> None:
        from mathics.core.expression import Integer

        self.set_ownvalue(name, Integer(new_value))

    def set_line_no(self, line_no) -> None:
        self.set_config_value("$Line", line_no)

    def get_line_no(self):
        return self.get_config_value("$Line", 0)

    def increment_line_no(self, increment: int = 1) -> None:
        self.set_config_value("$Line", self.get_line_no() + increment)

    def get_history_length(self):
        history_length = self.get_config_value("$HistoryLength", 100)
        if history_length is None or history_length > 100:
            history_length = 100
        return history_length


def get_tag_position(pattern, name) -> typing.Optional[str]:
    if pattern.get_name() == name:
        return "own"
    elif pattern.is_atom():
        return None
    else:
        head_name = pattern.get_head_name()
        if head_name == name:
            return "down"
        elif head_name == "System`Condition" and len(pattern.leaves) > 0:
            return get_tag_position(pattern.leaves[0], name)
        elif pattern.get_lookup_name() == name:
            return "sub"
        else:
            for leaf in pattern.leaves:
                if leaf.get_lookup_name() == name:
                    return "up"
        return None


def insert_rule(values, rule) -> None:
    for index, existing in enumerate(values):
        if existing.pattern.same(rule.pattern):
            del values[index]
            break
    # use insort_left to guarantee that if equal rules exist, newer rules will
    # get higher precedence by being inserted before them. see DownValues[].
    bisect.insort_left(values, rule)


class Definition(object):
    def __init__(
        self,
        name,
        rules=None,
        ownvalues=None,
        downvalues=None,
        subvalues=None,
        upvalues=None,
        formatvalues=None,
        messages=None,
        attributes=(),
        options=None,
        nvalues=None,
        defaultvalues=None,
        builtin=None,
    ) -> None:

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
        self.formatvalues = dict((name, list) for name, list in formatvalues.items())
        self.messages = messages
        self.attributes = set(attributes)
        for a in self.attributes:
            assert "`" in a, "%s attribute %s has no context" % (name, a)
        self.options = options
        self.nvalues = nvalues
        self.defaultvalues = defaultvalues
        self.builtin = builtin

    def get_values_list(self, pos):
        assert pos.isalpha()
        if pos == "messages":
            return self.messages
        else:
            return getattr(self, "%svalues" % pos)

    def set_values_list(self, pos, rules) -> None:
        assert pos.isalpha()
        if pos == "messages":
            self.messages = rules
        else:
            setattr(self, "%svalues" % pos, rules)

    def add_rule_at(self, rule, position) -> bool:
        values = self.get_values_list(position)
        insert_rule(values, rule)
        return True

    def add_rule(self, rule) -> bool:
        pos = get_tag_position(rule.pattern, self.name)
        if pos:
            return self.add_rule_at(rule, pos)
        return False

    def remove_rule(self, lhs) -> bool:
        position = get_tag_position(lhs, self.name)
        if position:
            values = self.get_values_list(position)
            for index, existing in enumerate(values):
                if existing.pattern.expr.same(lhs):
                    del values[index]
                    return True
        return False

    def __repr__(self) -> str:
        s = "<Definition: name: {}, downvalues: {}, formats: {}, attributes: {}>".format(
            self.name, self.downvalues, self.formatvalues, self.attributes
        )
        return s
