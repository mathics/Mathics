#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Natural language functions
"""

# PYTHON MODULES USED IN HERE

# spacy: everything related to parsing natural language
# nltk: functions using WordNet
# pattern: finding inflections (Pluralize[] and WordData[])
# langid, pycountry: LanguageIdentify[]
# pyenchant: SpellingCorrectionList[]

# If a module is not installed, all functions will gracefully alert the user to the missing functionality/packages.
# All modules above (except for "pattern", see below) are available in Python 2 and Python 3 versions.

# ADDING MORE LANGUAGES TO OMW

# In order to use the Extended Open Multilingual Wordnet with NLTK and use even more languages, you need to install
# them manually. Go to http://compling.hss.ntu.edu.sg/omw/summx.html, download the data, and then create a new folder
# under $HOME/nltk_data/corpora/omw/your_language where you put the file from wiki/wn-wikt-your_language.tab, and
# rename it to wn-data-your_language.tab.

# PATTERN MODULE

# This module uses "pattern" for various tasks (http://www.clips.ua.ac.be/pattern). Pattern currently only supports
# Python 2, but there exists for Python 3 an official, though preliminary version: https://github.com/pattern3/pattern,
# which is not in pip, but can be installed manually. on OS X make sure to "pip install Pillow lxml" and after
# installing, uncompress pattern.egg into folder. you may also need to run "export STATIC_DEPS=true" before you run
# setup.py

# PYENCHANT MODULE

# "pyenchant" OS X python 3 fix: patch enchant/_enchant.py:
# prefix_dir.contents = c_char_p(e_dir.encode('utf8'))

# PORTABLE INSTALLATION

# for nltk, use the environment variable NLTK_DATA to specify a custom data path (instead of $HOME/.nltk).
# for spacy, use SPACY_DATA; the latter is a custom Mathics variable.

from mathics.builtin.base import Builtin, MessageException
from mathics.builtin.randomnumbers import RandomEnv
from mathics.builtin.codetables import iso639_3
from mathics.builtin.strings import to_regex, anchor_pattern
from mathics.core.expression import Expression, String, Integer, Real, Symbol, strip_context, string_list

import os
import re
import itertools
from itertools import chain
import heapq
import math


def _parse_nltk_lookup_error(e):
    m = re.search("Resource '([^']+)' not found\.", str(e))
    if m:
        return m.group(1)
    else:
        return 'unknown'


def _make_forms():
    forms = {
        'Word': lambda doc: (token for token in doc),
        'Sentence': lambda doc: (sent for sent in doc.sents),
        'Paragraph': lambda doc: _fragments(doc, re.compile(r"^[\n][\n]+$")),
        'Line': lambda doc: _fragments(doc, re.compile(r"^[\n]$")),

        'URL': lambda doc: (token for token in doc if token.orth_.like_url()),
        'EmailAddress': lambda doc: (token for token in doc if token.orth_.like_email()),
    }

    def filter_named_entity(label):
        def generator(doc):
            for ent in doc.ents:
                if ent.label == label:
                    yield ent

        return generator

    def filter_pos(pos):
        def generator(doc):
            for token in doc:
                if token.pos == pos:
                    yield token

        return generator

    for name, symbol in _symbols.items():
        forms[name] = filter_named_entity(symbol)

    for tag, names in _pos_tags.items():
        name, phrase_name = names
        forms[name] = filter_pos(tag)

    return forms


# the following two may only be accessed after_WordNetBuiltin._load_wordnet has
# been called.

_wordnet_pos_to_type = {}
_wordnet_type_to_pos = {}

try:
    import nltk

    def _init_nltk_maps():
        _wordnet_pos_to_type.update({
            nltk.corpus.wordnet.VERB: 'Verb',
            nltk.corpus.wordnet.NOUN: 'Noun',
            nltk.corpus.wordnet.ADJ: 'Adjective',
            nltk.corpus.wordnet.ADJ_SAT: 'Adjective',
            nltk.corpus.wordnet.ADV: 'Adverb',
        })
        _wordnet_type_to_pos.update({
            'Verb': [nltk.corpus.wordnet.VERB],
            'Noun': [nltk.corpus.wordnet.NOUN],
            'Adjective': [nltk.corpus.wordnet.ADJ, nltk.corpus.wordnet.ADJ_SAT],
            'Adverb': [nltk.corpus.wordnet.ADV],
        })
except ImportError:
    pass

try:
    import spacy
    from spacy.tokens import Span

    # Part of speech tags and their public interface names in Mathics
    # see http://www.mathcs.emory.edu/~choi/doc/clear-dependency-2012.pdf
    _pos_tags = {
        spacy.parts_of_speech.ADJ: ('Adjective', ''),
        spacy.parts_of_speech.ADP: ('Preposition', 'Prepositional Phrase'),
        spacy.parts_of_speech.ADV: ('Adverb', ''),
        spacy.parts_of_speech.CONJ: ('Conjunct', ''),
        spacy.parts_of_speech.DET: ('Determiner', ''),
        spacy.parts_of_speech.INTJ: ('Interjection', ''),
        spacy.parts_of_speech.NOUN: ('Noun', 'Noun Phrase'),
        spacy.parts_of_speech.NUM: ('Number', ''),
        spacy.parts_of_speech.PART: ('Particle', ''),
        spacy.parts_of_speech.PRON: ('Pronoun', ''),
        spacy.parts_of_speech.PROPN: ('Proposition', ''),
        spacy.parts_of_speech.PUNCT: ('Punctuation', ''),
        spacy.parts_of_speech.SCONJ: ('Sconj', ''),
        spacy.parts_of_speech.SYM: ('Symbol', ''),
        spacy.parts_of_speech.VERB: ('Verb', 'Verb Phrase'),
        spacy.parts_of_speech.X: ('X', ''),
        spacy.parts_of_speech.EOL: ('EOL', ''),
        spacy.parts_of_speech.SPACE: ('Space', ''),
    }

    # Mathics named entitiy names and their corresponding constants in spacy.
    _symbols = {
        'Person': spacy.symbols.PERSON,
        'Company': spacy.symbols.ORG,
        'Quantity': spacy.symbols.QUANTITY,
        'Number': spacy.symbols.CARDINAL,
        'CurrencyAmount': spacy.symbols.MONEY,
        'Country': spacy.symbols.GPE,  # also includes cities and states
        'City': spacy.symbols.GPE,  # also includes countries and states
    }

    # forms are everything one can use in TextCases[] or TextPosition[].
    _forms = _make_forms()
except ImportError:
    _pos_tags = {}
    _symbols = {}
    _forms = {}


def _merge_dictionaries(a, b):
    c = a.copy()
    c.update(b)
    return c


def _position(t):
    if isinstance(t, Span):
        l = t.doc[t.start]
        r = t.doc[t.end - 1]
        return 1 + l.idx, r.idx + len(r.text)
    else:
        return 1 + t.idx, t.idx + len(t.text)


def _fragments(doc, sep):
    start = 0
    for i, token in enumerate(doc):
        if sep.match(token.text):
            yield Span(doc, start, i)
            start = i + 1
    end = len(doc)
    if start < end:
        yield Span(doc, start, end)


class _SpacyBuiltin(Builtin):
    requires = (
        'spacy',
    )

    options = {
        'Language': '"English"',
    }

    messages = {
        'runtime': 'Spacy gave the following error: ``',
        'lang': 'Language "`1`" is currently not supported with `2`[].',
    }

    _language_codes = {
        'English': 'en',
        'German': 'de',
    }

    _spacy_instances = {}

    def _load_spacy(self, evaluation, options):
        language_code = None
        language_name = self.get_option(options, 'Language', evaluation)
        if language_name is None:
            language_name = String('Undefined')
        if isinstance(language_name, String):
            language_code = _SpacyBuiltin._language_codes.get(language_name.get_string_value())
        if not language_code:
            evaluation.message(self.get_name(), 'lang', language_name, strip_context(self.get_name()))
            return None

        instance = _SpacyBuiltin._spacy_instances.get(language_code)
        if instance:
            return instance

        try:
            if 'SPACY_DATA' in os.environ:
                instance = spacy.load(language_code, via=os.environ['SPACY_DATA'])
            else:
                instance = spacy.load(language_code)

            _SpacyBuiltin._spacy_instances[language_code] = instance
            return instance
        except RuntimeError as e:
            evaluation.message(self.get_name(), 'runtime', str(e))
            return None

    def _nlp(self, text, evaluation, options):
        nlp = self._load_spacy(evaluation, options)
        if not nlp:
            return None
        return nlp(text)

    def _is_stop_lambda(self, evaluation, options):
        nlp = self._load_spacy(evaluation, options)
        if not nlp:
            return None

        vocab = nlp.vocab

        def is_stop(word):
            return vocab[word].is_stop

        return is_stop


class WordFrequencyData(_SpacyBuiltin):
    """
    <dl>
    <dt>'WordFrequencyData[$word$]'
      <dd>returns the frequency of $word$ in common English texts.
    </dl>
    """

    # Mathematica uses the gargantuan Google n-gram corpus, see
    # http://commondatastorage.googleapis.com/books/syntactic-ngrams/index.html

    def apply(self, word, evaluation, options):
        'WordFrequencyData[word_String,  OptionsPattern[%(name)s]]'
        doc = self._nlp(word.get_string_value(), evaluation, options)
        frequency = 0.
        if doc:
            if len(doc) == 1:
                frequency = math.exp(doc[0].prob)  # convert log probability
        return Real(frequency)


class WordCount(_SpacyBuiltin):
    """
    <dl>
    <dt>'WordCount[$string$]'
      <dd>returns the number of words in $string$.
    </dl>

    >> WordCount["A long time ago"]
     = 4
    """

    def apply(self, text, evaluation, options):
        'WordCount[text_String, OptionsPattern[%(name)s]]'
        doc = self._nlp(text.get_string_value(), evaluation, options)
        if doc:
            punctuation = spacy.parts_of_speech.PUNCT
            return Integer(sum(1 for word in doc if word.pos != punctuation))


class TextWords(_SpacyBuiltin):
    """
    <dl>
    <dt>'TextWords[$string$]'
      <dd>returns the words in $string$.
    <dt>'TextWords[$string$, $n$]'
      <dd>returns the first $n$ words in $string$
    </dl>

    >> TextWords["Hickory, dickory, dock! The mouse ran up the clock."]
     = {Hickory, dickory, dock, The, mouse, ran, up, the, clock}
    """

    def apply(self, text, evaluation, options):
        'TextWords[text_String, OptionsPattern[%(name)s]]'
        doc = self._nlp(text.get_string_value(), evaluation, options)
        if doc:
            punctuation = spacy.parts_of_speech.PUNCT
            return string_list('List', [String(word.text) for word in doc if word.pos != punctuation], evaluation)

    def apply_n(self, text, n, evaluation, options):
        'TextWords[text_String, n_Integer, OptionsPattern[%(name)s]]'
        doc = self._nlp(text.get_string_value(), evaluation, options)
        if doc:
            punctuation = spacy.parts_of_speech.PUNCT
            return string_list('List', itertools.islice(
                (String(word.text) for word in doc if word.pos != punctuation), n.get_int_value()), evaluation)


class TextSentences(_SpacyBuiltin):
    """
    <dl>
    <dt>'TextSentences[$string$]'
      <dd>returns the sentences in $string$.
    <dt>'TextSentences[$string$, $n$]'
      <dd>returns the first $n$ sentences in $string$
    </dl>

    >> TextSentences["Night and day. Day and night."]
     = {Night and day., Day and night.}

    >> TextSentences["Night and day. Day and night.", 1]
     = {Night and day.}

    >> TextSentences["Mr. Jones met Mrs. Jones."]
     = {Mr. Jones met Mrs. Jones.}
    """

    def apply(self, text, evaluation, options):
        'TextSentences[text_String, OptionsPattern[%(name)s]]'
        doc = self._nlp(text.get_string_value(), evaluation, options)
        if doc:
            return string_list('List', [String(sent.text) for sent in doc.sents], evaluation)

    def apply_n(self, text, n, evaluation, options):
        'TextSentences[text_String, n_Integer, OptionsPattern[%(name)s]]'
        doc = self._nlp(text.get_string_value(), evaluation, options)
        if doc:
            return string_list('List', itertools.islice(
                (String(sent.text) for sent in doc.sents), n.get_int_value()), evaluation)


class DeleteStopwords(_SpacyBuiltin):
    """
    <dl>
    <dt>'DeleteStopwords[$list$]'
      <dd>returns the words in $list$ without stopwords.
    <dt>'DeleteStopwords[$string$]'
      <dd>returns $string$ without stopwords.
    </dl>

    >> DeleteStopwords[{"Somewhere", "over", "the", "rainbow"}]
     = {rainbow}

    >> DeleteStopwords["There was an Old Man of Apulia, whose conduct was very peculiar"]
     = Old Man Apulia, conduct peculiar
    """

    def apply_list(self, l, evaluation, options):
        'DeleteStopwords[l_List, OptionsPattern[%(name)s]]'
        is_stop = self._is_stop_lambda(evaluation, options)

        def filter_words(words):
            for w in words:
                s = w.get_string_value()
                if not s:
                    yield String(s)
                elif not is_stop(s):
                    yield String(s)
        return string_list('List', filter_words(l.leaves), evaluation)

    def apply_string(self, s, evaluation, options):
        'DeleteStopwords[s_String, OptionsPattern[%(name)s]]'
        doc = self._nlp(s.get_string_value(), evaluation, options)
        if doc:
            is_stop = self._is_stop_lambda(evaluation, options)
            if is_stop:
                def tokens():
                    for token in doc:
                        if not is_stop(token.text):
                            yield token.text_with_ws
                        else:
                            yield token.whitespace_.strip()
                return String(''.join(tokens()))


class WordFrequency(_SpacyBuiltin):
    """
    <dl>
    <dt>'WordFrequency[$text$, $word$]'
      <dd>returns the relative frequency of $word$ in $text$.
    </dl>

    $word$ may also specify multiple words using $a$ | $b$ | ...

    >> WordFrequency[Import["ExampleData/EinsteinSzilLetter.txt"], "a" | "the"]
     = 0.0667702

    >> WordFrequency["Apple Tree", "apple", IgnoreCase -> True]
     = 0.5
    """

    options = _SpacyBuiltin.options
    options.update({
        'IgnoreCase': 'False'
    })

    def apply(self, text, word, evaluation, options):
        'WordFrequency[text_String, word_, OptionsPattern[%(name)s]]'
        doc = self._nlp(text.get_string_value(), evaluation, options)
        if not doc:
            return
        if isinstance(word, String):
            words = set((word.get_string_value(),))
        elif word.get_head_name() == 'System`Alternatives':
            if not all(isinstance(a, String) for a in word.leaves):
                return  # error
            words = set(a.get_string_value() for a in word.leaves)
        else:
            return  # error
        ignore_case = self.get_option(
            options, 'IgnoreCase', evaluation).is_true()
        if ignore_case:
            words = [w.lower() for w in words]
        n = 0
        for token in doc:
            text = token.text
            if ignore_case:
                text = text.lower()
            if text in words:
                n += 1
        return Expression('N', Expression('Divide', n, len(doc)))


class Containing(Builtin):
    pass


def _cases(doc, form):
    if isinstance(form, String):
        generators = [_forms.get(form.get_string_value())]
    elif form.get_head_name() == 'System`Alternatives':
        if not all(isinstance(f, String) for f in form.leaves):
            return  # error
        generators = [_forms.get(f.get_string_value()) for f in form.leaves]
    elif form.get_head_name() == 'System`Containing':
        if len(form.leaves) == 2:
            for t in _containing(doc, *form.leaves):
                yield t
            return
        else:
            return  # error
    else:
        return  # error

    def try_next(iterator):
        try:
            return next(iterator)
        except StopIteration:
            return None

    feeds = []
    for i, iterator in enumerate([iter(generator(doc)) for generator in generators]):
        t = try_next(iterator)
        if t:
            feeds.append((_position(t), i, t, iterator))
    heapq.heapify(feeds)
    while feeds:
        pos, i, token, iterator = heapq.heappop(feeds)
        yield token
        t = try_next(iterator)
        if t:
            heapq.heappush(feeds, (_position(t), i, t, iterator))


def _containing(doc, outer, inner):
    if not isinstance(outer, String):
        return  # error
    outer_generator = _forms.get(outer.get_string_value())
    inner_iter = _cases(doc, inner)
    inner_start = None
    produce_t = False
    try:
        for t in outer_generator(doc):
            start, end = _position(t)
            if inner_start is not None and inner_start < end:
                produce_t = True
            if produce_t:
                yield t
                produce_t = False
            while True:
                inner_start, inner_end = _position(next(inner_iter))
                if inner_end > start:
                    break
            if inner_start < end:
                produce_t = True
    except StopIteration:
        pass


class TextCases(_SpacyBuiltin):
    """
    <dl>
    <dt>'TextCases[$text$, $form$]'
      <dd>returns all elements of type $form$ in $text$ in order of their appearance.
    </dl>

    >> TextCases["I was in London last year.", "Pronoun"]
     = {I}

    >> TextCases["I was in London last year.", "City"]
     = {London}

    >> TextCases[Import["ExampleData/EinsteinSzilLetter.txt"], "Person", 3]
     = {Albert Einstein, E. Fermi, L. Szilard}
    """

    def apply(self, text, form, evaluation, options):
        'TextCases[text_String, form_,  OptionsPattern[%(name)s]]'
        doc = self._nlp(text.get_string_value(), evaluation, options)
        if doc:
            return Expression('List', *[t.text for t in _cases(doc, form)])

    def apply_n(self, text, form, n, evaluation, options):
        'TextCases[text_String, form_, n_Integer,  OptionsPattern[%(name)s]]'
        doc = self._nlp(text.get_string_value(), evaluation, options)
        if doc:
            return Expression('List', *itertools.islice(
                (t.text for t in _cases(doc, form)), n.get_int_value()))


class TextPosition(_SpacyBuiltin):
    """
    <dl>
    <dt>'TextPosition[$text$, $form$]'
      <dd>returns the positions of elements of type $form$ in $text$ in order of their appearance.
    </dl>

    >> TextPosition["Liverpool and Manchester are two English cities.", "City"]
     = {{1, 9}, {15, 24}}
    """

    def apply(self, text, form, evaluation, options):
        'TextPosition[text_String, form_,  OptionsPattern[%(name)s]]'
        doc = self._nlp(text.get_string_value(), evaluation, options)
        if doc:
            return Expression('List', *[_position(t) for t in _cases(doc, form)])

    def apply_n(self, text, form, n, evaluation, options):
        'TextPosition[text_String, form_, n_Integer,  OptionsPattern[%(name)s]]'
        doc = self._nlp(text.get_string_value(), evaluation, options)
        if doc:
            return Expression('List', *itertools.islice(
                (_position(t) for t in _cases(doc, form)), n.get_int_value()))


class TextStructure(_SpacyBuiltin):
    """
    <dl>
    <dt>'TextStructure[$text$, $form$]'
      <dd>returns the grammatical structure of $text$ as $form$.
    </dl>

    >> TextStructure["The cat sat on the mat.", "ConstituentString"]
     = {(Sentence, ((Verb Phrase, (Noun Phrase, (Determiner, The), (Noun, cat)), (Verb, sat), (Prepositional Phrase, (Preposition, on), (Noun Phrase, (Determiner, the), (Noun, mat))), (Punctuation, .))))}
    """

    _root_pos = set(i for i, names in _pos_tags.items() if names[1])

    def _to_constituent_string(self, node):
        token, children = node
        name, phrase_name = _pos_tags.get(token.pos, ('Unknown', 'Unknown Phrase'))
        if not children:
            return '(%s, %s)' % (name, token.text)
        else:
            sub = ', '.join(self._to_constituent_string(next_node) for next_node in children)
            return '(%s, %s)' % (phrase_name, sub)

    def _to_tree(self, tokens, path=[]):
        roots = []
        i = 0
        while i < len(tokens):
            token = tokens[i]

            if token in path:
                roots.append((token, None))
                i += 1
            else:
                root = token
                while root.head != root and root.head not in path:
                    root = root.head

                sub = list(root.subtree)

                if root.pos not in self._root_pos:
                    roots.extend(self._to_tree(sub, path + [root]))
                else:
                    roots.append((root, self._to_tree(sub, path + [root])))

                i += len(sub)

        return roots

    def apply(self, text, evaluation, options):
        'TextStructure[text_String, "ConstituentString",  OptionsPattern[%(name)s]]'
        doc = self._nlp(text.get_string_value(), evaluation, options)
        if doc:
            tree = self._to_tree(list(doc))
            sents = ['(Sentence, (%s))' % self._to_constituent_string(x) for x in tree]
            return Expression('List', *[String(s) for s in sents])


class WordSimilarity(_SpacyBuiltin):
    """
    <dl>
    <dt>'WordSimilarity[$text1$, $text2]'
      <dd>returns a real-valued measure of semantic similarity of two texts or words.
    <dt>'WordSimilarity[{$text1$, $i1}, {$text2, $j1$}]'
      <dd>returns a measure of similarity of two words within two texts.
    <dt>'WordSimilarity[{$text1$, {$i1, $i2, ...}}, {$text2, {$j1$, $j2$, ...}}]'
      <dd>returns a measure of similarity of multiple words within two texts.
    </dl>

    >> NumberForm[WordSimilarity["car", "train"], 3]
     = 0.5

    >> NumberForm[WordSimilarity["car", "hedgehog"], 3]
     = 0.368

    >> NumberForm[WordSimilarity[{"An ocean full of water.", {2, 2}}, { "A desert full of sand.", {2, 5}}], 3]
     = {0.253, 0.177}
    """

    messages = _merge_dictionaries(_SpacyBuiltin.messages, {
        'txtidx': 'Index `1` in position `2` must be between 1 and `3`.',
        'idxfmt': 'Indices must be integers or lists of integers of the same length.',
    })

    def apply(self, text1, text2, evaluation, options):
        'WordSimilarity[text1_String, text2_String, OptionsPattern[%(name)s]]'
        doc1 = self._nlp(text1.get_string_value(), evaluation, options)
        if doc1:
            doc2 = self._nlp(text2.get_string_value(), evaluation, options)
            if doc2:
                return Real(doc1.similarity(doc2))

    def apply_pair(self, text1, i1, text2, i2, evaluation, options):
        'WordSimilarity[{text1_String, i1_}, {text2_String, i2_}, OptionsPattern[%(name)s]]'
        doc1 = self._nlp(text1.get_string_value(), evaluation, options)
        if doc1:
            if text2.get_string_value() == text1.get_string_value():
                doc2 = doc1
            else:
                doc2 = self._nlp(text2.get_string_value(), evaluation, options)
            if doc2:
                if i1.get_head_name() == 'System`List' and i2.get_head_name() == 'System`List':
                    if len(i1.leaves) != len(i2.leaves):
                        evaluation.message('TextSimilarity', 'idxfmt')
                        return
                    if any(not all(isinstance(i, Integer) for i in l.leaves) for l in (i1, i2)):
                        evaluation.message('TextSimilarity', 'idxfmt')
                        return
                    indices1 = [i.get_int_value() for i in i1.leaves]
                    indices2 = [i.get_int_value() for i in i2.leaves]
                    multiple = True
                elif isinstance(i1, Integer) and isinstance(i2, Integer):
                    indices1 = [i1.get_int_value()]
                    indices2 = [i2.get_int_value()]
                    multiple = False
                else:
                    evaluation.message('TextSimilarity', 'idxfmt')
                    return

                for index1, index2 in zip(indices1, indices2):
                    for i, pos, doc in zip((index1, index2), (1, 2), (doc1, doc2)):
                        if i < 1 or i > len(doc):
                            evaluation.message('TextSimilarity', 'txtidx', i, pos, len(doc))
                            return

                result = [Real(doc1[j1 - 1].similarity(doc2[j2 - 1])) for j1, j2 in zip(indices1, indices2)]

                if multiple:
                    return Expression('List', *result)
                else:
                    return result[0]


class WordStem(Builtin):
    """
    <dl>
    <dt>'WordStem[$text$]'
      <dd>returns a stemmed form of $word$, thereby reducing an inflected form to its root.
    </dl>

    >> WordStem["towers"]
     = tower

    >> WordStem[{"towers", "knights", "queens"}]
     = {tower, knight, queen}
    """

    requires = (
        'nltk',
    )

    _stemmer = None

    @staticmethod
    def _get_porter_stemmer():
        if WordStem._stemmer is None:
            WordStem._stemmer = nltk.stem.porter.PorterStemmer()
        return WordStem._stemmer

    @staticmethod
    def porter(w):
        return WordStem._get_porter_stemmer().stem(w)

    def apply(self, word, evaluation):
        'WordStem[word_String]'
        stemmer = self._get_porter_stemmer()
        return String(stemmer.stem(word.get_string_value()))

    def apply_list(self, words, evaluation):
        'WordStem[words_List]'
        if all(isinstance(w, String) for w in words.leaves):
            stemmer = self._get_porter_stemmer()
            return Expression('List', *[String(
                stemmer.stem(w.get_string_value())) for w in words.leaves])


class _WordNetBuiltin(Builtin):
    requires = (
        'nltk',
    )

    options = {
        'Language': '"English"',
    }

    messages = {
        'package': "NLTK's `` corpus is not installed. Please install it using nltk.download().",
        'lang': 'Language "`1`" is currently not supported with `2`[]. Please install it manually.',
        # 'load': 'Loading `1` word data. Please wait.',
        'wordnet': 'WordNet returned the following error: ``',
    }

    _wordnet_instances = {}

    def _language_name(self, evaluation, options):
        return self.get_option(options, 'Language', evaluation)

    def _init_wordnet(self, evaluation, language_name, language_code):
        try:
            wordnet_resource = nltk.data.find('corpora/wordnet')
            _init_nltk_maps()
        except LookupError:
            evaluation.message(self.get_name(), 'package', 'wordnet')
            return None

        try:
            omw = nltk.corpus.util.LazyCorpusLoader(
                'omw', nltk.corpus.reader.CorpusReader, r'.*/wn-data-.*\.tab', encoding='utf8')
        except LookupError:
            evaluation.message(self.get_name(), 'package', 'omw')
            return None

        wordnet = nltk.corpus.reader.wordnet.WordNetCorpusReader(wordnet_resource, omw)

        if language_code not in wordnet.langs():
            evaluation.message(self.get_name(), 'lang', language_name, strip_context(self.get_name()))
            return None

        return wordnet

    def _load_wordnet(self, evaluation, language_name):
        language_code = None
        if isinstance(language_name, String):
            language_code = iso639_3.get(language_name.get_string_value())
        if not language_code:
            evaluation.message(self.get_name(), 'lang', language_name, strip_context(self.get_name()))
            return None, None

        wordnet = _WordNetBuiltin._wordnet_instances.get(language_code)
        if not wordnet:
            try:
                wordnet = self._init_wordnet(evaluation, language_name, language_code)
            except LookupError as e:
                evaluation.message(self.get_name(), 'package', _parse_nltk_lookup_error(e))
                return None, None

            _WordNetBuiltin._wordnet_instances[language_code] = wordnet

        return wordnet, language_code

    @staticmethod
    def _decode_synset(syn):
        what, pos, nr = (syn.name().split('.') + ['01'])[:3]
        return what.replace('_', ' '), pos, nr

    @staticmethod
    def _capitalize(s):
        return re.sub(r'^[a-z]|\s[a-z]', lambda m: m.group(0).upper().lstrip(' '), s)

    @staticmethod
    def _underscore(s):
        return re.sub(r'[a-z][A-Z]', lambda m: m.group(0)[0] + '_' + m.group(0)[1].lower(), s).lower()

    @staticmethod
    def _list_syn_form(syn):
        what, pos, nr = _WordNetBuiltin._decode_synset(syn)

        def containers():
            for name in syn.lemma_names():
                if name != what:
                    yield name

            for s in chain(syn.hypernyms(), syn.hyponyms(), syn.similar_tos()):
                container, _, _ = _WordNetBuiltin._decode_synset(s)
                yield container

            for lemma in WordProperty._synonymous_lemmas(syn):
                yield lemma.name()

        return what, _wordnet_pos_to_type[pos], containers

    @staticmethod
    def syn(syn, wordnet, language_code):
        what, pos, nr = _WordNetBuiltin._decode_synset(syn)
        for s, form in _WordNetBuiltin._iterate_senses(what, wordnet, language_code):
            if s == syn:
                return form
        return what, pos, 'Unknown'

    @staticmethod
    def _iterate_senses(word, wordnet, language_code):
        if not word:
            return

        used = set()
        output_word = word.replace('_', ' ')

        for syn in wordnet.synsets(word, None, language_code):
            if syn.lexname() in ('noun.location', 'noun.person'):
                continue  # ignore

            what, pos, containers = _WordNetBuiltin._list_syn_form(syn)

            for container in containers():
                container = container.replace('_', ' ')
                if container != word:
                    if container not in used:
                        used.add(container)
                        yield syn, (output_word, pos, _WordNetBuiltin._capitalize(container))
                        break

    def _senses(self, word, wordnet, language_code):
        if isinstance(word, tuple):  # find forms like ["tree", "Noun", "WoodyPlant"]
            for syn, form in _WordNetBuiltin._iterate_senses(word[0], wordnet, language_code):
                if form == word:
                    return [[syn, form]]
        else:  # find word given as strings, e.g. "tree"
            word = wordnet.morphy(word)  # base form, e.g. trees -> tree
            return list(_WordNetBuiltin._iterate_senses(word, wordnet, language_code))


class WordDefinition(_WordNetBuiltin):
    """
    <dl>
    <dt>'WordDefinition[$word$]'
      <dd>returns a definition of $word$ or Missing["Available"] if $word$ is not known.
    </dl>

    >> WordDefinition["gram"]
     = {a metric unit of weight equal to one thousandth of a kilogram}
    """

    def apply(self, word, evaluation, options):
        'WordDefinition[word_String, OptionsPattern[%(name)s]]'
        wordnet, language_code = self._load_wordnet(evaluation, self._language_name(evaluation, options))
        if wordnet:
            senses = self._senses(word.get_string_value().lower(), wordnet, language_code)
            if senses:
                return Expression('List', *[String(syn.definition()) for syn, _ in senses])
            else:
                return Expression('Missing', 'NotAvailable')


class WordProperty:
    def __init__(self, syn_form, wordnet, language_code):
        self.syn_form = syn_form
        self.wordnet = wordnet
        self.language_code = language_code

    def syn(self, syn):
        return self.syn_form(_WordNetBuiltin.syn(syn, self.wordnet, self.language_code))

    @staticmethod
    def _synonymous_lemmas(syn):
        first_lemma = syn.name().split('.')[0]
        return (s for s in syn.lemmas() if s.name() != first_lemma)

    @staticmethod
    def _antonymous_lemmas(syn):
        return (s for lemma in syn.lemmas() for s in lemma.antonyms())

    def definitions(self, syn, desc):
        return syn.definition()

    def examples(self, syn, desc):
        return syn.examples()

    def synonyms(self, syn, desc):
        _, pos, container = desc
        return [self.syn_form((l.name().replace('_', ' '), pos, container))
                for l in WordProperty._synonymous_lemmas(syn)]

    def antonyms(self, syn, desc):
        return [self.syn(l.synset()) for l in WordProperty._antonymous_lemmas(syn)]

    def broader_terms(self, syn, desc):
        return [self.syn(s) for s in syn.hypernyms()]

    def narrower_terms(self, syn, desc):
        return [self.syn(s) for s in syn.hyponyms()]

    def usage_field(self, syn, desc):
        return syn.usage_domains()

    def whole_terms(self, syn, desc):
        return [self.syn(s) for s in syn.part_holonyms()]

    def part_terms(self, syn, desc):
        return [self.syn(s) for s in syn.part_meronyms()]

    def material_terms(self, syn, desc):
        return [self.syn(s) for s in syn.substance_meronyms()]

    def word_net_id(self, syn, desc):
        return syn.offset()

    def entailed_terms(self, syn, desc):  # e.g. fall to condense
        return [self.syn(s) for s in syn.entailments()]

    def causes_terms(self, syn, desc):  # e.g. ignite to burn
        return [self.syn(s) for s in syn.causes()]

    def inflected_forms(self, syn, desc):
        try:
            word, pos, _ = desc
            if pos == 'Verb':
                from pattern.en import lexeme
                return [w for w in reversed(lexeme(word)) if w != word]
            elif pos == 'Noun':
                from pattern.en import pluralize
                return [pluralize(word)]
            elif pos == 'Adjective':
                from pattern.en import comparative, superlative
                return [comparative(word), superlative(word)]
            else:
                return []
        except ImportError:
            raise MessageException('General', 'unavailable', 'WordData[_, "InflectedForms"]', 'pattern')


class _WordListBuiltin(_WordNetBuiltin):
    _dictionary = {}

    def _words(self, language_name, type, evaluation):
        wordnet, language_code = self._load_wordnet(evaluation, language_name)

        if not wordnet:
            return

        key = '%s.%s' % (language_code, type)
        words = self._dictionary.get(key)
        if not words:
            try:
                if type == 'All':
                    filtered_pos = [None]
                else:
                    filtered_pos = _wordnet_type_to_pos[type]
                words = []
                for pos in filtered_pos:
                    words.extend(list(wordnet.all_lemma_names(pos, language_code)))
                words.sort()
                self._dictionary[key] = words
            except nltk.corpus.reader.wordnet.WordNetError as err:
                evaluation.message(self.get_name(), 'wordnet', str(err))
                return

        return words


class WordData(_WordListBuiltin):
    """
    <dl>
    <dt>'WordData[$word$]'
      <dd>returns a list of possible senses of a word.
    <dt>'WordData[$word$, $property$]'
      <dd>returns detailed information about a word regarding $property$, e.g. "Definitions" or "Examples".
    </dl>

    The following are valid properties:
    - Definitions, Examples
    - InflectedForms
    - Synonyms, Antonyms
    - BroaderTerms, NarrowerTerms
    - WholeTerms, PartTerms, MaterialTerms
    - EntailedTerms, CausesTerms
    - UsageField
    - WordNetID
    - Lookup

    >> WordData["riverside", "Definitions"]
    = {{riverside, Noun, Bank} -> the bank of a river}

    >> WordData[{"fish", "Verb", "Angle"}, "Examples"]
    = {{fish, Verb, Angle} -> {fish for compliments}}
    """

    messages = _merge_dictionaries(_WordNetBuiltin.messages, {
        'notprop': 'WordData[] does not recognize `1` as a valid property.',
    })

    def _parse_word(self, word):
        if isinstance(word, String):
            return word.get_string_value().lower()
        elif word.get_head_name() == 'System`List':
            if len(word.leaves) == 3 and all(isinstance(s, String) for s in word.leaves):
                return tuple(s.get_string_value() for s in word.leaves)

    def _standard_property(self, py_word, py_form, py_property, wordnet, language_code, evaluation):
        senses = self._senses(py_word, wordnet, language_code)
        if not senses:
            return Expression('Missing', 'NotAvailable')
        elif py_form == 'List':
            word_property = WordProperty(self._short_syn_form, wordnet, language_code)
            property_getter = getattr(word_property, '%s' % self._underscore(py_property), None)
            if property_getter:
                return Expression('List', *[property_getter(syn, desc) for syn, desc in senses])
        elif py_form in ('Rules', 'ShortRules'):
            syn_form = (lambda s: s) if py_form == 'Rules' else (lambda s: s[0])
            word_property = WordProperty(syn_form, wordnet, language_code)
            property_getter = getattr(word_property, '%s' % self._underscore(py_property), None)
            if property_getter:
                return Expression('List', *[Expression(
                    'Rule', desc, property_getter(syn, desc)) for syn, desc in senses])
        evaluation.message(self.get_name(), 'notprop', property)

    def _parts_of_speech(self, py_word, wordnet, language_code):
        parts = set(syn.pos() for syn, _ in self._senses(py_word, wordnet, language_code))
        if not parts:
            return Expression('Missing', 'NotAvailable')
        else:
            return Expression('List', *[String(s) for s in sorted([_wordnet_pos_to_type[p] for p in parts])])

    def _property(self, word, py_property, py_form, evaluation, options):
        if py_property == 'PorterStem':
            if isinstance(word, String):
                return String(WordStem.porter(word.get_string_value()))
            else:
                return

        wordnet, language_code = self._load_wordnet(evaluation, self._language_name(evaluation, options))
        if not wordnet:
            return

        py_word = self._parse_word(word)
        if not py_word:
            return

        if py_property == 'PartsOfSpeech':
            return self._parts_of_speech(py_word, wordnet, language_code)

        try:
            return self._standard_property(py_word, py_form, py_property, wordnet, language_code, evaluation)
        except MessageException as e:
            e.message(evaluation)

    def apply(self, word, evaluation, options):
        'WordData[word_, OptionsPattern[%(name)s]]'
        if word.get_head_name() == 'System`StringExpression':
            return Expression('DictionaryLookup', word)
        elif isinstance(word, String) or word.get_head_name() == 'System`List':
            pass
        else:
            return

        wordnet, language_code = self._load_wordnet(evaluation, self._language_name(evaluation, options))
        if not wordnet:
            return

        py_word = self._parse_word(word)
        if not py_word:
            return

        senses = self._senses(py_word, wordnet, language_code)
        return Expression('List', *[
            [String(s) for s in desc] for syn, desc in senses])

    def apply_property(self, word, property, evaluation, options):
        'WordData[word_, property_String, OptionsPattern[%(name)s]]'
        if word.get_head_name() == 'System`StringExpression':
            if property.get_string_value() == 'Lookup':
                return Expression('DictionaryLookup', word)
        elif isinstance(word, String) or word.get_head_name() == 'System`List':
            return self._property(word, property.get_string_value(), "ShortRules", evaluation, options)

    def apply_property_form(self, word, property, form, evaluation, options):
        'WordData[word_, property_String, form_String, OptionsPattern[%(name)s]]'
        if isinstance(word, String) or word.get_head_name() == 'System`List':
            return self._property(word, property.get_string_value(), form.get_string_value(), evaluation, options)


class DictionaryWordQ(_WordNetBuiltin):
    """
    <dl>
    <dt>'DictionaryWordQ[$word$]'
      <dd>returns True if $word$ is a word usually found in dictionaries, and False otherwise.
    </dl>

    >> DictionaryWordQ["couch"]
     = True

    >> DictionaryWordQ["meep-meep"]
     = False
    """

    def apply(self, word, evaluation, options):
        'DictionaryWordQ[word_,  OptionsPattern[%(name)s]]'
        if not isinstance(word, String):
            return False
        wordnet, language_code = self._load_wordnet(evaluation, self._language_name(evaluation, options))
        if wordnet:
            if list(wordnet.synsets(word.get_string_value().lower(), None, language_code)):
                return Symbol('System`True')
            else:
                return Symbol('System`False')


class DictionaryLookup(_WordListBuiltin):
    """
    <dl>
    <dt>'DictionaryLookup[$word$]'
      <dd>lookup words that match the given $word$ or pattern.

    <dt>'DictionaryLookup[$word$, $n$]'
      <dd>lookup first $n$ words that match the given $word$ or pattern.
    </dl>

    >> DictionaryLookup["bake" ~~ ___, 3]
     = {bake, bakeapple, baked}
    """

    def compile(self, pattern, evaluation):
        re_patt = to_regex(pattern, evaluation)
        if re_patt is None:
            evaluation.message('StringExpression', 'invld', pattern, Expression('StringExpression', pattern))
            return
        re_patt = anchor_pattern(re_patt)

        return re.compile(re_patt, flags=re.IGNORECASE)

    def search(self, dictionary_words, pattern):
        for dictionary_word in dictionary_words:
            if pattern.match(dictionary_word):
                yield dictionary_word.replace('_', ' ')

    def lookup(self, language_name, word, n, evaluation):
        pattern = self.compile(word, evaluation)
        if pattern:
            dictionary_words = self._words(language_name, 'All', evaluation)
            if dictionary_words:
                matches = self.search(dictionary_words, pattern)
                if n is not None:
                    matches = itertools.islice(matches, 0, n)
                return Expression('List', *(String(match) for match in sorted(matches)))

    def apply_english(self, word, evaluation):
        'DictionaryLookup[word_]'
        return self.lookup(String('English'), word, None, evaluation)

    def apply_language(self, language, word, evaluation):
        'DictionaryLookup[{language_String, word_}]'
        return self.lookup(language, word, None, evaluation)

    def apply_english_n(self, word, n, evaluation):
        'DictionaryLookup[word_, n_Integer]'
        return self.lookup(String('English'), word, n.get_int_value(), evaluation)

    def apply_language_n(self, language, word, n, evaluation):
        'DictionaryLookup[{language_String, word_}, n_Integer]'
        return self.lookup(language, word, n.get_int_value(), evaluation)


class WordList(_WordListBuiltin):
    """
    <dl>
    <dt>'WordList[]'
      <dd>returns a list of common words.
    <dt>'WordList[$type]'
      <dd>returns a list of common words of type $type$.
    </dl>

    >> N[Mean[StringLength /@ WordList["Adjective"]], 2]
     = 9.3
    """

    def apply(self, evaluation, options):
        'WordList[OptionsPattern[%(name)s]]'
        words = self._words(self._language_name(evaluation, options), 'All', evaluation)
        if words:
            return Expression('List', *[String(w) for w in words])

    def apply_type(self, wordtype, evaluation, options):
        'WordList[wordtype_String, OptionsPattern[%(name)s]]'
        words = self._words(self._language_name(evaluation, options), wordtype.get_string_value(), evaluation)
        if words:
            return Expression('List', *[String(w) for w in words])


class RandomWord(_WordListBuiltin):
    """
    <dl>
    <dt>'RandomWord[]'
      <dd>returns a random word.
    <dt>'RandomWord[$type$]'
      <dd>returns a random word of the given $type$, e.g. of type "Noun" or "Adverb".
    <dt>'RandomWord[$type$, $n$]'
      <dd>returns $n$ random words of the given $type$.
    </dl>
    """

    def _random_words(self, type, n, evaluation, options):
        words = self._words(self._language_name(evaluation, options), type, evaluation)
        with RandomEnv(evaluation) as rand:
            return [String(words[rand.randint(0, len(words) - 1)].replace('_', ' ')) for _ in range(n)]

    def apply(self, evaluation, options):
        'RandomWord[OptionsPattern[%(name)s]]'
        words = self._random_words('All', 1, evaluation, options)
        if words:
            return words[0]

    def apply_type(self, type, evaluation, options):
        'RandomWord[type_String, OptionsPattern[%(name)s]]'
        words = self._random_words(type.get_string_value(), 1, evaluation, options)
        if words:
            return words[0]

    def apply_type_n(self, type, n, evaluation, options):
        'RandomWord[type_String, n_Integer, OptionsPattern[%(name)s]]'
        words = self._random_words(type.get_string_value(), n.get_int_value(), evaluation, options)
        if words:
            return Expression('List', *words)


class LanguageIdentify(Builtin):
    '''
    <dl>
    <dt>'LanguageIdentify[$text$]'
      <dd>returns the name of the language used in $text$.
    </dl>

    >> LanguageIdentify["eins zwei drei"]
     = German
    '''

    requires = (
        'langid',
        'pycountry',
    )

    def apply(self, text, evaluation):
        'LanguageIdentify[text_String]'
        import langid  # see https://github.com/saffsd/langid.py
        # an alternative: https://github.com/Mimino666/langdetect
        import pycountry
        code, _ = langid.classify(text.get_string_value())
        language = pycountry.languages.get(alpha_2=code)
        if language is None:
            return Symbol("$Failed")
        return String(language.name)


class Pluralize(Builtin):
    '''
    <dl>
    <dt>'Pluralize[$word$]'
      <dd>returns the plural form of $word$.
    </dl>

    >> Pluralize["potato"]
     = potatoes
    '''

    requires = (
        'pattern',
    )

    def apply(self, word, evaluation):
        'Pluralize[word_String]'
        from pattern.en import pluralize
        return String(pluralize(word.get_string_value()))


class SpellingCorrectionList(Builtin):
    '''
    <dl>
    <dt>'SpellingCorrectionList[$word$]'
      <dd>returns a list of suggestions for spelling corrected versions of $word$.
    </dl>

    Results may differ depending on which dictionaries can be found by enchant.

    >> SpellingCorrectionList["hipopotamus"]
     = {hippopotamus...}
    '''

    requires = (
        'enchant',
    )

    options = {
        'Language': '"English"',
    }

    messages = {
        'lang': 'SpellingCorrectionList does not support `1` as a language.',
    }

    _languages = {
        'English': 'en_US',  # en_GB, en_AU
        'German': 'de_DE',
        'French': 'fr_FR',
    }

    _dictionaries = {}

    def apply(self, word, evaluation, options):
        'SpellingCorrectionList[word_String, OptionsPattern[%(name)s]]'
        import enchant

        language_name = self.get_option(options, 'Language', evaluation)
        if not isinstance(language_name, String):
            return
        language_code = SpellingCorrectionList._languages.get(
            language_name.get_string_value(), None)
        if not language_code:
            return evaluation.message('SpellingCorrectionList', 'lang', language_name)

        d = SpellingCorrectionList._dictionaries.get(language_code, None)
        if not d:
            d = enchant.Dict(language_code)
            SpellingCorrectionList._dictionaries[language_code] = d

        py_word = word.get_string_value()

        if d.check(py_word):
            return Expression('List', word)
        else:
            return Expression('List', *[String(s) for s in d.suggest(py_word)])
