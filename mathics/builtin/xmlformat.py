#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
XML
"""

from __future__ import unicode_literals


from mathics.builtin.base import Builtin
from mathics.builtin.files import mathics_open
from mathics.core.expression import Expression, String, Symbol, from_python
from mathics.builtin.base import MessageException

from io import StringIO
import re

# use lxml, if available, as it has some additional features such as parsing XML
# versions, comments and cdata. fallback on python builtin xml parser otherwise.

try:
    from lxml import etree as ET
    lxml_available = True
except ImportError:
    import xml.etree.ElementTree as ET
    lxml_available = False


def xml_cdata(node):
    if lxml_available:
        return String('\n'.join(node.itertext()))


def xml_comments(node):
    if lxml_available:
        return Expression('List', *[String(s.text) for s in node.xpath('//comment()')])


_namespace_key = Expression('List', String('http://www.w3.org/2000/xmlns/'), String('xmlns'))


def node_to_xml_element(node, parent_namespace=None, strip_whitespace=True):
    if lxml_available:
        if isinstance(node, ET._Comment):
            items = [Expression(Expression('XMLObject', String('Comment')), String(node.text))]
            if node.tail is not None:
                items.append(String(node.tail))
            return items

    # see https://reference.wolfram.com/language/XML/tutorial/RepresentingXML.html

    default_namespace = node.get('xmlns')
    if default_namespace is None:
        default_namespace = parent_namespace

    if lxml_available:
        tag = ET.QName(node.tag)
        localname = tag.localname
        namespace = tag.namespace
    else:
        tag = node.tag
        if not tag.startswith('{'):
            namespace = None
            localname = tag
        else:
            m = re.match('\{(.*)\}(.*)', node.tag)
            namespace = m.group(1)
            localname = m.group(2)

    def children():
        text = node.text
        if text:
            if strip_whitespace:
                text = text.strip()
            if text:
                yield String(text)
        for child in node:
            for element in node_to_xml_element(child, default_namespace, strip_whitespace):
                yield element
        tail = node.tail
        if tail:
            if strip_whitespace:
                tail = tail.strip()
            if tail:
                yield String(tail)

    def attributes():
        for name, value in node.attrib.items():
            if name == 'xmlns':
                name = _namespace_key
            else:
                name = from_python(name)
            yield Expression('Rule', name, from_python(value))

    if namespace is None or namespace == default_namespace:
        name = String(localname)
    else:
        name = Expression('List', String(namespace), String(localname))

    return [Expression(
        'XMLElement',
        name,
        Expression('List', *list(attributes())),
        Expression('List', *list(children())))]


def xml_object(root):
    if lxml_available:
        tree = root.getroottree()
        declaration = [
            Expression(Expression('XMLObject', String('Declaration')),
            Expression('Rule', String('Version'), String(tree.docinfo.xml_version)),
            Expression('Rule', String('Encoding'), String(tree.docinfo.encoding)))]
    else:
        declaration = []

    return Expression(
        Expression('XMLObject', String('Document')),
        Expression('List', *declaration), *node_to_xml_element(root))


class ParseError(Exception):
    pass


def parse_xml_stream(f):
    def parse(**kwargs):  # inspired by http://effbot.org/zone/element-namespaces.htm
        events = "start", "start-ns"

        root = None
        namespace = None

        for event, elem in ET.iterparse(f, events, **kwargs):
            if event == "start-ns":
                if not elem[0]:  # setting default namespace?
                    namespace = elem[1]
            elif event == "start":
                if root is None:
                    root = elem
                if namespace:
                    elem.set('xmlns', namespace)
                    namespace = None

        return root

    if lxml_available:
        try:
            return parse(remove_comments=False, recover=False)
        except ET.XMLSyntaxError as e:
            raise ParseError(str(e))
    else:
        try:
            return parse()
        except ET.ParseError as e:
            raise ParseError(str(e))


def parse_xml_file(filename):
    with mathics_open(filename, 'rb') as f:
        root = parse_xml_stream(f)
    return root


def parse_xml(parse, text, evaluation):
    try:
        return parse(text.get_string_value())
    except ParseError as e:
        evaluation.message('XML`Parser`Get', 'prserr', str(e))
        return Symbol('$Failed')
    except IOError:
        evaluation.message('General', 'noopen', text.get_string_value())
        return Symbol('$Failed')
    except MessageException as e:
        e.message(evaluation)
        return Symbol('$Failed')


class XMLObject(Builtin):
    pass


class XMLElement(Builtin):
    pass


class _Get(Builtin):
    context = 'XML`Parser`'

    messages = {
        'prserr': '``.',
    }

    def apply(self, text, evaluation):
        '''%(name)s[text_String]'''
        root = parse_xml(self._parse, text, evaluation)
        if isinstance(root, Symbol):  # $Failed?
            return root
        else:
            return xml_object(root)

class XMLGet(_Get):
    def _parse(self, text):
        return parse_xml_file(text)


class XMLGetString(_Get):
    def _parse(self, text):
        with StringIO() as f:
            f.write(text)
            f.seek(0)
            return parse_xml_stream(f)


class PlaintextImport(Builtin):
    """
    >> StringReplace[StringTake[Import["ExampleData/InventionNo1.xml", "Plaintext"],31],FromCharacterCode[10]->"/"]
     = MuseScore 1.2/2012-09-12/5.7/40
    """

    context = 'XML`'

    def apply(self, text, evaluation):
        '''%(name)s[text_String]'''
        root = parse_xml(parse_xml_file, text, evaluation)
        if isinstance(root, Symbol):  # $Failed?
            return root

        def lines():
            for line in root.itertext():
                s = line.strip()
                if s:
                    yield s
        plaintext = String('\n'.join(lines()))
        return Expression('List', Expression('Rule', 'Plaintext', plaintext))


class TagsImport(Builtin):
    """
    >> Take[Import["ExampleData/InventionNo1.xml", "Tags"], 10]
     = {accidental, alter, arpeggiate, articulations, attributes, backup, bar-style, barline, beam, beat-type}
    """

    context = 'XML`'

    @staticmethod
    def _tags(root):
        tags = set()

        def gather(node):
            tags.add(node.tag)
            for child in node:
                gather(child)

        gather(root)
        return Expression('List', *[String(tag) for tag in sorted(list(tags))])

    def apply(self, text, evaluation):
        '''%(name)s[text_String]'''
        root = parse_xml(parse_xml_file, text, evaluation)
        if isinstance(root, Symbol):  # $Failed?
            return root
        return Expression('List', Expression('Rule', 'Tags', self._tags(root)))


class XMLObjectImport(Builtin):
    """
    >> Part[Import["ExampleData/InventionNo1.xml", "XMLObject"], 2, 3, 1]
     = XMLElement[identification, {}, {XMLElement[encoding, {}, {XMLElement[software, {}, {MuseScore 1.2}], XMLElement[encoding-date, {}, {2012-09-12}]}]}]

    >> Part[Import["ExampleData/Namespaces.xml"], 2]
     = XMLElement[book, {{http://www.w3.org/2000/xmlns/, xmlns} -> urn:loc.gov:books}, {XMLElement[title, {}, {Cheaper by the Dozen}], XMLElement[{urn:ISBN:0-395-36341-6, number}, {}, {1568491379}], XMLElement[notes, {}, {XMLElement[p, {{http://www.w3.org/2000/xmlns/, xmlns} -> http://www.w3.org/1999/xhtml}, {This is a, XMLElement[i, {}, {funny, book!}]}]}]}]
    """

    context = 'XML`'

    def apply(self, text, evaluation):
        '''%(name)s[text_String]'''
        xml = Expression('XML`Parser`XMLGet', text).evaluate(evaluation)
        return Expression('List', Expression('Rule', 'XMLObject', xml))

