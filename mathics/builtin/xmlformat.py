#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
XML
"""

from mathics.builtin.base import Builtin
from mathics.builtin.files import mathics_open
from mathics.core.expression import Expression, String, Symbol, from_python

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

    default_namespace = node.nsmap[None]

    if default_namespace != parent_namespace:
        declare_default_namespace = default_namespace
    else:
        declare_default_namespace = None

    tag = ET.QName(node.tag)
    namespace = tag.namespace

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
            yield Expression('Rule', from_python(name), from_python(value))
        if declare_default_namespace:
            yield Expression('Rule', _namespace_key, String(declare_default_namespace))

    if namespace is None or namespace == default_namespace:
        name = String(tag.localname)
    else:
        name = Expression('List', String(namespace), String(tag.localname))

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


def parse_xml_string(xml):
    if lxml_available:
        try:
            parser = ET.XMLParser(strip_cdata=False, remove_comments=False, recover=False)
            return ET.XML(xml, parser)
        except ET.XMLSyntaxError as e:
            raise ParseError(str(e))
    else:
        try:
            return ET.fromstring(xml)
        except ET.ParseError as e:
            raise ParseError(str(e))


def parse_xml_file(filename):
    with mathics_open(filename, 'rb') as f:
        root = parse_xml_string(f.read())
    return root


def parse_xml(parse, text, evaluation):
    try:
        return parse(text.get_string_value())
    except ParseError as e:
        evaluation.message('XML`Parser`Get', 'prserr', str(e))
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
        return parse_xml_string(text)


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

    >> Import["ExampleData/Namespaces.xml"]
     = XMLObject[Document][{XMLObject[Declaration][Version -> 1.0, Encoding -> UTF-8]}, XMLElement[book, {{http://www.w3.org/2000/xmlns/, xmlns} -> urn:loc.gov:books}, {XMLElement[title, {}, {Cheaper by the Dozen}], XMLElement[{urn:ISBN:0-395-36341-6, number}, {}, {1568491379}], XMLElement[notes, {}, {XMLElement[p, {{http://www.w3.org/2000/xmlns/, xmlns} -> http://www.w3.org/1999/xhtml}, {This is a, XMLElement[i, {}, {funny, book!}]}]}]}]]
    """

    context = 'XML`'

    def apply(self, text, evaluation):
        '''%(name)s[text_String]'''
        xml = Expression('XML`Parser`XMLGet', text).evaluate(evaluation)
        return Expression('List', Expression('Rule', 'XMLObject', xml))

