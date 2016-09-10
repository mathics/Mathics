#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
XML
"""

from mathics.builtin.base import Builtin
from mathics.builtin.files import mathics_open
from mathics.core.expression import Expression, String

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


def node_to_xml_element(node, strip_whitespace=True):
    if lxml_available:
        if isinstance(node, ET._Comment):
            items = [Expression(Expression('XMLObject', String('Comment')), String(node.text))]
            if node.tail is not None:
                items.append(String(node.tail))
            return items

    def children():
        text = node.text
        if text:
            if strip_whitespace:
                text = text.strip()
            if text:
                yield String(text)
        for child in node:
            for element in node_to_xml_element(child):
                yield element
        tail = node.tail
        if tail:
            if strip_whitespace:
                tail = tail.strip()
            if tail:
                yield String(tail)

    return [Expression('XMLElement', String(node.tag), Expression('List'), Expression('List', *list(children())))]


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


def parse_xml(filename):
    with mathics_open(filename) as f:
        xml = f.read()
        if lxml_available:
            parser = ET.XMLParser(strip_cdata=False, remove_comments=False, recover=True)
            root = ET.XML(xml, parser)
        else:
            root = ET.fromstring(xml)
    return root


class PlaintextImport(Builtin):
    """
    >> StringReplace[StringTake[Import["ExampleData/InventionNo1.xml", "Plaintext"],31],"\n"->"/"]
     = MuseScore 1.2/2012-09-12/5.7/40
    """

    context = 'System`XML`'

    def apply(self, text, evaluation):
        '''%(name)s[text_String]'''
        root = parse_xml(text.get_string_value())

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
     = {accidental,alter,arpeggiate,articulations,attributes,backup,bar-style,barline,beam,beat-type}
    """

    context = 'System`XML`'

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
        root = parse_xml(text.get_string_value())
        return Expression('List', Expression('Rule', 'Tags', self._tags(root)))


class XMLObjectImport(Builtin):
    # Import["ExampleData/InventionNo1.xml", "XMLObject"]

    context = 'System`XML`'

    def apply(self, text, evaluation):
        '''%(name)s[text_String]'''
        root = parse_xml(text.get_string_value())
        return Expression('List', Expression('Rule', 'XMLObject', xml_object(root)))
