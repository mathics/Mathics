#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
XML
"""

from mathics.builtin.base import Builtin
from mathics.builtin.files import mathics_open
from mathics.core.expression import Expression, String

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

def xml_plaintext(node):
    return String('\n'.join(node.itertext()))

def xml_tags(root):
    tags = set()

    def gather(node):
        tags.add(node.tag)
        for child in node:
            gather(child)
    gather(root)
    return Expression('List', *[String(tag) for tag in tags])

def node_to_xml_element(node):
    if lxml_available:
        if isinstance(node, ET._Comment):
            items = [Expression(Expression('XMLObject', String('Comment')), String(node.text))]  # FIXME
            if node.tail is not None:
                items.append(String(node.tail))
            return items

    def children():
        if node.text is not None:
            yield String(node.text)
        for child in node:
            for element in node_to_xml_element(child):
                yield element
        if node.tail is not None:
            yield String(node.tail)

    return [Expression('XMLElement', String(node.tag), Expression('List'), Expression('List', *list(children())))]

def xml_object(root):
    if lxml_available:
        tree = root.getroottree()
        declaration = [Expression(Expression('XMLObject', String('Declaration')),
            Expression('Rule', String('Version'), String(tree.docinfo.xml_version)),
            Expression('Rule', String('Encoding'), String(tree.docinfo.encoding)))]
    else:
        declaration = []

    return Expression(Expression('XMLObject', String('Document')), Expression('List', *declaration), *node_to_xml_element(root))


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
    # Import["ExampleData/InventionNo1.xml", "Plaintext"]

    context = 'System`XML`'

    def apply(self, text, evaluation):
        '''%(name)s[text_String]'''
        root = parse_xml(text.get_string_value())
        plaintext = String('\n'.join(root.itertext()))
        return Expression('List', Expression('Rule', 'Plaintext', plaintext))


class TagsImport(Builtin):
    # Import["ExampleData/InventionNo1.xml", "Tags"]]

    context = 'System`XML`'

    def apply(self, text, evaluation):
        '''%(name)s[text_String]'''
        root = parse_xml(text.get_string_value())
        return Expression('List', Expression('Rule', 'Tags', xml_tags(root)))

class XMLObjectImport(Builtin):
    # Import["ExampleData/InventionNo1.xml", "XMLObject"]]

    context = 'System`XML`'

    def apply(self, text, evaluation):
        '''%(name)s[text_String]'''
        root = parse_xml(text.get_string_value())
        return Expression('List', Expression('Rule', 'XMLObject', xml_object(root)))
