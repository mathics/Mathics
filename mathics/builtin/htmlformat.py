#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
HTML
"""

from __future__ import unicode_literals


from mathics.builtin.base import Builtin
from mathics.builtin.files import mathics_open
from mathics.core.expression import Expression, String, Symbol, from_python
from mathics.builtin.base import MessageException

from io import BytesIO
import re

try:
    import lxml.html as lhtml
    lxml_available = True
except ImportError:
    lxml_available = False

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
    tree = root.getroottree()
    declaration = [
        Expression(Expression('XMLObject', String('Declaration')),
        Expression('Rule', String('Version'), String(tree.docinfo.xml_version)),
        Expression('Rule', String('Encoding'), String(tree.docinfo.encoding)))]

    return Expression(
        Expression('XMLObject', String('Document')),
        Expression('List', *declaration), *node_to_xml_element(root))


class ParseError(Exception):
    pass


def parse_html_stream(f):
    return lhtml.parse(f)


def parse_html_file(filename):
    with mathics_open(filename, 'rb') as f:
        root = parse_html_stream(f)
    return root


def parse_html(parse, text, evaluation):
    try:
        return parse(text.get_string_value())
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
    context = 'HTML`Parser`'

    messages = {
        'prserr': '``.',
    }

    def apply(self, text, evaluation):
        '''%(name)s[text_String]'''
        root = parse_html(self._parse, text, evaluation)
        if isinstance(root, Symbol):  # $Failed?
            return root
        else:
            return xml_object(root)


class HTMLGet(_Get):
    def _parse(self, text):
        return parse_html_file(text)


class HTMLGetString(_Get):
    """
    >> Head[XML`Parser`XMLGetString["<a></a>"]]
     = XMLObject[Document]

    #> XML`Parser`XMLGetString["<a><b></a>"]
     : Opening and ending tag mismatch: b line 1 and a, line 1, column 11.
    """

    def _parse(self, text):
        with BytesIO() as f:
            f.write(text.encode('utf8'))
            f.seek(0)
            return parse_html_stream(f)


class DataImport(Builtin):
    """
    >> StringReplace[StringTake[Import["ExampleData/InventionNo1.xml", "Plaintext"],31],FromCharacterCode[10]->"/"]
     = MuseScore 1.2/2012-09-12/5.7/40
    """

    context = 'HTML`'

    def apply(self, text, evaluation):
        '''%(name)s[text_String]'''
        root = parse_html(parse_html_file, text, evaluation)
        if isinstance(root, Symbol):  # $Failed?
            return root

        full_data = False

        def data():
            for node in root.xpath("//*[local-name()='table' or local-name()='ul']"):
                if node.tag == 'table':
                    row_data = []
                    for tr in node.xpath('tr'):
                        col_data = []
                        for td in tr.xpath('td'):
                            text = []
                            for s in td.xpath('.//text()'):
                                text.append(s)
                            col_data.append(String(' '.join(text)))
                        row_data.append(Expression('List', *col_data))
                    if row_data or full_data:
                        yield Expression('List', *row_data)

        return Expression('List', Expression('Rule', 'Data', Expression('List', *list(data()))))


class HyperlinksImport(Builtin):
    """
    >> StringReplace[StringTake[Import["ExampleData/InventionNo1.xml", "Plaintext"],31],FromCharacterCode[10]->"/"]
     = MuseScore 1.2/2012-09-12/5.7/40
    """

    context = 'HTML`'

    def apply(self, text, evaluation):
        '''%(name)s[text_String]'''
        root = parse_html(parse_html_file, text, evaluation)
        if isinstance(root, Symbol):  # $Failed?
            return root

        def hyperlinks():
            for link in root.xpath('//a'):
                href = link.get('href')
                if href:
                    yield href

        return Expression('List', Expression('Rule', 'Hyperlinks', Expression('List', *list(hyperlinks()))))


class ImageLinksImport(Builtin):
    """
    >> StringReplace[StringTake[Import["ExampleData/InventionNo1.xml", "Plaintext"],31],FromCharacterCode[10]->"/"]
     = MuseScore 1.2/2012-09-12/5.7/40
    """

    context = 'HTML`'

    def apply(self, text, evaluation):
        '''%(name)s[text_String]'''
        root = parse_html(parse_html_file, text, evaluation)
        if isinstance(root, Symbol):  # $Failed?
            return root

        def hyperlinks():
            for link in root.xpath('//img'):
                src = link.get('src')
                if src:
                    yield src

        return Expression('List', Expression('Rule', 'ImageLinks', Expression('List', *list(hyperlinks()))))


class PlaintextImport(Builtin):
    """
    >> StringReplace[StringTake[Import["ExampleData/InventionNo1.xml", "Plaintext"],31],FromCharacterCode[10]->"/"]
     = MuseScore 1.2/2012-09-12/5.7/40
    """

    context = 'HTML`'

    def apply(self, text, evaluation):
        '''%(name)s[text_String]'''
        root = parse_html(parse_html_file, text, evaluation)
        if isinstance(root, Symbol):  # $Failed?
            return root

        def lines():
            for text in root.xpath('//text()'):
                s = text.strip()
                if s:
                    yield s
        plaintext = String('\n'.join(lines()))
        return Expression('List', Expression('Rule', 'Plaintext', plaintext))


class SourceImport(Builtin):
    """
    >> StringReplace[StringTake[Import["ExampleData/InventionNo1.xml", "Plaintext"],31],FromCharacterCode[10]->"/"]
     = MuseScore 1.2/2012-09-12/5.7/40
    """

    context = 'HTML`'

    def apply(self, text, evaluation):
        '''%(name)s[text_String]'''
        with mathics_open(text, 'r') as f:
            return Expression('List', Expression('Rule', 'Source',  String(f.read())))


class XMLObjectImport(Builtin):
    """
    >> Part[Import["ExampleData/InventionNo1.xml", "XMLObject"], 2, 3, 1]
     = XMLElement[identification, {}, {XMLElement[encoding, {}, {XMLElement[software, {}, {MuseScore 1.2}], XMLElement[encoding-date, {}, {2012-09-12}]}]}]

    >> Part[Import["ExampleData/Namespaces.xml"], 2]
     = XMLElement[book, {{http://www.w3.org/2000/xmlns/, xmlns} -> urn:loc.gov:books}, {XMLElement[title, {}, {Cheaper by the Dozen}], XMLElement[{urn:ISBN:0-395-36341-6, number}, {}, {1568491379}], XMLElement[notes, {}, {XMLElement[p, {{http://www.w3.org/2000/xmlns/, xmlns} -> http://www.w3.org/1999/xhtml}, {This is a, XMLElement[i, {}, {funny, book!}]}]}]}]
    """

    context = 'HTML`'

    def apply(self, text, evaluation):
        '''%(name)s[text_String]'''
        xml = Expression('HTML`Parser`HTMLGet', text).evaluate(evaluation)
        return Expression('List', Expression('Rule', 'XMLObject', xml))

