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
except ImportError:
    pass


def node_to_xml_element(node, strip_whitespace=True):
    def children():
        text = node.text
        if text:
            if strip_whitespace:
                text = text.strip()
            if text:
                yield String(text)
        for child in node:
            for element in node_to_xml_element(child, strip_whitespace):
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

    return [Expression(
        'XMLElement',
        String(node.tag),
        Expression('List', *list(attributes())),
        Expression('List', *list(children())))]


def xml_object(tree):
    declaration = [Expression(
        Expression('XMLObject', String('Declaration')),
        Expression('Rule', String('Version'), String(tree.docinfo.xml_version or "1.0")),
        Expression('Rule', String('Standalone'), String("yes") if tree.docinfo.standalone else String("no")),
        Expression('Rule', String('Encoding'), String(tree.docinfo.encoding)))]

    return Expression(
        Expression('XMLObject', String('Document')),
        Expression('List', *declaration), *node_to_xml_element(tree.getroot()))


class ParseError(Exception):
    pass


def parse_html_stream(f):
    return lhtml.parse(f)


def parse_html_file(filename):
    with mathics_open(filename, 'rb') as f:
        return parse_html_stream(f)


def parse_html(parse, text, evaluation):
    try:
        return parse(text.get_string_value())
    except IOError:
        evaluation.message('General', 'noopen', text.get_string_value())
        return Symbol('$Failed')
    except MessageException as e:
        e.message(evaluation)
        return Symbol('$Failed')


class _HTMLBuiltin(Builtin):
    context = 'HTML`'

    requires = (
        'lxml',
    )


class _TagImport(_HTMLBuiltin):
    def _import(self, tree):
        raise NotImplementedError

    def apply(self, text, evaluation):
        '''%(name)s[text_String]'''
        tree = parse_html(parse_html_file, text, evaluation)
        if isinstance(tree, Symbol):  # $Failed?
            return tree
        return Expression('List', Expression(
            'Rule', self.tag_name, self._import(tree)))


class _Get(_HTMLBuiltin):
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
    #> Head[HTML`Parser`HTMLGetString["<a></a>"]]
     = XMLObject[Document]

    #> Head[HTML`Parser`HTMLGetString["<a><b></a>"]]
     = XMLObject[Document]
    """

    def _parse(self, text):
        with BytesIO() as f:
            f.write(text.encode('utf8'))
            f.seek(0)
            return parse_html_stream(f)


class _DataImport(_TagImport):
    def _import(self, tree):
        full_data = self.full_data

        if full_data:
            def add_data(l, x):
                l.append(x)
                return l
        else:
            def add_data(l, x):
                if x is None:
                    return l
                if l is None:
                    return [x]
                elif len(x) == 1:
                    l.extend(x)
                elif x:
                    l.append(Expression('List', *x))
                return l

        newline = re.compile(r"\s+")

        def add_text(l, node):
            deep_data = traverse(node)
            if deep_data:  # if there's data, we ignore any text
                add_data(l, deep_data)
            else:
                t = []
                for s in node.xpath('.//text()'):
                    t.append(s)
                if t or full_data:
                    l.append(String(newline.sub(' ', ' '.join(t))))

        def traverse(parent):
            if full_data:
                data = []
            else:
                data = None

            for node in parent:
                tag = node.tag
                if tag == 'table':
                    row_data = []
                    for tr in node.xpath('tr'):
                        col_data = []
                        for td in tr.xpath('th|td'):
                            add_text(col_data, td)
                        add_data(row_data, col_data)
                    data = add_data(data, row_data)
                elif tag in ('ul', 'ol'):
                    list_data = []
                    for child in node:
                        deep_data = traverse(child)
                        if deep_data:
                            add_data(list_data, deep_data)
                        elif child.tag == 'li':
                            add_text(list_data, child)
                    data = add_data(data, list_data)
                else:
                    data = add_data(data, traverse(node))

            if data and len(data) == 1:
                data = data[0]

            return data

        result = traverse(tree.getroot())
        if result is None:
            result = []

        return Expression('List', *result)


class DataImport(_DataImport):
    """
    >> Import["ExampleData/PrimeMeridian.html", "Data"][[1, 1, 2, 3]]
     = {Washington, D.C., 77°03′56.07″ W (1897) or 77°04′02.24″ W (NAD 27) or 77°04′01.16″ W (NAD 83), New Naval Observatory meridian}

    #> Length[Import["ExampleData/PrimeMeridian.html", "Data"]]
     = 3
    """

    full_data = False
    tag_name = 'Data'


class FullDataImport(_DataImport):
    full_data = True
    tag_name = 'FullData'


class _LinksImport(_TagImport):
    def _links(self, root):
        raise NotImplementedError

    def _import(self, tree):
        return Expression('List', *list(self._links(tree)))


class HyperlinksImport(_LinksImport):
    """
    >> Import["ExampleData/PrimeMeridian.html", "Hyperlinks"][[1]]
     = /wiki/Prime_meridian_(Greenwich)
    """

    tag_name = 'Hyperlinks'

    def _links(self, tree):
        for link in tree.xpath('//a'):
            href = link.get('href')
            if href and not href.startswith('#'):
                yield href


class ImageLinksImport(_LinksImport):
    """
    >> Import["ExampleData/PrimeMeridian.html", "ImageLinks"][[6]]
     = //upload.wikimedia.org/wikipedia/commons/thumb/d/d5/Prime_meridian.jpg/180px-Prime_meridian.jpg
    """

    tag_name = 'ImageLinks'

    def _links(self, tree):
        for link in tree.xpath('//img'):
            src = link.get('src')
            if src:
                yield src


class PlaintextImport(_TagImport):
    """
    >> DeleteDuplicates[StringCases[Import["ExampleData/PrimeMeridian.html"], RegularExpression["Wiki[a-z]+"]]]
     = {Wikipedia, Wikidata, Wikibase, Wikimedia}
    """

    tag_name = 'Plaintext'

    def _import(self, tree):
        def lines():
            for s in tree.xpath('//text()'):
                t = s.strip()
                if t:
                    yield t

        return String('\n'.join(lines()))


class SourceImport(_HTMLBuiltin):
    """
    >> DeleteDuplicates[StringCases[Import["ExampleData/PrimeMeridian.html", "Source"], RegularExpression["<t[a-z]+>"]]]
     = {<title>, <tr>, <th>, <td>}
    """

    def apply(self, text, evaluation):
        '''%(name)s[text_String]'''
        def source(filename):
            with mathics_open(filename, 'r', encoding='UTF-8') as f:
                return Expression('List', Expression('Rule', 'Source',  String(f.read())))

        return parse_html(source, text, evaluation)


class TitleImport(_TagImport):
    """
    >> Import["ExampleData/PrimeMeridian.html", "Title"]
     = Prime meridian - Wikipedia
    """

    tag_name = 'Title'

    def _import(self, tree):
        for node in tree.xpath('//title'):
            return String(node.text_content())
        return String('')


class XMLObjectImport(_HTMLBuiltin):
    """
    >> Part[Import["ExampleData/PrimeMeridian.html", "XMLObject"], 2, 3, 1, 3, 2]
     = XMLElement[title, {}, {Prime meridian - Wikipedia}]
    """

    def apply(self, text, evaluation):
        '''%(name)s[text_String]'''
        xml = Expression('HTML`Parser`HTMLGet', text).evaluate(evaluation)
        return Expression('List', Expression('Rule', 'XMLObject', xml))

