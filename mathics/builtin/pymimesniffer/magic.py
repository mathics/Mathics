#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
import os.path
import logging
import io


class MagicRule(object):
    def __init__(
        self, mimeType, parentType, extensions, allowsLeadingWhiteSpace,
            magicNumbers, magicStrings):
        self.mimeType = mimeType
        self.parentType = parentType
        self.extensions = extensions
        self.allowsLeadingWhiteSpace = allowsLeadingWhiteSpace
        self.magicNumbers = magicNumbers
        self.magicStrings = magicStrings

    def __repr__(self):
        return "<rule %s>" % self.mimeType


class MagicDetector(object):
    def __init__(self, mimetypes):
        self.mimetypes = mimetypes

    def match(self, filename, data=None):
        matches = {}
        
        if not data:
            file = open(filename, 'rb')
            buf = b''
        elif isinstance(data, str):
            from io import StringIO
            file = StringIO(data)
            matches['text/plain'] =  self.mimetypes['text/plain']
            buf = ''
        elif hasattr(data, 'read'):
            buf = b''
            file = data
        else:
            from io import BytesIO
            file = BytesIO(data)
            buf = b''
            
        ext = os.path.splitext(filename)[1]

        if ext:
            ext = ext[1:]

        for mimetype, rules in self.mimetypes.items():
            for rule in rules:
                if rule.parentType and rule.parentType not in list(matches.keys()):
                    continue

                if rule.extensions and ext != ""  and ext not in rule.extensions:
                    continue

                for offset, value in rule.magicNumbers:
                    if offset + len(value) > len(buf):
                        buf += file.read(offset + len(value) - len(buf))

                    if buf[offset:offset + len(value)] == value:
                        matches[mimetype] = rule
                        break

                for caseSensitive, value in rule.magicStrings:
                    if len(value) > len(buf):
                        buf += file.read(len(value) - len(buf))

                    if buf[:len(value)] == value:
                        matches[mimetype] = rule
                        break

        return list(matches.keys())


class MagicLoader(object):
    def __init__(self, filename=None):
        if not filename:
            filename = os.path.join(os.path.dirname(__file__), 'mimetypes.xml')

        if not os.path.isfile(filename):
            raise IOError("magic mime type database '%s' doesn't exists" % filename)

        self.filename = filename
        self.mimetypes = {}

    def getText(self, node, name=None):
        from xml.dom.minidom import Node

        text = b''

        if name:
            for child in node.getElementsByTagName(name):
                text += self.getText(child).encode('utf-8', 'ignore')
        else:
            for child in node.childNodes:
                if child.nodeType == child.TEXT_NODE:
                    text += child.data.encode('utf-8', 'ignore')

        return text.decode('utf-8')

    def getAttr(self, node, attr, default=''):
        if not node.hasAttribute(attr):
            return default

        return type(default)(node.getAttribute(attr))

    def load(self, filename=None):
        from binascii import unhexlify
        from xml.dom.minidom import parse

        dom = parse(filename or self.filename)

        logging.info("loading magic database from %s", filename or self.filename)

        descriptions = dom.getElementsByTagName('description')

        for desc in descriptions:
            mimeType = self.getText(desc, 'mimeType')
            parentType = self.getText(desc, 'parentType')
            extensions = self.getText(desc, 'extensions').split(',')
            allowsLeadingWhiteSpace = self.getText(desc, 'allowsLeadingWhiteSpace') == 'true'

            magicNumbers = []

            for magicNumber in desc.getElementsByTagName('magicNumber'):
                encoding = self.getAttr(magicNumber, 'encoding', 'string')
                offset = self.getAttr(magicNumber, 'offset', 0)
                value = self.getText(magicNumber)

                if encoding == 'hex':
                    value = unhexlify(value.replace(' ', '').encode('ascii'))

                magicNumbers.append((offset, value))

            magicStrings = []

            for magicString in desc.getElementsByTagName('magicString'):
                caseSensitive = not (self.getAttr(magicString, 'caseSensitive') == 'false')
                value = self.getText(magicString)

                magicStrings.append((caseSensitive, value))

            self.mimetypes.setdefault(mimeType, []).append(MagicRule(mimeType, parentType, extensions, allowsLeadingWhiteSpace, magicNumbers, magicStrings))

        logging.info("loaded %d rules for %d MIME types from magic database", len(descriptions), len(self.mimetypes))

        return len(descriptions)

    def reload(self):
        self.mimetypes = {}
        self.load()

import unittest


class TestDetector(unittest.TestCase):
    detector = None

    def setUp(self):
        if not self.detector:
            loader = MagicLoader()
            loader.load()
            self.detector = MagicDetector(loader.mimetypes)

    def testMagicNumber(self):
        self.assertEquals(['application/zip'], self.detector.match('test.zip', 'PKtest'))
        self.assertEquals([], self.detector.match('test.zip', '_PKtest'))
        self.assertEquals([], self.detector.match('test.zip1', 'PKtest'))

        self.assertEquals(['application/gzip'], self.detector.match('test.gz', '\x1f\x8b\x08test'))
        self.assertEquals(['application/gzip'], self.detector.match('test.tgz', '\x1f\x8b\x08test'))
        self.assertEquals([], self.detector.match('test.gz1', '\x1f\x8b\x08test'))
        self.assertEquals([], self.detector.match('test.gz', '\x1f \x8b\x08test'))

        padding = ''.join([' ' for _ in range(257)])

        self.assertEquals(['application/x-tar'], self.detector.match('test.tar', padding + 'ustartest'))
        self.assertEquals([], self.detector.match('test.tar1', padding + 'ustartest'))
        self.assertEquals([], self.detector.match('test.tar', padding + 'ust artest'))


class TestLoader(unittest.TestCase):
    def testInit(self):
        self.assertRaises(IOError, MagicLoader, 'not_exists_file')

        self.assert_(MagicLoader().filename)

    def testLoad(self):
        loader = MagicLoader()

        self.assertFalse(loader.mimetypes)

        self.assert_(loader.load() > 0)

        self.assert_(loader.mimetypes)


def dump(mimetypes):
    for type, rules in mimetypes.items():
        print(type)

        for rule in rules:
            print(("\textenions = %s" % rule.extensions))
            print(("\tmagic num = %s" % rule.magicNumbers))
            print(("\tmagic str = %s" % rule.magicStrings))

if __name__ == '__main__':
    logging.basicConfig(
        level=logging.DEBUG if '-v' in sys.argv else logging.WARN,
        format='%(asctime)s %(levelname)-8s %(message)s')

    unittest.main()
