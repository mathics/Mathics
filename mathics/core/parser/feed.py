'''
Rather than trying to parse all the code at once this module implemets methods
for returning one line code at a time.
'''

import six
from abc import abstractmethod, ABCMeta


class LineFeeder(object):
    __metaclass__ = ABCMeta

    def __init__(self, filename):
        self.messages = []
        self.lineno = 0
        self.filename = filename

    @abstractmethod
    def feed(self):
        ''''
        Consume and return next line of code. Each line should be followed by a
        newline character. Returns '' after all lines are consumed.
        '''
        return

    @abstractmethod
    def empty(self):
        '''
        Return True once all lines have been consumed.
        '''
        return

    def message(self, sym, tag, *args):
        args = ['"' + args[i] + '"' if i < len(args) else '""'
                for i in range(3)]
        args.append(self.lineno)
        args.append('"' + self.filename + '"')
        assert len(args) == 5
        self.messages.append([sym, tag] + args)

    def send_messages(self, evaluation):
        for message in self.messages:
            evaluation.message(*message)
        self.messages = []


class SingleLineFeeder(LineFeeder):
    'Feeds all the code as a single line.'
    def __init__(self, code, filename=''):
        super(SingleLineFeeder, self).__init__(filename)
        self.code = code
        self._empty = False

    def feed(self):
        if self._empty:
            return ''
        self._empty = True
        self.lineno += 1
        return self.code

    def empty(self):
        return self._empty


class MultiLineFeeder(LineFeeder):
    'Feeds one line at a time.'
    def __init__(self, lines, filename=''):
        super(MultiLineFeeder, self).__init__(filename)
        self.lineno = 0
        if isinstance(lines, six.string_types):
            self.lines = lines.splitlines(True)
        else:
            self.lines = lines

    def feed(self):
        if self.lineno < len(self.lines):
            result = self.lines[self.lineno]
            self.lineno += 1
        else:
            result = ''
        return result

    def empty(self):
        return self.lineno >= len(self.lines)


class FileLineFeeder(LineFeeder):
    'Feeds lines from an open file object'
    def __init__(self, fileobject):
        super(FileLineFeeder, self).__init__(fileobject.name)
        self.fileobject = fileobject
        self.lineno = 0
        self.eof = False

    def feed(self):
        result = self.fileobject.readline()
        while result == '\n':
            result = self.fileobject.readline()
            self.lineno += 1
        if result:
            self.lineno += 1
        else:
            self.eof = True
        return result

    def empty(self):
        return self.eof
