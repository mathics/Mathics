'''
Rather than trying to parse all the code at once this module implemets methods
for returning one line code at a time.
'''

class LineFeeder(object):
    def feed(self):
        return ''

    def empty(self):
        return True


class SingleLineFeeder(LineFeeder):
    'Feeds all the code as a single line.'
    def __init__(self, code):
        self.code = code
        self.empty = False

    def feed(self):
        self.empty = True
        return self.code

    def empty():
        return self.empty


class MultiLineFeeder(LineFeeder):
    'Feeds one line at a time.'
    def __init__(self, lines):
        self.lineno = 0
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
