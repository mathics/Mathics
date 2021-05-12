# -*- coding: utf-8 -*-
from mathics_scanner import (
    FileLineFeeder,
    LineFeeder,
    MultiLineFeeder,
    SingleLineFeeder,
)


class MathicsLineFeeder(LineFeeder):
    def send_messages(self, evaluation):
        for message in self.messages:
            evaluation.message(*message)
        self.messages = []


class MathicsSingleLineFeeder(SingleLineFeeder, MathicsLineFeeder):
    "A feeder that feeds lines from an open ``File`` object"


class MathicsFileLineFeeder(FileLineFeeder, MathicsLineFeeder):
    "A feeder that feeds lines from an open ``File`` object"


class MathicsMultiLineFeeder(MultiLineFeeder, MathicsLineFeeder):
    "A feeder that feeds lines from an open ``File`` object"
