#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import sys


def error(msg):
    print('error: {}'.format(msg))
    sys.exit(1)


def main():
    try:
        script_filename = sys.argv[1]
    except IndexError:
        error('script filename missing')
    script_args = sys.argv[1:]

    if sys.argv[0].endswith('script'):
        shell = sys.argv[0][:-6]
    else:
        shell = None
    if shell is None or not os.path.exists(shell):
        error('cannot find mathics shell')
    args = [shell, '-script', script_filename, '--'] + script_args
    os.system(' '.join(args))
