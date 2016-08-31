#!/usr/bin/env python
# -*- coding: utf-8 -*-

# In order to use LayoutEngine, you need the "zerorpc" in your Python installation, and nodejs in your path.
# you may use "NODE" in settings.py to specify a custom node binary, and you may use NODE_PATH in settings.py
# to specify a custom node_modules path (that has the necessary node modules mathjax-node, zerorpc, ...).
#
# The zerorpc package from https://github.com/0rpc/zerorpc-python. needs to be installed manually using
# "/your/python setup.py install". For Python 3, you need to use the zerorpc 3.4 branch.

# Your installation of nodejs with the following packages: mathjax-node zerorpc svg2png (install them using
# npm).

# Some tips for installing nodejs and zmq on OS X:
# see https://gist.github.com/DanHerbert/9520689
# https://github.com/JustinTulloss/zeromq.node/issues/283
# brew install zmq && npm install zmq
# export NODE_PATH=/your/path/to/homebrew/bin/node_modules:$NODE_PATH

import subprocess
from subprocess import Popen
import os

from mathics import settings

try:
    import zerorpc
    supported = True
except ImportError:
    supported = False


class LayoutEngine(object):
    def __init__(self):
        if not supported:
            raise RuntimeError('Web layout engine is disabled as zerorpc is not installed.')
        else:
            try:
                popen_env = os.environ.copy()
                if settings.NODE_PATH:
                    popen_env["NODE_PATH"] = settings.NODE_PATH

                base = os.path.dirname(os.path.realpath(__file__))

                self.process = Popen(
                    [settings.NODE, base + "/server.js"],
                    stdout=subprocess.PIPE,
                    env=popen_env)

                status = self.process.stdout.readline().decode('utf8').strip()
                if status != 'OK':
                    error = ''
                    while True:
                        line = self.process.stdout.readline().decode('utf8')
                        if not line:
                            break
                        error += '  ' + line

                    self.process.terminate()

                    raise RuntimeError(
                        'Node.js failed to start web layout engine:\n' + error + '\n' +
                        'Check necessary node.js modules and that NODE_PATH is set correctly.')
            except OSError as e:
                raise RuntimeError('Failed to start web layout engine: ' + str(e))

        if self.process is None:
            self.client = None
        else:
            try:
                self.client = zerorpc.Client()
                self.client.connect("tcp://127.0.0.1:4241")
            except Exception as e:
                self.client = None
                self.process.terminate()
                raise RuntimeError(
                    'node.js failed to start web layout engine: \n' + str(e) + '\n' +
                    'Probably you are missing node.js modules.')

    def mathml_to_svg(self, mathml):
        if self.client:
            return self.client.mathml_to_svg(mathml)

    def rasterize(self, svg):
        if self.client:
            return self.client.rasterize(svg)

    def terminate(self):
        if self.process:
            self.process.terminate()
