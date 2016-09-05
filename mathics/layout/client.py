#!/usr/bin/env python
# -*- coding: utf-8 -*-

# you may use "NODE" in settings.py to specify a custom node binary, and you may use NODE_PATH in settings.py
# to specify a custom node_modules path (that has the necessary node modules mathjax-node, ...).
#
# Your installation of nodejs with the following packages: mathjax-node svg2png (install them using
# npm).

# Tips for installing nodejs on OS X:
# see https://gist.github.com/DanHerbert/9520689
# export NODE_PATH=/your/path/to/homebrew/bin/node_modules:$NODE_PATH

import subprocess
from subprocess import Popen
import os

from mathics import settings

import socket
import json
import struct


class Pipe:
    def __init__(self, sock):
        self.sock = sock

    # the following three functions are taken from
    # http://stackoverflow.com/questions/17667903/python-socket-receive-large-amount-of-data

    def _recvall(self, n):
        # Helper function to recv n bytes or return None if EOF is hit
        data = b''
        sock = self.sock
        while len(data) < n:
            packet = sock.recv(n - len(data))
            if not packet:
                return None
            data += packet
        return data

    def put(self, msg):
        msg = json.dumps(msg).encode('utf8')
        # Prefix each message with a 4-byte length (network byte order)
        msg = struct.pack('>I', len(msg)) + msg
        self.sock.sendall(msg)

    def get(self):
        # Read message length and unpack it into an integer
        raw_msglen = self._recvall(4)
        if not raw_msglen:
            return None
        msglen = struct.unpack('>I', raw_msglen)[0]
        # Read the message data
        return json.loads(self._recvall(msglen).decode('utf8'))


class RemoteMethod:
    def __init__(self, socket, name):
        self.pipe = Pipe(socket)
        self.name = name

    def __call__(self, *args):
        self.pipe.put({'call': self.name, 'args': args})
        return self.pipe.get()


class Client:
    def __init__(self, ip, port):
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.socket.connect((ip, port))

    def __getattr__(self, name):
        return RemoteMethod(self.socket, name)

    def close(self):
        return self.socket.close()


# Why WebEngine? Well, QT calls its class for similar stuff "web engine", an engine
# that "provides functionality for rendering regions of dynamic web content". This
# is not about web servers but layout (http://doc.qt.io/qt-5/qtwebengine-index.html).


class WebEngineUnavailable(RuntimeError):
    pass


class NoWebEngine:
    def assume_is_available(self):
        raise WebEngineUnavailable

    def mathml_to_svg(self, mathml):
        raise WebEngineUnavailable

    def rasterize(self, svg, *args, **kwargs):
        raise WebEngineUnavailable


class WebEngine:
    def __init__(self):
        self.process = None
        self.client = None
        self.unavailable = None

    def _create_client(self):
        try:
            popen_env = os.environ.copy()
            if settings.NODE_MODULES:
                popen_env["NODE_PATH"] = os.path.expandvars(settings.NODE_MODULES)

            server_path = os.path.join(
                os.path.dirname(os.path.realpath(__file__)), 'server.js')

            def abort(message):
                error_text = 'Node.js failed to startup %s:\n\n' % server_path
                raise WebEngineUnavailable(error_text + message)

            process = Popen(
                [os.path.expandvars(settings.NODE), server_path],
                stdout=subprocess.PIPE,
                env=popen_env)

            hello = 'HELLO:'  # agreed upon "all ok" hello message.

            status = process.stdout.readline().decode('utf8').strip()
            if not status.startswith(hello):
                error = ''
                while True:
                    line = process.stdout.readline().decode('utf8')
                    if not line:
                        break
                    error += '  ' + line

                process.terminate()
                abort(error + '\nPlease check Node.js modules and NODE_PATH')

            port = int(status[len(hello):])
        except OSError as e:
            abort(str(e))

        try:
            self.client = Client('127.0.0.1', port)
            self.process = process
        except Exception as e:
            self.client = None
            self.process = None
            process.terminate()
            abort(str(e))

    def _ensure_client(self):
        if not self.client:
            if self.unavailable is not None:
                raise WebEngineUnavailable(self.unavailable)
            try:
                self._create_client()
            except WebEngineUnavailable as e:
                self.unavailable = str(e)
                raise e

        return self.client

    def assume_is_available(self):
        if self.unavailable is not None:
            raise WebEngineUnavailable(self.unavailable)

    def mathml_to_svg(self, mathml):
        return self._ensure_client().mathml_to_svg(mathml)

    def rasterize(self, svg, size):
        return self._ensure_client().rasterize(svg, size)

    def terminate(self):
        if self.process:
            self.process.terminate()
            self.process = None
            self.client = None

