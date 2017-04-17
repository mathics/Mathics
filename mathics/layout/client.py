#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Your installation of nodejs with the following packages: mathjax-node svg2png (install them using
# npm).

# Tips for installing nodejs on OS X:
# see https://gist.github.com/DanHerbert/9520689
# export NODE_PATH=/your/path/to/homebrew/bin/node_modules:$NODE_PATH

import subprocess
from subprocess import Popen
import os

import socket
import json
import struct


class WebEngineError(RuntimeError):
    pass


class WebEngineUnavailable(WebEngineError):
    pass


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
        reply = self.pipe.get()

        error = reply.get('error')
        if error:
            raise WebEngineError(str(error))
        else:
            return reply.get('data')


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

class NoWebEngine:
    def assume_is_available(self):
        raise WebEngineUnavailable

    def mathml_to_svg(self, mathml):
        raise WebEngineUnavailable

    def rasterize(self, svg, *args, **kwargs):
        raise WebEngineUnavailable


def _normalize_svg(svg):
    import xml.etree.ElementTree as ET
    import base64
    import re

    ET.register_namespace('', 'http://www.w3.org/2000/svg')
    root = ET.fromstring(svg)
    prefix = 'data:image/svg+xml;base64,'

    def rewrite(up):
        changes = []

        for i, node in enumerate(up):
            if node.tag == '{http://www.w3.org/2000/svg}image':
                src = node.attrib.get('src', '')
                if src.startswith(prefix):
                    attrib = node.attrib

                    if 'width' in attrib and 'height' in attrib:
                        target_width = float(attrib['width'])
                        target_height = float(attrib['height'])
                        target_transform = attrib.get('transform', '')

                        image_svg = _normalize_svg(base64.b64decode(src[len(prefix):]))
                        root = ET.fromstring(image_svg)

                        view_box = re.split('\s+', root.attrib.get('viewBox', ''))

                        if len(view_box) == 4:
                            x, y, w, h = (float(t) for t in view_box)
                            root.tag = '{http://www.w3.org/2000/svg}g'
                            root.attrib = {'transform': '%s scale(%f, %f) translate(%f, %f)' % (
                                target_transform, target_width / w, target_height / h, -x, -y)}

                            changes.append((i, node, root))
            else:
                rewrite(node)

        for i, node, new_node in reversed(changes):
            up.remove(node)
            up.insert(i, new_node)

    rewrite(root)

    return ET.tostring(root, 'utf8').decode('utf8')


class WebEngine:
    def __init__(self):
        self.process = None
        self.client = None
        self.unavailable = None

    def _create_client(self):
        try:
            popen_env = os.environ.copy()

            server_path = os.path.join(
                os.path.dirname(os.path.realpath(__file__)), 'server.js')

            if True:
                # fixes problems on Windows network drives
                import tempfile
                fd, copied_path = tempfile.mkstemp(suffix='js')
                with open(server_path, 'rb') as f:
                    os.write(fd, f.read())
                os.fsync(fd)
                server_path = copied_path

            def abort(message):
                error_text = 'Node.js failed to startup %s:\n\n' % server_path
                raise WebEngineUnavailable(error_text + message)

            process = Popen(
                ['node', server_path],
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
        buffer = self._ensure_client().rasterize(_normalize_svg(svg), size)
        return bytearray(buffer['data'])

    def terminate(self):
        if self.process:
            self.process.terminate()
            self.process = None
            self.client = None

