#!/usr/bin/env python
# -*- coding: utf-8 -*-

# you may use "NODE" in settings.py to specify a custom node binary, and you may use NODE_PATH in settings.py
# to specify a custom node_modules path (that has the necessary node modules mathjax-node, ...).
#
# Your installation of nodejs with the following packages: mathjax-node svg2png (install them using
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

import socket
import json
import struct

# the following three functions are taken from
# http://stackoverflow.com/questions/17667903/python-socket-receive-large-amount-of-data

def send_msg(sock, msg):
    # Prefix each message with a 4-byte length (network byte order)
    msg = struct.pack('>I', len(msg)) + msg
    sock.sendall(msg)


def recv_msg(sock):
    # Read message length and unpack it into an integer
    raw_msglen = recvall(sock, 4)
    if not raw_msglen:
        return None
    msglen = struct.unpack('>I', raw_msglen)[0]
    # Read the message data
    return recvall(sock, msglen)


def recvall(sock, n):
    # Helper function to recv n bytes or return None if EOF is hit
    data = b''
    while len(data) < n:
        packet = sock.recv(n - len(data))
        if not packet:
            return None
        data += packet
    return data


class RemoteMethod:
    def __init__(self, socket, name):
        self.socket = socket
        self.name = name

    def __call__(self, data):
        send_msg(self.socket, json.dumps({'call': self.name, 'data': data}).encode('utf8'))
        return json.loads(recv_msg(self.socket).decode('utf8'))


class Client:
    def __init__(self, ip, port):
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.socket.connect((ip, port))

    def __getattr__(self, name):
        return RemoteMethod(self.socket, name)

    def close(self):
        return self.socket.close()


class LayoutEngine(object):
    def __init__(self):
        try:
            popen_env = os.environ.copy()
            if settings.NODE_PATH:
                popen_env["NODE_PATH"] = settings.NODE_PATH

            server_path = os.path.dirname(os.path.realpath(__file__)) + "/server.js"

            def abort(message):
                error_text = 'Node.js failed to start %s:\n' % server_path
                raise RuntimeError(error_text + message)

            self.process = Popen(
                [settings.NODE, server_path],
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

                abort(error + '\nCheck Node.js modules and that NODE_PATH.')
        except OSError as e:
            abort(str(e))

        if self.process is None:
            self.client = None
        else:
            try:
                self.client = Client('127.0.0.1', 5000)
            except Exception as e:
                self.client = None
                self.process.terminate()
                abort(str(e))

    def mathml_to_svg(self, mathml):
        if self.client:
            return self.client.mathml_to_svg(mathml)

    def rasterize(self, svg):
        if self.client:
            return self.client.rasterize(svg)

    def terminate(self):
        if self.process:
            self.process.terminate()
