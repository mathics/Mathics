# -*- coding: utf-8 -*-
# cython: language_level=3

"""
File Stream Operations
"""
import io
import requests
import sys
import tempfile

from typing import Optional

import os.path as osp

from mathics.settings import ROOT_DIR

HOME_DIR = osp.expanduser("~")
PATH_VAR = [".", HOME_DIR, osp.join(ROOT_DIR, "data"), osp.join(ROOT_DIR, "packages")]

def create_temporary_file(suffix=None, delete=False):
    if suffix == "":
        suffix = None

    fp = tempfile.NamedTemporaryFile(delete=delete, suffix=suffix)
    result = fp.name
    fp.close()
    return result


def urlsave_tmp(url, location=None, **kwargs):
    suffix = ""
    strip_url = url.split("/")
    if len(strip_url) > 3:
        strip_url = strip_url[-1]
        if strip_url != "":
            suffix = strip_url[len(strip_url.split(".")[0]) :]
        try:
            r = requests.get(url, allow_redirects=True)
            if location is None:
                location = create_temporary_file(suffix=suffix)
            with open(location, "wb") as fp:
                fp.write(r.content)
                result = fp.name
        except Exception:
            result = None
    return result


def path_search(filename):
    # For names of the form "name`", search for name.mx and name.m
    if filename[-1] == "`":
        filename = filename[:-1].replace("`", osp.sep)
        for ext in [".mx", ".m"]:
            result = path_search(filename + ext)
            if result is not None:
                filename = None
                break
    if filename is not None:
        result = None
        # If filename is an internet address, download the file
        # and store it in a temporal location
        lenfn = len(filename)
        if (
            (lenfn > 7 and filename[:7] == "http://")
            or (lenfn > 8 and filename[:8] == "https://")
            or (lenfn > 6 and filename[:6] == "ftp://")
        ):
            result = urlsave_tmp(filename)
        else:
            for p in PATH_VAR + [""]:
                path = osp.join(p, filename)
                if osp.exists(path):
                    result = path
                    break

            # If FindFile resolves to a dir, search within for Kernel/init.m and init.m
            if result is not None and osp.isdir(result):
                for ext in [osp.join("Kernel", "init.m"), "init.m"]:
                    tmp = osp.join(result, ext)
                    if osp.isfile(tmp):
                        return tmp
    return result


class StreamsManager(object):
    __instance = None
    STREAMS = {}
    @staticmethod
    def get_instance():
        """ Static access method. """
        if StreamsManager.__instance == None:
            StreamsManager()
        return StreamsManager.__instance

    def __init__(self):
        """ Virtually private constructor. """
        if StreamsManager.__instance != None:
            raise Exception("this class is a singleton!")
        else:
            StreamsManager.__instance = self

    def lookup_stream(self, n=None) -> Optional["Stream"]:
        if n is None:
            return None
        return self.STREAMS.get(n, None)

    def delete_stream(self, n: int) -> bool:
        stream = self.STREAMS.get(n, None)
        if stream is not None:
            del self.STREAMS[stream.n]
            return True
        return False

    def add(self, name: str, mode:str, encoding, stream, num: Optional[int]=None) -> Optional["Stream"]:
        if num is None:
            num = self.next
            # In theory in this branch we won't find num.
        # sanity check num
        found = self.lookup_stream(num)
        if found and found is not None:
            raise Exception(f"Stream {num} already open")
        stream = Stream(name, mode, encoding, stream, num)
        self.STREAMS[num] = stream
        return stream

    @property
    def count(self):
        return len(self.STREAMS)

    @property
    def next(self):
        return len(self.STREAMS) + 1


stream_manager = StreamsManager()

def channel_to_stream(channel:str, mode="r"):
    if mode not in ("r", "rb", "w", "a", "wb", "ab"):
        raise ValueError(f"Unknown format {mode}")

    opener = Stream(channel, mode)
    return opener.__enter__()


class Stream(object):
    """
    Opens a stream

    This can be used in a context_manager like this:

    with Stream(pypath, "r") as f:
         ...

    However see mathics_open which wraps this
    """
    def __init__(self, name: str, mode="r", encoding=None, stream=None, num=None):
        if num is None:
            num = stream_manager.next
        self.name = name
        self.mode = mode
        self.encoding = encoding
        self.stream = stream
        self.n = num
        self.old_inputfile_var = None  # Set in __enter__ and __exit__

        if mode not in ["r", "w", "a", "rb", "wb", "ab"]:
            raise ValueError("Can't handle mode {0}".format(mode))

    def __enter__(self):
        # find path
        path = path_search(self.name)
        if path is None and self.mode in ["w", "a", "wb", "ab"]:
            path = self.name
        if path is None:
            raise IOError

        # determine encoding
        if "b" not in self.mode:
            encoding = self.encoding
        else:
            encoding = None

        # open the stream
        stream = io.open(path, self.mode, encoding=encoding)
        stream_manager.add(name=path, mode=self.mode, encoding=encoding, stream=stream)
        return stream

    def __exit__(self, type, value, traceback):
        if self.stream is not None:
            self.stream.close()
            self.stream = None
        stream_manager.delete_stream(self.n)

Stream("sys.stdin", mode="r", num=0, stream=sys.stdin)
Stream("sys.stdout", mode="w", num=1, stream=sys.stdout)
Stream("sys.stderr", mode="w", num=2, stream=sys.stderr)
