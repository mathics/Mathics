# -*- coding: utf-8 -*-

"""
File Stream Operations
"""
from io import open as io_open
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
        """Static access method."""
        if StreamsManager.__instance == None:
            StreamsManager()
        return StreamsManager.__instance

    def __init__(self):
        """Virtually private constructor."""
        if StreamsManager.__instance != None:
            raise Exception("this class is a singleton!")
        else:
            StreamsManager.__instance = self

    def add(
        self,
        name: str,
        mode: Optional[str] = None,
        encoding=None,
        io=None,
        num: Optional[int] = None,
    ) -> Optional["Stream"]:
        if num is None:
            num = self.next
            # In theory in this branch we won't find num.
        # sanity check num
        found = self.lookup_stream(num)
        if found and found is not None:
            raise Exception(f"Stream {num} already open")
        stream = Stream(name, mode, encoding, io, num)
        self.STREAMS[num] = stream
        return stream

    def delete(self, n: int) -> bool:
        stream = self.STREAMS.get(n, None)
        if stream is not None:
            del self.STREAMS[stream.n]
            return True
        return False

    def lookup_stream(self, n=None) -> Optional["Stream"]:
        if n is None:
            return None
        return self.STREAMS.get(n, None)

    @property
    def next(self):
        numbers = [stream.n for stream in self.STREAMS.values()] + [2]
        return max(numbers) + 1


stream_manager = StreamsManager()


class Stream(object):
    """
    Opens a stream

    This can be used in a context_manager like this:

    with Stream(pypath, "r") as f:
         ...

    However see mathics_open which wraps this
    """

    def __init__(self, name: str, mode="r", encoding=None, io=None, channel_num=None):
        if channel_num is None:
            channel_num = stream_manager.next
        if mode is None:
            mode = "r"
        self.name = name
        self.mode = mode
        self.encoding = encoding
        self.io = io
        self.n = channel_num

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
        fp = io_open(path, self.mode, encoding=encoding)
        stream_manager.add(name=path, mode=self.mode, encoding=encoding, io=fp)
        return fp

    def __exit__(self, type, value, traceback):
        if self.io is not None:
            self.io.close()
        # Leave around self.io so we can call closed() to query its status.
        stream_manager.delete(self.n)


stream_manager.add("stdin", mode="r", num=0, io=sys.stdin)
stream_manager.add("stdout", mode="w", num=1, io=sys.stdout)
stream_manager.add("stderr", mode="w", num=2, io=sys.stderr)
