"""
Functions to support Read[]
"""

def read_check_options(options: dict) -> dict:
    # Options
    # TODO Proper error messages

    result = {}
    keys = list(options.keys())

    # AnchoredSearch
    if "System`AnchoredSearch" in keys:
        anchored_search = options["System`AnchoredSearch"].to_python()
        assert anchored_search in [True, False]
        result["AnchoredSearch"] = anchored_search

    # IgnoreCase
    if "System`IgnoreCase" in keys:
        ignore_case = options["System`IgnoreCase"].to_python()
        assert ignore_case in [True, False]
        result["IgnoreCase"] = ignore_case

    # WordSearch
    if "System`WordSearch" in keys:
        word_search = options["System`WordSearch"].to_python()
        assert word_search in [True, False]
        result["WordSearch"] = word_search

    # RecordSeparators
    if "System`RecordSeparators" in keys:
        record_separators = options["System`RecordSeparators"].to_python()
        assert isinstance(record_separators, list)
        assert all(
            isinstance(s, str) and s[0] == s[-1] == '"' for s in record_separators
        )
        record_separators = [s[1:-1] for s in record_separators]
        result["RecordSeparators"] = record_separators

    # WordSeparators
    if "System`WordSeparators" in keys:
        word_separators = options["System`WordSeparators"].to_python()
        assert isinstance(word_separators, list)
        assert all(
            isinstance(s, str) and s[0] == s[-1] == '"' for s in word_separators
        )
        word_separators = [s[1:-1] for s in word_separators]
        result["WordSeparators"] = word_separators

    # NullRecords
    if "System`NullRecords" in keys:
        null_records = options["System`NullRecords"].to_python()
        assert null_records in [True, False]
        result["NullRecords"] = null_records

    # NullWords
    if "System`NullWords" in keys:
        null_words = options["System`NullWords"].to_python()
        assert null_words in [True, False]
        result["NullWords"] = null_words

    # TokenWords
    if "System`TokenWords" in keys:
        token_words = options["System`TokenWords"].to_python()
        assert token_words == []
        result["TokenWords"] = token_words

    return result

def read_get_separators(options, name):
    # Options
    # TODO Implement extra options
    py_options = read_check_options(options)
    # null_records = py_options['NullRecords']
    # null_words = py_options['NullWords']
    record_separators = py_options["RecordSeparators"]
    # token_words = py_options['TokenWords']
    word_separators = py_options["WordSeparators"]

    py_name = name.to_python()
    return record_separators, word_separators, py_name

def reader(stream, word_separators, evaluation, accepted=None):
    while True:
        word = ""
        while True:
            try:
                tmp = stream.io.read(1)
            except UnicodeDecodeError:
                tmp = " "  # ignore
                evaluation.message("General", "ucdec")

            if tmp == "":
                if word == "":
                    pos = stream.io.tell()
                    newchar = stream.io.read(1)
                    if pos == stream.io.tell():
                        raise EOFError
                    else:
                        if newchar:
                            word = newchar
                            continue
                        else:
                            yield word
                            continue
                last_word = word
                word = ""
                yield last_word
                break

            if tmp in word_separators:
                if word == "":
                    continue
                if stream.io.seekable():
                    # stream.io.seek(-1, 1) #Python3
                    stream.io.seek(stream.io.tell() - 1)
                last_word = word
                word = ""
                yield last_word
                break

            if accepted is not None and tmp not in accepted:
                last_word = word
                word = ""
                yield last_word
                break

            word += tmp
