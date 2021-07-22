# -*- coding: utf-8 -*-
"""A module and library that assists in organizing document data
previously obtained from static files and Python module/class doc
strings. This data is stored in a way that facilitates:

* organizing information to produce a LaTeX file
* running documentation tests
* producing HTML-based documentation

The command-line utility `docpipeline.py`, which loads the data from
Python modules and static files, accesses functions here.

Mathics-core routines also use this to get usage strings of Mathics
Built-in functions.

Mathics Django also uses this library for its HTML-based documentation.

As with reading in data, final assembly to a LateX file or running
documentation tests is done elsewhere.

FIXME: too much of this code is duplicated in Django. Code should
be moved for both to a separate package.

More importantly, this code should be replaced by Sphinx and autodoc.
Things are such a mess, that it is too difficult to contemplate this right now.

"""

import importlib
import pkgutil
import re

from os import getenv, listdir
from types import ModuleType

from mathics import builtin
from mathics import settings
from mathics.builtin import get_module_doc
from mathics.core.evaluation import Message, Print
from mathics.doc.utils import slugify

# These regular expressions pull out information from docstring or text in a file.
CHAPTER_RE = re.compile('(?s)<chapter title="(.*?)">(.*?)</chapter>')
SECTION_RE = re.compile('(?s)(.*?)<section title="(.*?)">(.*?)</section>')
SUBSECTION_RE = re.compile('(?s)<subsection title="(.*?)">')
SUBSECTION_END_RE = re.compile("</subsection>")

TESTCASE_RE = re.compile(
    r"""(?mx)^  # re.MULTILINE (multi-line match) and re.VERBOSE (readable regular expressions
        ((?:.|\n)*?)
        ^\s+([>#SX])>[ ](.*)  # test-code indicator
        ((?:\n\s*(?:[:|=.][ ]|\.).*)*)  # test-code results"""
)
TESTCASE_OUT_RE = re.compile(r"^\s*([:|=])(.*)$")

MATHICS_RE = re.compile(r"(?<!\\)\'(.*?)(?<!\\)\'")

# Preserve space before and after in-line code variables.
LATEX_RE = re.compile(r"(\s?)\$(\w+?)\$(\s?)")

DL_RE = re.compile(r"(?s)<dl>(.*?)</dl>")
DL_ITEM_RE = re.compile(
    r"(?s)<(?P<tag>d[td])>(?P<content>.*?)(?:</(?P=tag)>|)\s*(?:(?=<d[td]>)|$)"
)
LIST_RE = re.compile(r"(?s)<(?P<tag>ul|ol)>(?P<content>.*?)</(?P=tag)>")
LIST_ITEM_RE = re.compile(r"(?s)<li>(.*?)(?:</li>|(?=<li>)|$)")
CONSOLE_RE = re.compile(r"(?s)<(?P<tag>con|console)>(?P<content>.*?)</(?P=tag)>")
ITALIC_RE = re.compile(r"(?s)<(?P<tag>i)>(?P<content>.*?)</(?P=tag)>")
IMG_RE = re.compile(
    r'<img src="(?P<src>.*?)" title="(?P<title>.*?)" label="(?P<label>.*?)">'
)
IMG_PNG_RE = re.compile(
    r'<imgpng src="(?P<src>.*?)" title="(?P<title>.*?)" label="(?P<label>.*?)">'
)
REF_RE = re.compile(r'<ref label="(?P<label>.*?)">')
PYTHON_RE = re.compile(r"(?s)<python>(.*?)</python>")
LATEX_CHAR_RE = re.compile(r"(?<!\\)(\^)")

QUOTATIONS_RE = re.compile(r"\"([\w\s,]*?)\"")
HYPERTEXT_RE = re.compile(r"(?s)<(?P<tag>em|url)>(?P<content>.*?)</(?P=tag)>")

OUTSIDE_ASY_RE = re.compile(r"(?s)((?:^|\\end\{asy\}).*?(?:$|\\begin\{asy\}))")
LATEX_TEXT_RE = re.compile(
    r"(?s)\\text\{([^{}]*?(?:[^{}]*?\{[^{}]*?(?:[^{}]*?\{[^{}]*?\}[^{}]*?)*?"
    r"[^{}]*?\}[^{}]*?)*?[^{}]*?)\}"
)
LATEX_TESTOUT_RE = re.compile(
    r"(?s)\\begin\{(?P<tag>testmessage|testprint|testresult)\}"
    r"(?P<content>.*?)\\end\{(?P=tag)\}"
)
LATEX_TESTOUT_DELIM_RE = re.compile(r",")
NUMBER_RE = re.compile(r"(\d*(?<!\.)\.\d+|\d+\.(?!\.)\d*|\d+)")
LATEX_ARRAY_RE = re.compile(
    r"(?s)\\begin\{testresult\}\\begin\{array\}\{l\}(.*?)"
    r"\\end\{array\}\\end\{testresult\}"
)
LATEX_INLINE_END_RE = re.compile(r"(?s)(?P<all>\\lstinline'[^']*?'\}?[.,;:])")
LATEX_CONSOLE_RE = re.compile(r"\\console\{(.*?)\}")

# These are all of the XML/HTML-like tags that documentation supports.
ALLOWED_TAGS = (
    "dl",
    "dd",
    "dt",
    "em",
    "url",
    "ul",
    "i",
    "ol",
    "li",
    "con",
    "console",
    "img",
    "imgpng",
    "ref",
    "subsection",
)
ALLOWED_TAGS_RE = dict(
    (allowed, re.compile("&lt;(%s.*?)&gt;" % allowed)) for allowed in ALLOWED_TAGS
)

SPECIAL_COMMANDS = {
    "LaTeX": (r"<em>LaTeX</em>", r"\LaTeX{}"),
    "Mathematica": (
        r"<em>Mathematica</em>&reg;",
        r"\emph{Mathematica}\textregistered{}",
    ),
    "Mathics": (r"<em>Mathics</em>", r"\emph{Mathics}"),
    "Sage": (r"<em>Sage</em>", r"\emph{Sage}"),
    "Wolfram": (r"<em>Wolfram</em>", r"\emph{Wolfram}"),
    "skip": (r"<br /><br />", r"\bigskip"),
}


# Used for getting test results by test expresson and chapter/section information.
test_result_map = {}


def get_results_by_test(test_expr: str, full_test_key: list, doc_data: dict) -> list:
    """
    Sometimes test numbering is off, either due to bugs or changes since the
    data was read.

    Here, we compensate for this by looking up the test by its chapter and section name
    portion stored in `full_test_key` along with the and the test expresion data
    stored in `test_expr`.

    This new key is looked up in `test_result_map` its value is returned.

    `doc_data` is only first time this is called to populate `test_result_map`.
    """

    # Strip off the test index form new key with this and the test string.
    # Add to any existing value for that "result". This is now what we want to
    # use as a tee in test_result_map to look for.
    test_section = list(full_test_key)[:-1]
    search_key = tuple(test_section)

    if not test_result_map:
        # Populate test_result_map from doc_data
        for key, result in doc_data.items():
            test_section = list(key)[:-1]
            new_test_key = tuple(test_section)
            next_result = test_result_map.get(new_test_key, None)
            if next_result:
                next_result.append(result)
            else:
                next_result = [result]
            test_result_map[new_test_key] = next_result

    results = test_result_map.get(search_key, None)
    result = {}
    if results:
        for result_candidate in results:
            if result_candidate["query"] == test_expr:
                if result:
                    # Already found something
                    print(f"Warning, multiple results appear under {search_key}.")
                    return {}
                else:
                    result = result_candidate

    return result


def get_submodule_names(object) -> list:
    """Many builtins are organized into modules which, from a documentation
    standpoint, are like Mathematica Online Guide Docs.

    "List Functions", "Colors", or "Distance and Similarity Measures"
    are some examples Guide Documents group group various Bultin Functions,
    under submodules relate to that general classification.

    Here, we want to return a list of the Python modules under a "Guide Doc"
    module.

    As an example of a "Guide Doc" and its submodules, consider the
    module named mathics.builtin.colors. It collects code and documentation pertaining
    to the builtin functions that would be found in the Guide documenation for "Colors".

    The `mathics.builtin.colors` module has a submodule
    `mathics.builtin.colors.named_colors`.

    The builtin functions defined in `named_colors` then are those found in the
    "Named Colors" group of the "Colors" Guide Doc.

    So in this example then, in the list the modules returned for
    Python module `mathics.builtin.colors` would be the
    `mathics.builtin.colors.named_colors` module which contains the
    definition and docs for the "Named Colors" Mathics Bultin
    Functions.
    """
    modpkgs = []
    if hasattr(object, "__path__"):
        for importer, modname, ispkg in pkgutil.iter_modules(object.__path__):
            modpkgs.append(modname)
        modpkgs.sort()
    return modpkgs


def filter_comments(doc: str) -> str:
    """Remove docstring documentation comments. These are lines
    that start with ##"""
    return "\n".join(
        line for line in doc.splitlines() if not line.lstrip().startswith("##")
    )


def strip_system_prefix(name):
    if name.startswith("System`"):
        stripped_name = name[len("System`") :]
        # don't return Private`sym for System`Private`sym
        if "`" not in stripped_name:
            return stripped_name
    return name


def get_latex_escape_char(text):
    for escape_char in ("'", "~", "@"):
        if escape_char not in text:
            return escape_char
    raise ValueError


def _replace_all(text, pairs):
    for (i, j) in pairs:
        text = text.replace(i, j)
    return text


def escape_latex_output(text):
    "Escape Mathics output"

    text = _replace_all(
        text,
        [
            ("\\", "\\\\"),
            ("{", "\\{"),
            ("}", "\\}"),
            ("~", "\\~"),
            ("&", "\\&"),
            ("%", "\\%"),
            ("$", r"\$"),
            ("_", "\\_"),
        ],
    )
    return text


def escape_latex_code(text):
    "Escape verbatim Mathics input"

    text = escape_latex_output(text)
    escape_char = get_latex_escape_char(text)
    return "\\lstinline%s%s%s" % (escape_char, text, escape_char)


def escape_latex(text):
    "Escape documentation text"

    def repl_python(match):
        return (
            r"""\begin{lstlisting}[style=python]
%s
\end{lstlisting}"""
            % match.group(1).strip()
        )

    text, post_substitutions = pre_sub(PYTHON_RE, text, repl_python)

    text = _replace_all(
        text,
        [
            ("\\", "\\\\"),
            ("{", "\\{"),
            ("}", "\\}"),
            ("~", "\\~{ }"),
            ("&", "\\&"),
            ("%", "\\%"),
            ("#", "\\#"),
        ],
    )

    def repl(match):
        text = match.group(1)
        if text:
            text = _replace_all(text, [("\\'", "'"), ("^", "\\^")])
            escape_char = get_latex_escape_char(text)
            text = LATEX_RE.sub(
                lambda m: "%s%s\\codevar{\\textit{%s}}%s\\lstinline%s"
                % (escape_char, m.group(1), m.group(2), m.group(3), escape_char),
                text,
            )
            if text.startswith(" "):
                text = r"\ " + text[1:]
            if text.endswith(" "):
                text = text[:-1] + r"\ "
            return "\\code{\\lstinline%s%s%s}" % (escape_char, text, escape_char)
        else:
            # treat double '' literaly
            return "''"

    text = MATHICS_RE.sub(repl, text)

    text = LATEX_RE.sub(
        lambda m: "%s\\textit{%s}%s" % (m.group(1), m.group(2), m.group(3)), text
    )

    text = text.replace("\\\\'", "'")

    def repl_dl(match):
        text = match.group(1)
        text = DL_ITEM_RE.sub(
            lambda m: "\\%(tag)s{%(content)s}\n" % m.groupdict(), text
        )
        return "\\begin{definitions}%s\\end{definitions}" % text

    text = DL_RE.sub(repl_dl, text)

    def repl_list(match):
        tag = match.group("tag")
        content = match.group("content")
        content = LIST_ITEM_RE.sub(lambda m: "\\item %s\n" % m.group(1), content)
        env = "itemize" if tag == "ul" else "enumerate"
        return "\\begin{%s}%s\\end{%s}" % (env, content, env)

    text = LIST_RE.sub(repl_list, text)

    # FIXME: get this from MathicsScanner
    text = _replace_all(
        text,
        [
            ("$", r"\$"),
            ("\00f1", r"\~n"),
            ("\u00e7", r"\c{c}"),
            ("\u00e9", r"\'e"),
            ("\u00ea", r"\^e"),
            ("\u03b3", r"$\gamma$"),
            ("\u03b8", r"$\theta$"),
            ("\u03bc", r"$\mu$"),
            ("\u03c0", r"$\pi$"),
            ("\u2107", r"$\mathrm{e}$"),
            ("\u222b", r"\int"),
            ("\u2243", r"$\simeq$"),
            ("\u2026", r"$\dots$"),
            ("\u2260", r"$\ne$"),
            ("\u2264", r"$\le$"),
            ("\u2265", r"$\ge$"),
            ("\u22bb", r"$\oplus$"),  # The WL veebar-looking symbol isn't in AMSLaTeX
            ("\u22bc", r"$\barwedge$"),
            ("\u22bd", r"$\veebar$"),
            ("\u21d2", r"$\Rightarrow$"),
            ("\uf74c", r"d"),
        ],
    )

    def repl_char(match):
        char = match.group(1)
        return {
            "^": "$^\\wedge$",
        }[char]

    text = LATEX_CHAR_RE.sub(repl_char, text)

    def repl_img(match):
        src = match.group("src")
        title = match.group("title")
        label = match.group("label")
        return r"""\begin{figure*}[htp]
\centering
\includegraphics[width=\textwidth]{images/%(src)s}
\caption{%(title)s}
\label{%(label)s}
\end{figure*}""" % {
            "src": src,
            "title": title,
            "label": label,
        }

    text = IMG_RE.sub(repl_img, text)

    def repl_imgpng(match):
        src = match.group("src")
        return r"\includegraphics[scale=1.0]{images/%(src)s}" % {"src": src}

    text = IMG_PNG_RE.sub(repl_imgpng, text)

    def repl_ref(match):
        return r"figure \ref{%s}" % match.group("label")

    text = REF_RE.sub(repl_ref, text)

    def repl_quotation(match):
        return r"``%s''" % match.group(1)

    def repl_hypertext(match):
        tag = match.group("tag")
        content = match.group("content")
        if tag == "em":
            return r"\emph{%s}" % content
        elif tag == "url":
            return "\\url{%s}" % content

    text = QUOTATIONS_RE.sub(repl_quotation, text)
    text = HYPERTEXT_RE.sub(repl_hypertext, text)

    def repl_console(match):
        tag = match.group("tag")
        content = match.group("content")
        content = content.strip()
        content = content.replace(r"\$", "$")
        if tag == "con":
            return "\\console{%s}" % content
        else:
            return "\\begin{lstlisting}\n%s\n\\end{lstlisting}" % content

    text = CONSOLE_RE.sub(repl_console, text)

    def repl_italic(match):
        content = match.group("content")
        return "\\emph{%s}" % content

    text = ITALIC_RE.sub(repl_italic, text)

    # def repl_asy(match):
    #     """
    #     Ensure \begin{asy} and \end{asy} are on their own line,
    #     but there shall be no extra empty lines
    #     """
    #     #tag = match.group(1)
    #     #return '\n%s\n' % tag
    #     #print "replace"
    #     return '\\end{asy}\n\\begin{asy}'
    # text = LATEX_BETWEEN_ASY_RE.sub(repl_asy, text)

    def repl_subsection(match):
        return "\n\\subsection*{%s}\n" % match.group(1)

    text = SUBSECTION_RE.sub(repl_subsection, text)
    text = SUBSECTION_END_RE.sub("", text)

    for key, (xml, tex) in SPECIAL_COMMANDS.items():
        # "\" has been escaped already => 2 \
        text = text.replace("\\\\" + key, tex)

    text = post_sub(text, post_substitutions)

    return text


def get_doc_name_from_module(module):
    name = "???"
    if module.__doc__:
        lines = module.__doc__.strip()
        if not lines:
            name = module.__name__
        else:
            name = lines.split("\n")[0]
    return name


def post_process_latex(result):
    """
    Some post-processing hacks of generated LaTeX code to handle linebreaks
    """

    WORD_SPLIT_RE = re.compile(r"(\s+|\\newline\s*)")

    def wrap_word(word):
        if word.strip() == r"\newline":
            return word
        return r"\text{%s}" % word

    def repl_text(match):
        text = match.group(1)
        if not text:
            return r"\text{}"
        words = WORD_SPLIT_RE.split(text)
        assert len(words) >= 1
        if len(words) > 1:
            text = ""
            index = 0
            while index < len(words) - 1:
                text += "%s%s\\allowbreak{}" % (
                    wrap_word(words[index]),
                    wrap_word(words[index + 1]),
                )
                index += 2
            text += wrap_word(words[-1])
        else:
            text = r"\text{%s}" % words[0]
        if not text:
            return r"\text{}"
        text = text.replace("><", r">}\allowbreak\text{<")
        return text

    def repl_out_delim(match):
        return ",\\allowbreak{}"

    def repl_number(match):
        guard = r"\allowbreak{}"
        inter_groups_pre = r"\,\discretionary{\~{}}{\~{}}{}"
        inter_groups_post = r"\discretionary{\~{}}{\~{}}{}"
        number = match.group(1)
        parts = number.split(".")
        if len(number) <= 3:
            return number
        assert 1 <= len(parts) <= 2
        pre_dec = parts[0]
        groups = []
        while pre_dec:
            groups.append(pre_dec[-3:])
            pre_dec = pre_dec[:-3]
        pre_dec = inter_groups_pre.join(reversed(groups))
        if len(parts) == 2:
            post_dec = parts[1]
            groups = []
            while post_dec:
                groups.append(post_dec[:3])
                post_dec = post_dec[3:]
            post_dec = inter_groups_post.join(groups)
            result = pre_dec + "." + post_dec
        else:
            result = pre_dec
        return guard + result + guard

    def repl_array(match):
        content = match.group(1)
        lines = content.split("\\\\")
        content = "".join(
            r"\begin{dmath*}%s\end{dmath*}" % line for line in lines if line.strip()
        )
        return r"\begin{testresultlist}%s\end{testresultlist}" % content

    def repl_out(match):
        tag = match.group("tag")
        content = match.group("content")
        content = LATEX_TESTOUT_DELIM_RE.sub(repl_out_delim, content)
        content = NUMBER_RE.sub(repl_number, content)
        content = content.replace(r"\left[", r"\left[\allowbreak{}")
        return "\\begin{%s}%s\\end{%s}" % (tag, content, tag)

    def repl_inline_end(match):
        "Prevent linebreaks between inline code and sentence delimeters"

        code = match.group("all")
        if code[-2] == "}":
            code = code[:-2] + code[-1] + code[-2]
        return r"\mbox{%s}" % code

    def repl_console(match):
        code = match.group(1)
        code = code.replace("/", r"/\allowbreak{}")
        return r"\console{%s}" % code

    def repl_nonasy(match):
        result = match.group(1)
        result = LATEX_TEXT_RE.sub(repl_text, result)
        result = LATEX_TESTOUT_RE.sub(repl_out, result)
        result = LATEX_ARRAY_RE.sub(repl_array, result)
        result = LATEX_INLINE_END_RE.sub(repl_inline_end, result)
        result = LATEX_CONSOLE_RE.sub(repl_console, result)
        return result

    return OUTSIDE_ASY_RE.sub(repl_nonasy, result)


POST_SUBSTITUTION_TAG = "_POST_SUBSTITUTION%d_"


def pre_sub(re, text, repl_func):
    post_substitutions = []

    def repl_pre(match):
        repl = repl_func(match)
        index = len(post_substitutions)
        post_substitutions.append(repl)
        return POST_SUBSTITUTION_TAG % index

    text = re.sub(repl_pre, text)

    return text, post_substitutions


def post_sub(text, post_substitutions):
    for index, sub in enumerate(post_substitutions):
        text = text.replace(POST_SUBSTITUTION_TAG % index, sub)
    return text


class Tests(object):
    # FIXME: add optional guide section
    def __init__(self, part: str, chapter: str, section: str, doctests):
        self.part, self.chapter = part, chapter
        self.section, self.tests = section, doctests


class Documentation(object):
    def __str__(self):
        return "\n\n\n".join(str(part) for part in self.parts)

    def get_part(self, part_slug):
        return self.parts_by_slug.get(part_slug)

    def get_chapter(self, part_slug, chapter_slug):
        part = self.parts_by_slug.get(part_slug)
        if part:
            return part.chapters_by_slug.get(chapter_slug)
        return None

    def get_section(self, part_slug, chapter_slug, section_slug):
        part = self.parts_by_slug.get(part_slug)
        if part:
            chapter = part.chapters_by_slug.get(chapter_slug)
            if chapter:
                return chapter.sections_by_slug.get(section_slug)
        return None

    def get_subsection(self, part_slug, chapter_slug, section_slug, subsection_slug):
        part = self.parts_by_slug.get(part_slug)
        if part:
            chapter = part.chapters_by_slug.get(chapter_slug)
            if chapter:
                section = chapter.sections_by_slug.get(section_slug)
                if section:
                    return section.subsections_by_slug.get(subsection_slug)

        return None

    def get_tests(self):
        for part in self.parts:
            for chapter in part.chapters:
                tests = chapter.doc.get_tests()
                if tests:
                    yield Tests(part.title, chapter.title, "", tests)
                for section in chapter.all_sections:
                    if section.installed:
                        if isinstance(section, DocGuideSection):
                            for docsection in section.subsections:
                                for docsubsection in docsection.subsections:
                                    # FIXME: Something is weird here where tests for subsection items
                                    # appear not as a collection but individually and need to be
                                    # iterated below. Probably some other code is faulty and
                                    # when fixed the below loop and collection into doctest_list[]
                                    # will be removed.
                                    doctest_list = []
                                    index = 1
                                    for doctests in docsubsection.items:
                                        doctest_list += list(doctests.get_tests())
                                        for test in doctest_list:
                                            test.index = index
                                            index += 1

                                    if doctest_list:
                                        yield Tests(
                                            section.chapter.part.title,
                                            section.chapter.title,
                                            docsubsection.title,
                                            doctest_list,
                                        )
                        else:
                            tests = section.doc.get_tests()
                            if tests:
                                yield Tests(
                                    part.title, chapter.title, section.title, tests
                                )
                                pass
                            pass
                        pass
                    pass
                pass
            pass
        return

    def latex(self, doc_data: dict, quiet=False) -> str:
        """Render self as a LaTeX string and return that.

        `output` is not used here but passed along to the bottom-most
        level in getting expected test results.
        """
        parts = []
        appendix = False
        for part in self.parts:
            text = part.latex(doc_data, quiet)
            if part.is_appendix and not appendix:
                appendix = True
                text = "\n\\appendix\n" + text
            parts.append(text)
        result = "\n\n".join(parts)
        result = post_process_latex(result)
        return result


class MathicsMainDocumentation(Documentation):
    def __init__(self):
        self.doc_dir = settings.DOC_DIR
        self.latex_file = settings.DOC_LATEX_FILE
        self.parts = []
        self.parts_by_slug = {}
        self.pymathics_doc_loaded = False
        self.doc_data_file = settings.DOC_DATA_PATH
        self.title = "Overview"
        files = listdir(self.doc_dir)
        files.sort()
        appendix = []

        for file in files:
            part_title = file[2:]
            if part_title.endswith(".mdoc"):
                part_title = part_title[: -len(".mdoc")]
                part = DocPart(self, part_title)
                text = open(self.doc_dir + file, "rb").read().decode("utf8")
                text = filter_comments(text)
                chapters = CHAPTER_RE.findall(text)
                for title, text in chapters:
                    chapter = DocChapter(part, title)
                    text += '<section title=""></section>'
                    sections = SECTION_RE.findall(text)
                    for pre_text, title, text in sections:
                        if title:
                            section = DocSection(
                                chapter, title, text, operator=None, installed=True
                            )
                            chapter.sections.append(section)
                            subsections = SUBSECTION_RE.findall(text)
                            for subsection_title in subsections:
                                subsection = DocSubsection(
                                    chapter,
                                    section,
                                    subsection_title,
                                    text,
                                )
                                section.subsections.append(subsection)
                                pass
                            pass
                        else:
                            section = None
                        if not chapter.doc:
                            chapter.doc = XMLDoc(pre_text, title, section)

                    part.chapters.append(chapter)
                if file[0].isdigit():
                    self.parts.append(part)
                else:
                    part.is_appendix = True
                    appendix.append(part)

        for title, modules, builtins_by_module, start in [
            (
                "Reference of Built-in Symbols",
                builtin.modules,
                builtin.builtins_by_module,
                True,
            )
        ]:  # nopep8
            # ("Reference of optional symbols", optional.modules,
            #  optional.optional_builtins_by_module, False)]:

            builtin_part = DocPart(self, title, is_reference=start)
            modules_seen = set([])
            for module in modules:
                # FIXME add an additional mechanism in the module
                # to allow a docstring and indicate it is not to go in the
                # user manual
                # Note: this code assumes that all chapters with sections/doctests in them
                # are documented (as it should be)!
                if module.__doc__ is None:
                    continue
                if module in modules_seen:
                    continue
                title, text = get_module_doc(module)
                chapter = DocChapter(builtin_part, title, XMLDoc(text, title, None))
                builtins = builtins_by_module[module.__name__]
                # FIXME: some Box routines, like RowBox *are*
                # documented
                sections = [
                    builtin
                    for builtin in builtins
                    if not builtin.__class__.__name__.endswith("Box")
                ]
                if module.__file__.endswith("__init__.py"):
                    # We have a Guide Section.
                    name = get_doc_name_from_module(module)
                    guide_section = self.add_section(
                        chapter, name, module, operator=None, is_guide=True
                    )
                    submodules = [
                        value
                        for value in module.__dict__.values()
                        if isinstance(value, ModuleType)
                    ]

                    # Add sections in the guide section...
                    for submodule in submodules:
                        # FIXME add an additional mechanism in the module
                        # to allow a docstring and indicate it is not to go in the
                        # user manual
                        if submodule.__doc__ is None:
                            continue
                        if submodule in modules_seen:
                            continue

                        section = self.add_section(
                            chapter,
                            get_doc_name_from_module(submodule),
                            submodule,
                            operator=None,
                            is_guide=False,
                            in_guide=True,
                        )
                        modules_seen.add(submodule)
                        guide_section.subsections.append(section)

                        builtins = builtins_by_module[submodule.__name__]
                        subsections = [
                            builtin
                            for builtin in builtins
                            if not builtin.__class__.__name__.endswith("Box")
                        ]
                        for instance in subsections:
                            modules_seen.add(instance)
                            name = instance.get_name(short=True)
                            self.add_subsection(
                                chapter,
                                section,
                                instance.get_name(short=True),
                                instance,
                                instance.get_operator(),
                                in_guide=True,
                            )
                else:
                    for instance in sections:
                        if instance not in modules_seen:
                            name = instance.get_name(short=True)
                            self.add_section(
                                chapter,
                                instance.get_name(short=True),
                                instance,
                                instance.get_operator(),
                                is_guide=False,
                                in_guide=False,
                            )
                            modules_seen.add(instance)
                            pass
                        pass
                    pass
                builtin_part.chapters.append(chapter)
            self.parts.append(builtin_part)

        for part in appendix:
            self.parts.append(part)

        # set keys of tests
        for tests in self.get_tests():
            for test in tests.tests:
                test.key = (tests.part, tests.chapter, tests.section, test.index)

    def add_section(
        self,
        chapter,
        section_name: str,
        section_object,
        operator,
        is_guide: bool = False,
        in_guide: bool = False,
    ):
        """
        Adds a DocSection or DocGuideSection
        object to the chapter, a DocChapter object.
        "section_object" is either a Python module or a Class object instance.
        """
        installed = True
        for package in getattr(section_object, "requires", []):
            try:
                importlib.import_module(package)
            except ImportError:
                installed = False
                break
        # FIXME add an additional mechanism in the module
        # to allow a docstring and indicate it is not to go in the
        # user manual
        if not section_object.__doc__:
            return
        if is_guide:
            section = DocGuideSection(
                chapter,
                section_name,
                section_object.__doc__,
                section_object,
                installed=installed,
            )
            chapter.guide_sections.append(section)
        else:
            section = DocSection(
                chapter,
                section_name,
                section_object.__doc__,
                operator=operator,
                installed=installed,
                in_guide=in_guide,
            )
            chapter.sections.append(section)

        return section

    def add_subsection(
        self,
        chapter,
        section,
        subsection_name: str,
        instance,
        operator=None,
        in_guide=False,
    ):
        installed = True
        for package in getattr(instance, "requires", []):
            try:
                importlib.import_module(package)
            except ImportError:
                installed = False
                break

        # FIXME add an additional mechanism in the module
        # to allow a docstring and indicate it is not to go in the
        # user manual
        if not instance.__doc__:
            return
        subsection = DocSubsection(
            chapter,
            section,
            subsection_name,
            instance.__doc__,
            operator=operator,
            installed=installed,
            in_guide=in_guide,
        )
        section.subsections.append(subsection)

    def load_pymathics_doc(self):
        if self.pymathics_doc_loaded:
            return
        from mathics.settings import default_pymathics_modules

        pymathicspart = None
        # Look the "Pymathics Modules" part, and if it does not exist, create it.
        for part in self.parts:
            if part.title == "Pymathics Modules":
                pymathicspart = part
        if pymathicspart is None:
            pymathicspart = DocPart(self, "Pymathics Modules", is_reference=True)
            self.parts.append(pymathicspart)

        # For each module, create the documentation object and load the chapters in the pymathics part.
        for pymmodule in default_pymathics_modules:
            pymathicsdoc = PyMathicsDocumentation(pymmodule)
            for part in pymathicsdoc.parts:
                for ch in part.chapters:
                    ch.title = f"{pymmodule} {part.title} {ch.title}"
                    ch.part = pymathicspart
                    pymathicspart.chapters_by_slug[ch.slug] = ch
                    pymathicspart.chapters.append(ch)

        self.pymathics_doc_loaded = True


class PyMathicsDocumentation(Documentation):
    def __init__(self, module=None):
        self.title = "Overview"
        self.parts = []
        self.parts_by_slug = {}
        self.doc_dir = None
        self.doc_data_file = None
        self.latex_file = None
        self.symbols = {}
        if module is None:
            return

        import importlib

        # Load the module and verifies it is a pymathics module
        try:
            self.pymathicsmodule = importlib.import_module(module)
        except ImportError:
            print("Module does not exist")
            self.pymathicsmodule = None
            self.parts = []
            return

        try:
            if "name" in self.pymathicsmodule.pymathics_version_data:
                self.name = self.version = self.pymathicsmodule.pymathics_version_data[
                    "name"
                ]
            else:
                self.name = (self.pymathicsmodule.__package__)[10:]
            self.version = self.pymathicsmodule.pymathics_version_data["version"]
            self.author = self.pymathicsmodule.pymathics_version_data["author"]
        except (AttributeError, KeyError, IndexError):
            print(module + " is not a pymathics module.")
            self.pymathicsmodule = None
            self.parts = []
            return

        # Paths
        self.doc_dir = self.pymathicsmodule.__path__[0] + "/doc/"
        self.doc_data_file = self.doc_dir + "tex/data"
        self.latex_file = self.doc_dir + "tex/documentation.tex"

        # Load the dictionary of mathics symbols defined in the module
        self.symbols = {}
        from mathics.builtin import is_builtin, Builtin

        print("loading symbols")
        for name in dir(self.pymathicsmodule):
            var = getattr(self.pymathicsmodule, name)
            if (
                hasattr(var, "__module__")
                and var.__module__ != "mathics.builtin.base"
                and is_builtin(var)
                and not name.startswith("_")
                and var.__module__[: len(self.pymathicsmodule.__name__)]
                == self.pymathicsmodule.__name__
            ):  # nopep8
                instance = var(expression=False)
                if isinstance(instance, Builtin):
                    self.symbols[instance.get_name()] = instance
        # Defines de default first part, in case we are building an independent documentation module.
        self.title = "Overview"
        self.parts = []
        self.parts_by_slug = {}
        try:
            files = listdir(self.doc_dir)
            files.sort()
        except FileNotFoundError:
            self.doc_dir = ""
            self.doc_data_file = ""
            self.latex_file = ""
            files = []
        appendix = []
        for file in files:
            part_title = file[2:]
            if part_title.endswith(".mdoc"):
                part_title = part_title[: -len(".mdoc")]
                part = DocPart(self, part_title)
                text = open(self.doc_dir + file, "rb").read().decode("utf8")
                text = filter_comments(text)
                chapters = CHAPTER_RE.findall(text)
                for title, text in chapters:
                    chapter = DocChapter(part, title)
                    text += '<section title=""></section>'
                    sections = SECTION_RE.findall(text)
                    for pre_text, title, text in sections:
                        if not chapter.doc:
                            chapter.doc = XMLDoc(pre_text)
                        if title:
                            section = DocSection(chapter, title, text)
                            chapter.sections.append(section)
                    part.chapters.append(chapter)
                if file[0].isdigit():
                    self.parts.append(part)
                else:
                    part.is_appendix = True
                    appendix.append(part)

        # Builds the automatic documentation
        builtin_part = DocPart(self, "Pymathics Modules", is_reference=True)
        title, text = get_module_doc(self.pymathicsmodule)
        chapter = DocChapter(builtin_part, title, XMLDoc(text))
        for name in self.symbols:
            instance = self.symbols[name]
            installed = True
            for package in getattr(instance, "requires", []):
                try:
                    importlib.import_module(package)
                except ImportError:
                    installed = False
                    break
            section = DocSection(
                chapter,
                strip_system_prefix(name),
                instance.__doc__ or "",
                operator=instance.get_operator(),
                installed=installed,
            )
            chapter.sections.append(section)
        builtin_part.chapters.append(chapter)
        self.parts.append(builtin_part)
        # Adds possible appendices
        for part in appendix:
            self.parts.append(part)

        # set keys of tests
        for tests in self.get_tests():
            for test in tests.tests:
                test.key = (tests.part, tests.chapter, tests.section, test.index)


class DocPart(object):
    def __init__(self, doc, title, is_reference=False):
        self.doc = doc
        self.title = title
        self.slug = slugify(title)
        self.chapters = []
        self.chapters_by_slug = {}
        self.is_reference = is_reference
        self.is_appendix = False
        doc.parts_by_slug[self.slug] = self

    def __str__(self):
        return "%s\n\n%s" % (
            self.title,
            "\n".join(str(chapter) for chapter in self.chapters),
        )

    def latex(self, doc_data: dict, quiet=False) -> str:
        """Render this Part object as LaTeX string and return that.

        `output` is not used here but passed along to the bottom-most
        level in getting expected test results.
        """
        result = "\n\n\\part{%s}\n\n" % escape_latex(self.title) + (
            "\n\n".join(chapter.latex(doc_data, quiet) for chapter in self.chapters)
        )
        if self.is_reference:
            result = "\n\n\\referencestart" + result
        return result


class DocChapter(object):
    def __init__(self, part, title, doc=None):
        self.doc = doc
        self.guide_sections = []
        self.part = part
        self.title = title
        self.slug = slugify(title)
        self.sections = []
        self.sections_by_slug = {}
        part.chapters_by_slug[self.slug] = self

    def __str__(self):
        sections = "\n".join(str(section) for section in self.sections)
        return f"= {self.title} =\n\n{sections}"

    @property
    def all_sections(self):
        return sorted(self.sections + self.guide_sections)

    def latex(self, doc_data: dict, quiet=False) -> str:
        """Render this Chapter object as LaTeX string and return that.

        `output` is not used here but passed along to the bottom-most
        level in getting expected test results.
        """
        if not quiet:
            print(f"Formatting Chapter {self.title}")
        intro = self.doc.latex(doc_data).strip()
        if intro:
            short = "short" if len(intro) < 300 else ""
            intro = "\\begin{chapterintro%s}\n%s\n\n\\end{chapterintro%s}" % (
                short,
                intro,
                short,
            )
        chapter_sections = [
            ("\n\n\\chapter{%(title)s}\n\\chapterstart\n\n%(intro)s")
            % {"title": escape_latex(self.title), "intro": intro},
            "\\chaptersections\n",
            "\n\n".join(section.latex(doc_data, quiet) for section in self.sections),
            "\n\\chapterend\n",
        ]
        return "".join(chapter_sections)


class DocSection(object):
    def __init__(
        self, chapter, title, text, operator=None, installed=True, in_guide=False
    ):

        self.chapter = chapter
        self.in_guide = in_guide
        self.installed = installed
        self.items = []  # tests in section when this is under a guide section
        self.operator = operator
        self.slug = slugify(title)
        self.subsections = []
        self.subsections_by_slug = {}
        self.title = title
        if text.count("<dl>") != text.count("</dl>"):
            raise ValueError(
                "Missing opening or closing <dl> tag in "
                "{} documentation".format(title)
            )

        # Needs to come after self.chapter is initialized since
        # XMLDoc uses self.chapter.
        self.doc = XMLDoc(text, title, self)

        chapter.sections_by_slug[self.slug] = self

    # Add __eq__ and __lt__ so we can sort Sections.
    def __eq__(self, other):
        return self.title == other.title

    def __lt__(self, other):
        return self.title < other.title

    def __str__(self):
        return f"== {self.title} ==\n{self.doc}"

    def latex(self, doc_data: dict, quiet=False) -> str:
        """Render this Section object as LaTeX string and return that.

        `output` is not used here but passed along to the bottom-most
        level in getting expected test results.
        """
        if not quiet:
            # The leading spaces help show chapter level.
            print(f"  Formatting Section {self.title}")
        title = escape_latex(self.title)
        if self.operator:
            title += " (\\code{%s})" % escape_latex_code(self.operator)
        index = (
            r"\index{%s}" % escape_latex(self.title)
            if self.chapter.part.is_reference
            else ""
        )
        content = self.doc.latex(doc_data)
        section_string = (
            "\n\n\\%(sub)ssection*{%(title)s}%(index)s\n"
            "\\%(sub)ssectionstart\n\n%(content)s"
            "\\addcontentsline{toc}{%(sub)ssection}{%(title)s}"
            "%(sections)s"
            "\\%(sub)ssectionend"
        ) % {
            "sub": "",  # sub,
            "title": title,
            "index": index,
            "sections": "\n\n".join(
                section.latex(doc_data) for section in self.subsections
            ),
            "content": content,
        }
        return section_string


class DocGuideSection(DocSection):
    """An object for a Documented Guide Section.
    A Guide Section is part of a Chapter. "Colors" or "Special Functions"
    are examples of Guide Sections, and each contains a number of Sections.
    like NamedColors or Orthogonal Polynomials.
    """

    def __init__(
        self, chapter: str, title: str, text: str, submodule, installed: bool = True
    ):
        self.chapter = chapter
        self.doc = XMLDoc(text, title, None)
        self.in_guide = False
        self.installed = installed
        self.section = submodule
        self.slug = slugify(title)
        self.subsections = []
        self.subsections_by_slug = {}
        self.title = title

        # FIXME: Sections never are operators. Subsections can have
        # operators though.  Fix up the view and searching code not to
        # look for the operator field of a section.
        self.operator = False

        if text.count("<dl>") != text.count("</dl>"):
            raise ValueError(
                "Missing opening or closing <dl> tag in "
                "{} documentation".format(title)
            )
        # print("YYY Adding section", title)
        chapter.sections_by_slug[self.slug] = self

    def get_tests(self):
        # FIXME: The below is a little weird for Guide Sections.
        # Figure out how to make this clearer.
        # A guide section's subsection are Sections without the Guide.
        # it is *their* subsections where we generally find tests.
        for section in self.subsections:
            for subsection in section.subsections:
                # FIXME we are omitting the section title here...
                for doctests in subsection.items:
                    yield doctests.get_tests()

    def latex(self, doc_data: dict, quiet=False):
        """Render this Guide Section object as LaTeX string and return that.

        `output` is not used here but passed along to the bottom-most
        level in getting expected test results.
        """
        if not quiet:
            # The leading spaces help show chapter level.
            print(f"  Formatting Guide Section {self.title}")
        intro = self.doc.latex(doc_data).strip()
        if intro:
            short = "short" if len(intro) < 300 else ""
            intro = "\\begin{guidesectionintro%s}\n%s\n\n\\end{guidesectionintro%s}" % (
                short,
                intro,
                short,
            )
        guide_sections = [
            (
                "\n\n\\section{%(title)s}\n\\sectionstart\n\n%(intro)s"
                "\\addcontentsline{toc}{section}{%(title)s}"
            )
            % {"title": escape_latex(self.title), "intro": intro},
            "\n\n".join(section.latex(doc_data) for section in self.subsections),
        ]
        return "".join(guide_sections)


class DocSubsection(object):
    """An object for a Documented Subsection.
    A Subsection is part of a Section.
    """

    def __init__(
        self,
        chapter,
        section,
        title,
        text,
        operator=None,
        installed=True,
        in_guide=False,
    ):
        """
        Information that goes into a subsection object. This can be a written text, or
        text extracted from the docstring of a builtin module or class.

        About some of the parameters...

        Some of the subsections are contained in a grouping module and need special work to
        get the grouping module name correct.

        For example the Chapter "Colors" is a module so the docstring text for it is in
        mathics/builtin/colors/__init__.py . In mathics/builtin/colors/named-colors.py we have
        the "section" name for the class Read (the subsection) inside it.
        """

        self.doc = XMLDoc(text, title, section)
        self.chapter = chapter
        self.in_guide = in_guide
        self.installed = installed
        self.operator = operator

        self.section = section
        self.slug = slugify(title)
        self.subsections = []
        self.title = title

        if in_guide:
            # Tests haven't been picked out yet from the doc string yet.
            # Gather them here.
            self.items = gather_tests(text)
        else:
            self.items = []

        if text.count("<dl>") != text.count("</dl>"):
            raise ValueError(
                "Missing opening or closing <dl> tag in "
                "{} documentation".format(title)
            )
        self.section.subsections_by_slug[self.slug] = self

    def __str__(self):
        return f"=== {self.title} ===\n{self.doc}"

    def latex(self, doc_data: dict, quiet=False):
        """Render this Subsection object as LaTeX string and return that.

        `output` is not used here but passed along to the bottom-most
        level in getting expected test results.
        """
        if not quiet:
            # The leading spaces help show chapter, and section nesting level.
            print(f"    Formatting Subsection Section {self.title}")

        title = escape_latex(self.title)
        if self.operator:
            title += " (\\code{%s})" % escape_latex_code(self.operator)
        index = (
            r"\index{%s}" % escape_latex(self.title)
            if self.chapter.part.is_reference
            else ""
        )
        content = self.doc.latex(doc_data)
        section_string = (
            "\n\n\\%(sub)ssection*{%(title)s}%(index)s\n"
            "\\%(sub)ssectionstart\n\n%(content)s"
            "\\addcontentsline{toc}{%(sub)ssection}{%(title)s}"
            "%(sections)s"
            "\\%(sub)ssectionend"
        ) % {
            "sub": "sub",
            "title": title,
            "index": index,
            "content": content,
            "sections": "\n\n".join(
                section.latex(doc_data, quiet) for section in self.subsections
            ),
        }
        return section_string


def gather_tests(doc: str, key_part=None) -> list:
    # Remove commented lines.
    doc = filter_comments(doc).strip(r"\s")

    # Remove leading <dl>...</dl>
    # doc = DL_RE.sub("", doc)

    # pre-substitute Python code because it might contain tests
    doc, post_substitutions = pre_sub(
        PYTHON_RE, doc, lambda m: "<python>%s</python>" % m.group(1)
    )

    # HACK: Artificially construct a last testcase to get the "intertext"
    # after the last (real) testcase. Ignore the test, of course.
    doc += "\n>> test\n = test"
    testcases = TESTCASE_RE.findall(doc)

    tests = None
    items = []
    for index in range(len(testcases)):
        testcase = list(testcases[index])
        text = testcase.pop(0).strip()
        if text:
            if tests is not None:
                items.append(tests)
                tests = None
            text = post_sub(text, post_substitutions)
            items.append(DocText(text))
            tests = None
        if index < len(testcases) - 1:
            test = DocTest(index, testcase, key_part)
            if tests is None:
                tests = DocTests()
            tests.tests.append(test)
        if tests is not None:
            items.append(tests)
            tests = None
    return items


class XMLDoc(object):
    """A class to hold our internal XML-like format data.
    The `latex()` method can turn this into LaTeX.

    Mathics core also uses this in getting usage strings (`??`).
    """

    def __init__(self, doc, title, section):
        self.title = title
        if section:
            chapter = section.chapter
            part = chapter.part
            # Note: we elide section.title
            key_prefix = (part.title, chapter.title, title)
        else:
            key_prefix = None

        self.rawdoc = doc
        self.items = gather_tests(self.rawdoc, key_prefix)

    def __str__(self):
        return "\n".join(str(item) for item in self.items)

    def text(self, detail_level):
        # used for introspection
        # TODO parse XML and pretty print
        # HACK
        item = str(self.items[0])
        item = "\n".join(line.strip() for line in item.split("\n"))
        item = item.replace("<dl>", "")
        item = item.replace("</dl>", "")
        item = item.replace("<dt>", "  ")
        item = item.replace("</dt>", "")
        item = item.replace("<dd>", "    ")
        item = item.replace("</dd>", "")
        item = "\n".join(line for line in item.split("\n") if not line.isspace())
        return item

    def get_tests(self):
        tests = []
        for item in self.items:
            tests.extend(item.get_tests())
        return tests

    def latex(self, doc_data: dict):
        if len(self.items) == 0:
            if hasattr(self, "rawdoc") and len(self.rawdoc) != 0:
                # We have text but no tests
                return escape_latex(self.rawdoc)

        return "\n".join(
            item.latex(doc_data) for item in self.items if not item.is_private()
        )


class DocText(object):
    def __init__(self, text):
        self.text = text

    def get_tests(self):
        return []

    def is_private(self):
        return False

    def __str__(self):
        return self.text

    def latex(self, doc_data):
        return escape_latex(self.text)

    def test_indices(self):
        return []


class DocTests(object):
    def __init__(self):
        self.tests = []

    def get_tests(self):
        return self.tests

    def is_private(self):
        return all(test.private for test in self.tests)

    def __str__(self):
        return "\n".join(str(test) for test in self.tests)

    def latex(self, doc_data: dict):
        if len(self.tests) == 0:
            return "\n"

        testLatexStrings = [
            test.latex(doc_data) for test in self.tests if not test.private
        ]
        testLatexStrings = [t for t in testLatexStrings if len(t) > 1]
        if len(testLatexStrings) == 0:
            return "\n"

        return "\\begin{tests}%%\n%s%%\n\\end{tests}" % ("%\n".join(testLatexStrings))

    def test_indices(self):
        return [test.index for test in self.tests]


# This string is used so we can indicate a trailing blank at the end of a line by
# adding this string to the end of the line which gets stripped off.
# Some editors and formatters like to strip off trailing blanks at the ends of lines.
END_LINE_SENTINAL = "#<--#"


class DocTest(object):
    """
    DocTest formatting rules:

    * `>>` Marks test case; it will also appear as part of
           the documentation.
    * `#>` Marks test private or one that does not appear as part of
           the documentation.
    * `X>` Shows the example in the docs, but disables testing the example.
    * `S>` Shows the example in the docs, but disables testing if environment
           variable SANDBOX is set.
    * `=`  Compares the result text.
    * `:`  Compares an (error) message.
      `|`  Prints output.
    """

    def __init__(self, index, testcase, key_prefix=None):
        def strip_sentinal(line):
            """Remove END_LINE_SENTINAL from the end of a line if it appears.

            Some editors like to strip blanks at the end of a line.
            Since the line ends in END_LINE_SENTINAL which isn't blank,
            any blanks that appear before will be preserved.

            Some tests require some lines to be blank or entry because
            Mathics output can be that way
            """
            if line.endswith(END_LINE_SENTINAL):
                line = line[: -len(END_LINE_SENTINAL)]

            # Also remove any remaining trailing blanks since that
            # seems *also* what we want to do.
            return line.strip()

        self.index = index
        self.result = None
        self.outs = []

        # Private test cases are executed, but NOT shown as part of the docs
        self.private = testcase[0] == "#"

        # Ignored test cases are NOT executed, but shown as part of the docs
        # Sandboxed test cases are NOT executed if environtment SANDBOX is set
        if testcase[0] == "X" or (testcase[0] == "S" and getenv("SANDBOX", False)):
            self.ignore = True
            # substitute '>' again so we get the correct formatting
            testcase[0] = ">"
        else:
            self.ignore = False

        self.test = strip_sentinal(testcase[1])

        self.key = None
        if key_prefix:
            self.key = tuple(key_prefix + (index,))
        outs = testcase[2].splitlines()
        for line in outs:
            line = strip_sentinal(line)
            if line:
                if line.startswith("."):
                    text = line[1:]
                    if text.startswith(" "):
                        text = text[1:]
                    text = "\n" + text
                    if self.result is not None:
                        self.result += text
                    elif self.outs:
                        self.outs[-1].text += text
                    continue

                match = TESTCASE_OUT_RE.match(line)
                if not match:
                    continue
                symbol, text = match.group(1), match.group(2)
                text = text.strip()
                if symbol == "=":
                    self.result = text
                elif symbol == ":":
                    out = Message("", "", text)
                    self.outs.append(out)
                elif symbol == "|":
                    out = Print(text)
                    self.outs.append(out)

    def __str__(self):
        return self.test

    def latex(self, doc_data: dict) -> str:
        text = ""
        text += "\\begin{testcase}\n"
        text += "\\test{%s}\n" % escape_latex_code(self.test)
        if self.key is None:
            return ""
        output_for_key = doc_data.get(self.key, None)
        if output_for_key is None:
            output_for_key = get_results_by_test(self.test, self.key, doc_data)

        results = output_for_key.get("results", [])
        for result in results:
            for out in result["out"]:
                kind = "message" if out["message"] else "print"
                text += "\\begin{test%s}%s\\end{test%s}" % (
                    kind,
                    escape_latex_output(out["text"]),
                    kind,
                )
            if result["result"]:  # is not None and result['result'].strip():
                text += "\\begin{testresult}%s\\end{testresult}" % result["result"]
        text += "\\end{testcase}"
        return text
