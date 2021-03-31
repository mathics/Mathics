# -*- coding: utf-8 -*-

import re
from os import getenv, listdir, path
import pickle
import importlib

from mathics import settings

from mathics import builtin
from mathics.builtin import get_module_doc
from mathics.core.evaluation import Message, Print
from mathics.doc.utils import slugify

CHAPTER_RE = re.compile('(?s)<chapter title="(.*?)">(.*?)</chapter>')
SECTION_RE = re.compile('(?s)(.*?)<section title="(.*?)">(.*?)</section>')
SUBSECTION_RE = re.compile('(?s)<subsection title="(.*?)">')
SUBSECTION_END_RE = re.compile("</subsection>")

TESTCASE_RE = re.compile(
    r"""(?mx)^
    ((?:.|\n)*?)
    ^\s*([>#SX])>[ ](.*)
    ((?:\n\s*(?:[:|=.][ ]|\.).*)*)
"""
)
TESTCASE_OUT_RE = re.compile(r"^\s*([:|=])(.*)$")

MATHICS_RE = re.compile(r"(?<!\\)\'(.*?)(?<!\\)\'")

# preserve space before and after inline code variables
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

try:
    with open(settings.DOC_XML_DATA, "rb") as xml_data_file:
        xml_data = pickle.load(xml_data_file)
except IOError:
    xml_data = {}


def filter_comments(doc):
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
    " Escape Mathics output "

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
    " Escape verbatim Mathics input "

    text = escape_latex_output(text)
    escape_char = get_latex_escape_char(text)
    return "\\lstinline%s%s%s" % (escape_char, text, escape_char)


def escape_latex(text):
    " Escape documentation text "

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

    text = _replace_all(
        text,
        [
            ("$", r"\$"),
            ("\u03c0", r"$\pi$"),
            ("≥", r"$\ge$"),
            ("≤", r"$\le$"),
            ("≠", r"$\ne$"),
            ("ç", r"\c{c}"),
            ("é", r"\'e"),
            ("ê", r"\^e"),
            ("ñ", r"\~n"),
            ("∫", r"\int"),
            ("", r"d"),
        ],
    )

    def repl_char(match):
        char = match.group(1)
        return {"^": "$^\wedge$",}[char]

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

    '''def repl_asy(match):
        """
        Ensure \begin{asy} and \end{asy} are on their own line,
        but there shall be no extra empty lines
        """
        #tag = match.group(1)
        #return '\n%s\n' % tag
        #print "replace"
        return '\\end{asy}\n\\begin{asy}'
    text = LATEX_BETWEEN_ASY_RE.sub(repl_asy, text)'''

    def repl_subsection(match):
        return "\n\\subsection*{%s}\n" % match.group(1)

    text = SUBSECTION_RE.sub(repl_subsection, text)
    text = SUBSECTION_END_RE.sub("", text)

    for key, (xml, tex) in SPECIAL_COMMANDS.items():
        # "\" has been escaped already => 2 \
        text = text.replace("\\\\" + key, tex)

    text = post_sub(text, post_substitutions)

    return text


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
        " Prevent linebreaks between inline code and sentence delimeters "

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


def escape_html(text, verbatim_mode=False, counters=None, single_line=False):
    def repl_python(match):
        return (
            r"""<pre><![CDATA[
%s
]]></pre>"""
            % match.group(1).strip()
        )

    text, post_substitutions = pre_sub(PYTHON_RE, text, repl_python)

    text = text.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")

    if not verbatim_mode:

        def repl_quotation(match):
            return r"&ldquo;%s&rdquo;" % match.group(1)

        text = QUOTATIONS_RE.sub(repl_quotation, text)

    if counters is None:
        counters = {}

    text = text.replace('"', "&quot;")
    if not verbatim_mode:

        def repl_latex(match):
            return "%s<var>%s</var>%s" % (
                match.group(1),
                match.group(2),
                match.group(3),
            )

        text = LATEX_RE.sub(repl_latex, text)

        def repl_mathics(match):
            text = match.group(1)
            text = text.replace("\\'", "'")
            text = text.replace(" ", "&nbsp;")
            if text:
                return "<code>%s</code>" % text
            else:
                return "'"

        def repl_allowed(match):
            content = _replace_all(
                match.group(1), [("&ldquo;", '"'), ("&rdquo;", '"'), ("&quot;", '"')]
            )
            return "<%s>" % content

        text = MATHICS_RE.sub(repl_mathics, text)
        for allowed in ALLOWED_TAGS:
            text = ALLOWED_TAGS_RE[allowed].sub(repl_allowed, text)
            text = text.replace("&lt;/%s&gt;" % allowed, "</%s>" % allowed)

        def repl_dl(match):
            text = match.group(1)
            text = DL_ITEM_RE.sub(
                lambda m: "<%(tag)s>%(content)s</%(tag)s>\n" % m.groupdict(), text
            )
            return "<dl>%s</dl>" % text

        text = DL_RE.sub(repl_dl, text)

        def repl_list(match):
            tag = match.group("tag")
            content = match.group("content")
            content = LIST_ITEM_RE.sub(lambda m: "<li>%s</li>" % m.group(1), content)
            return "<%s>%s</%s>" % (tag, content, tag)

        text = LIST_RE.sub(repl_list, text)

        def repl_hypertext(match):
            tag = match.group("tag")
            content = match.group("content")
            if tag == "em":
                return r"<em>%s</em>" % content
            elif tag == "url":
                return r'<a href="%s">%s</a>' % (content, content)

        text = HYPERTEXT_RE.sub(repl_hypertext, text)

        def repl_console(match):
            tag = match.group("tag")
            content = match.group("content")
            tag = "div" if tag == "console" else "span"
            content = content.strip()
            pre = post = ""

            # gets replaced for <br /> later by DocText.html()
            content = content.replace("\n", "<br>")

            return r'<%s class="console">%s%s%s</%s>' % (tag, pre, content, post, tag)

        text = CONSOLE_RE.sub(repl_console, text)

        def repl_img(match):
            src = match.group("src")
            title = match.group("title")
            return (
                r'<a href="/media/doc/%(src)s.pdf">'
                r'<img src="/media/doc/%(src)s.png" title="%(title)s" />'
                r"</a>"
            ) % {"src": src, "title": title}

        text = IMG_RE.sub(repl_img, text)

        def repl_imgpng(match):
            src = match.group("src")
            title = match.group("title")
            return (r'<img src="/media/doc/%(src)s" title="%(title)s" />') % {
                "src": src,
                "title": title,
            }

        text = IMG_PNG_RE.sub(repl_imgpng, text)

        def repl_ref(match):
            # TODO: this is not an optimal solution - maybe we need figure
            # numbers in the XML doc as well?
            return r"the following figure"

        text = REF_RE.sub(repl_ref, text)

        def repl_subsection(match):
            return '\n<h2 label="%s">%s</h2>\n' % (match.group(1), match.group(1))

        text = SUBSECTION_RE.sub(repl_subsection, text)
        text = SUBSECTION_END_RE.sub("", text)

        text = text.replace("\\'", "'")
    else:
        text = text.replace(" ", "&nbsp;")
        text = "<code>%s</code>" % text
    text = text.replace("'", "&#39;")
    text = text.replace("---", "&mdash;")
    for key, (xml, tex) in SPECIAL_COMMANDS.items():
        text = text.replace("\\" + key, xml)

    if not single_line:
        text = linebreaks(text)
        text = text.replace("<br />", "\n").replace("<br>", "<br />")

    text = post_sub(text, post_substitutions)

    text = text.replace("<p><pre>", "<pre>").replace("</pre></p>", "</pre>")

    return text


class Tests(object):
    def __init__(self, part, chapter, section, tests):
        self.part, self.chapter = part, chapter
        self.section, self.tests = section, tests


class DocElement(object):
    def href(self, ajax=False):
        if ajax:
            return "javascript:loadDoc('%s')" % self.get_url()
        else:
            return "/doc%s" % self.get_url()

    def get_prev(self):
        return self.get_prev_next()[0]

    def get_next(self):
        return self.get_prev_next()[1]

    def get_collection(self):
        return []

    def get_prev_next(self):
        collection = self.get_collection()
        index = collection.index(self)
        prev = collection[index - 1] if index > 0 else None
        next = collection[index + 1] if index < len(collection) - 1 else None
        return prev, next

    def get_title_html(self):
        return mark_safe(escape_html(self.title, single_line=True))


class Documentation(DocElement):
    def __str__(self):
        return "\n\n\n".join(str(part) for part in self.parts)

    def get_tests(self):
        for part in self.parts:
            for chapter in part.chapters:
                tests = chapter.doc.get_tests()
                if tests:
                    yield Tests(part.title, chapter.title, "", tests)
                for section in chapter.sections:
                    if section.installed:
                        tests = section.doc.get_tests()
                        if tests:
                            yield Tests(part.title, chapter.title, section.title, tests)

    def get_part(self, part_slug):
        return self.parts_by_slug.get(part_slug)

    def get_chapter(self, part_slug, chapter_slug):
        part = self.parts_by_slug.get(part_slug)
        if part:
            return part.chapters_by_slug.get(chapter_slug)
        return None
        """for part in self.parts:
            if part.slug == part_slug:
                for chapter in self:
                    pass"""

    def get_section(self, part_slug, chapter_slug, section_slug):
        part = self.parts_by_slug.get(part_slug)
        if part:
            chapter = part.chapters_by_slug.get(chapter_slug)
            if chapter:
                return chapter.sections_by_slug.get(section_slug)
        return None

    def latex(self, output):
        parts = []
        appendix = False
        for part in self.parts:
            text = part.latex(output)
            if part.is_appendix and not appendix:
                appendix = True
                text = "\n\\appendix\n" + text
            parts.append(text)
        result = "\n\n".join(parts)
        result = post_process_latex(result)
        return result

    def get_url(self):
        return "/"

    def search(self, query):
        query = query.strip()
        query_parts = [q.strip().lower() for q in query.split()]

        def matches(text):
            text = text.lower()
            return all(q in text for q in query_parts)

        result = []
        for part in self.parts:
            if matches(part.title):
                result.append((False, part))
            for chapter in part.chapters:
                if matches(chapter.title):
                    result.append((False, chapter))
                for section in chapter.sections:
                    if matches(section.title):
                        result.append((section.title == query, section))
                    elif query == section.operator:
                        result.append((True, section))
        return result


class MathicsMainDocumentation(Documentation):
    def __init__(self):
        self.title = "Overview"
        self.parts = []
        self.parts_by_slug = {}
        self.doc_dir = settings.DOC_DIR
        self.xml_data_file = settings.DOC_XML_DATA
        self.tex_data_file = settings.DOC_TEX_DATA
        self.latex_file = settings.DOC_LATEX_FILE
        self.pymathics_doc_loaded = False
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
                        if not chapter.doc:
                            chapter.doc = Doc(pre_text)
                        if title:
                            section = DocSection(chapter, title, text)
                            chapter.sections.append(section)
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
            for module in modules:
                title, text = get_module_doc(module)
                chapter = DocChapter(builtin_part, title, Doc(text))
                builtins = builtins_by_module[module.__name__]
                for instance in builtins:
                    installed = True
                    for package in getattr(instance, "requires", []):
                        try:
                            importlib.import_module(package)
                        except ImportError:
                            installed = False
                            break
                    section = DocSection(
                        chapter,
                        strip_system_prefix(instance.get_name()),
                        instance.__doc__ or "",
                        operator=instance.get_operator(),
                        installed=installed,
                    )
                    chapter.sections.append(section)
                builtin_part.chapters.append(chapter)
            self.parts.append(builtin_part)

        for part in appendix:
            self.parts.append(part)

        # set keys of tests
        for tests in self.get_tests():
            for test in tests.tests:
                test.key = (tests.part, tests.chapter, tests.section, test.index)

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
        self.xml_data_file = None
        self.tex_data_file = None
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
            mainfolder = ""
            self.pymathicsmodule = None
            self.parts = []
            return

        try:
            mainfolder = self.pymathicsmodule.__path__[0]
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
            mainfolder = ""
            self.pymathicsmodule = None
            self.parts = []
            return

        # Paths
        self.doc_dir = self.pymathicsmodule.__path__[0] + "/doc/"
        self.xml_data_file = self.doc_dir + "xml/data"
        self.tex_data_file = self.doc_dir + "tex/data"
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
            self.xml_data_file = ""
            self.tex_data_file = ""
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
                            chapter.doc = Doc(pre_text)
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
        chapter = DocChapter(builtin_part, title, Doc(text))
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


class DocPart(DocElement):
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

    def latex(self, output):
        result = "\n\n\\part{%s}\n\n" % escape_latex(self.title) + (
            "\n\n".join(chapter.latex(output) for chapter in self.chapters)
        )
        if self.is_reference:
            result = "\n\n\\referencestart" + result
        return result

    def get_url(self):
        return f"/{self.slug}/"

    def get_collection(self):
        return self.doc.parts


class DocChapter(DocElement):
    def __init__(self, part, title, doc=None):
        self.part = part
        self.title = title
        self.slug = slugify(title)
        self.doc = doc
        self.sections = []
        self.sections_by_slug = {}
        part.chapters_by_slug[self.slug] = self

    def __str__(self):
        sections = "\n".join(str(section) for section in self.sections)
        return f"= {self.title} =\n\n{sections}"

    def latex(self, output):
        intro = self.doc.latex(output).strip()
        if intro:
            short = "short" if len(intro) < 300 else ""
            intro = "\\begin{chapterintro%s}\n%s\n\n\\end{chapterintro%s}" % (
                short,
                intro,
                short,
            )
        return "".join(
            [
                "\n\n\\chapter{%(title)s}\n\\chapterstart\n\n%(intro)s"
                % {"title": escape_latex(self.title), "intro": intro},
                "\\chaptersections\n",
                "\n\n".join(section.latex(output) for section in self.sections),
                "\n\\chapterend\n",
            ]
        )

    def get_url(self):
        return f"/{self.part.slug}/{self.slug}/"

    def get_collection(self):
        return self.part.chapters


class DocSection(DocElement):
    def __init__(self, chapter, title, text, operator=None, installed=True):
        self.chapter = chapter
        self.title = title
        self.slug = slugify(title)
        if text.count("<dl>") != text.count("</dl>"):
            raise ValueError(
                "Missing opening or closing <dl> tag in "
                "{} documentation".format(title)
            )
        self.doc = Doc(text)
        self.operator = operator
        self.installed = installed
        chapter.sections_by_slug[self.slug] = self

    def __str__(self):
        return f"== {self.title} ==\n{self.doc}"

    def latex(self, output):
        title = escape_latex(self.title)
        if self.operator:
            title += " (\\code{%s})" % escape_latex_code(self.operator)
        index = (
            "\index{%s}" % escape_latex(self.title)
            if self.chapter.part.is_reference
            else ""
        )
        return (
            "\n\n\\section*{%(title)s}%(index)s\n"
            "\\sectionstart\n\n%(content)s\\sectionend"
            "\\addcontentsline{toc}{section}{%(title)s}"
        ) % {"title": title, "index": index, "content": self.doc.latex(output)}

    def get_url(self):
        return f"/{self.chapter.part.slug}/{self.chapter.slug}/{self.slug}/"

    def get_collection(self):
        return self.chapter.sections

    def html_data(self):
        indices = set()
        for test in self.doc.items:
            indices.update(test.test_indices())
        result = {}
        for index in indices:
            result[index] = xml_data.get(
                (self.chapter.part.title, self.chapter.title, self.title, index)
            )
        return result


class Doc(object):
    def __init__(self, doc):
        self.items = []
        # remove commented lines
        doc = filter_comments(doc)
        # pre-substitute Python code because it might contain tests
        doc, post_substitutions = pre_sub(
            PYTHON_RE, doc, lambda m: "<python>%s</python>" % m.group(1)
        )
        # HACK: Artificially construct a last testcase to get the "intertext"
        # after the last (real) testcase. Ignore the test, of course.
        doc += "\n>> test\n = test"
        testcases = TESTCASE_RE.findall(doc)
        tests = None
        for index in range(len(testcases)):
            testcase = list(testcases[index])
            text = testcase.pop(0).strip()
            if text:
                if tests is not None:
                    self.items.append(tests)
                    tests = None
                text = post_sub(text, post_substitutions)
                self.items.append(DocText(text))
                tests = None
            if index < len(testcases) - 1:
                test = DocTest(index, testcase)
                if tests is None:
                    tests = DocTests()
                tests.tests.append(test)
            if tests is not None:
                self.items.append(tests)
                tests = None

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

    def latex(self, output):
        return "\n".join(
            item.latex(output) for item in self.items if not item.is_private()
        )

    def html(self):
        counters = {}
        return mark_safe(
            "\n".join(
                item.html(counters) for item in self.items if not item.is_private()
            )
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

    def latex(self, output):
        return escape_latex(self.text)

    def html(self, counters=None):
        result = escape_html(self.text, counters=counters)
        return result

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

    def latex(self, output):
        if len(self.tests) == 0:
            return "\n"

        testLatexStrings = [
            test.latex(output) for test in self.tests if not test.private
        ]
        testLatexStrings = [t for t in testLatexStrings if len(t) > 1]
        if len(testLatexStrings) == 0:
            return "\n"

        return "\\begin{tests}%%\n%s%%\n\\end{tests}" % ("%\n".join(testLatexStrings))

    def html(self, counters=None):
        if len(self.tests) == 0:
            return "\n"
        return '<ul class="tests">%s</ul>' % (
            "\n".join(
                "<li>%s</li>" % test.html() for test in self.tests if not test.private
            )
        )

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

    def __init__(self, index, testcase):
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

    def latex(self, output):
        text = ""
        text += "\\begin{testcase}\n"
        text += "\\test{%s}\n" % escape_latex_code(self.test)
        if self.key is None:
            return ""
        results = output[self.key]["results"]
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

    def html(self):
        result = '<div class="test"><span class="move"></span>'
        result += '<ul class="test" id="test_%d">' % self.index
        result += '<li class="test">%s</li>' % escape_html(self.test, True)
        result += "</ul>"
        result += "</div>"
        return result
