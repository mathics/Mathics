from __future__ import print_function

from metakernel import MetaKernel, ProcessMetaKernel, REPLWrapper, u
from IPython.display import Image, SVG
from IPython.display import Latex, HTML

import subprocess
import os
import sys
import tempfile


__version__ = '0.0.0'


class MathicsKernel(ProcessMetaKernel):
    implementation = 'Mathics Kernel'
    implementation_version = __version__,
    language = 'mathics'
    language_version = '0.1',
    banner = "Mathics Kernel"
    language_info = {
        'exec': 'mathics',
        'mimetype': 'text/x-mathics',
        'name': 'mathics_kernel',
        'file_extension': '.m',
        'help_links': MetaKernel.help_links,
    }

    _initstring = """
$MyPrePrint:=Module[{fn,res}, 
If[#1 === Null, res=\"null:\",
Switch[Head[#1],
          String,
            res=\"string:\"<>#1,
          Graphics,            
            fn=\"{sessiondir}/session-figure\"<>ToString[$Line]<>\".svg\";
            Export[fn,#1,"SVG"];
            res=\"svg:\"<>fn<>\":\"<>\"- graphics -\",
          Graphics3D,            
            fn=\"{sessiondir}/session-figure\"<>ToString[$Line]<>\".jpg\";
            Export[fn,#1,"JPG"];
            res=\"image:\"<>fn<>\":\"<>\"- graphics3d -\",
          Sound,
            fn=\"{sessiondir}/session-sound\"<>ToString[$Line]<>\".wav\";
            Export[fn,#1,"wav"];
            res=\"sound:\"<>fn<>\":\"<>\"- sound -\",
          _,            
            texstr=StringReplace[ToString[TeXForm[#1]],\"\\n\"->\" \"];
            res=\"tex:\"<>ToString[StringLength[texstr]]<>\":\"<> texstr<>\":\"<>ToString[InputForm[#1]]
       ]
       ];
       res
    ]&;
$DisplayFunction=Identity;
"""

    _session_dir = ""
    _first = True
    initfilename = ""
    _banner = None

    @property
    def banner(self):
        if self._banner is None:
            banner = "mathics 0.1"
            self._banner = banner.decode('utf-8')
        return self._banner

    def makeWrapper(self):
        """Start a bash shell and return a :class:`REPLWrapper` object.
        Note that this is equivalent :function:`metakernel.pyexpect.bash`,
        but is used here as an example of how to be cross-platform.
        """
        orig_prompt = u('In\[.*\]:=')
        prompt_cmd = None
        change_prompt = None

        self._first = True
        self._session_dir = tempfile.mkdtemp()
        self._initstring = self._initstring.replace("{sessiondir}", self._session_dir)
        self.initfilename = self._session_dir + '/init.m'
        print(self.initfilename)
        f = open(self.initfilename, 'w')
        f.write(self._initstring)
        f.close()
        if self.language_info['exec'] == 'mathics':
            replwrapper = REPLWrapper("mathics --colors NOCOLOR", orig_prompt, change_prompt,
                           prompt_emit_cmd=prompt_cmd, echo=True)
        else:
            replwrapper = REPLWrapper("math ", orig_prompt, change_prompt,
                           prompt_emit_cmd=prompt_cmd, echo=True)
        return replwrapper

    def do_execute_direct(self, code):
        if(self._first):
            super(MathicsKernel, self).do_execute_direct("Get[\"" + self.initfilename + "\"];$Line=0;")
            self._first = False
        # Processing multiline code
        codelines = code.splitlines()
        lastline = ""
        resp = ""
        for codeline in codelines:
            if codeline.strip() == "":
                lastline = lastline.strip()
                if lastline == "":
                    continue
                if resp.strip() != "":
                    print(resp)
                resp = self.do_execute_direct(lastline)
                lastline = ""
                continue
            lastline = lastline + codeline
        code = lastline.strip()
        if code == "":
            return resp
        else:
            if resp != "":
                print(resp)
            # Processing last valid line
        resp = super(MathicsKernel, self).do_execute_direct("$MyPrePrint[" + code + "]")

        lineresponse = resp.splitlines()
        outputfound = False
        outputtext = ""
        for linnum, liner in enumerate(lineresponse):
            if not outputfound and liner[:4] == "Out[":
                outputfound = True
                for pos in range(len(liner) - 4):
                    if liner[pos + 4] == '=':
                        outputtext = liner[(pos + 6):]
                continue
            if outputfound:
                if liner == u' ':
                    if outputtext[-1] == '\\':  # and lineresponse[linnum + 1] == '>':
                        outputtext = outputtext[:-1]
                        lineresponse[linnum + 1] = lineresponse[linnum + 1][4:]
                        continue
                outputtext = outputtext + liner
            else:
                print(liner)
        if(outputtext[:5] == 'null:'):
            return ""
        if(outputtext[:4] == 'svg:'):
            for p in range(len(outputtext) - 4):
                pp = p + 4
                if outputtext[pp] == ':':
                    self.Display(SVG(outputtext[4:pp]))
                    return outputtext[(pp + 1):]
        if(outputtext[:6] == 'image:'):
            for p in range(len(outputtext) - 6):
                pp = p + 6
                if outputtext[pp] == ':':
                    print(outputtext[6:pp])
                    self.Display(Image(outputtext[6:pp]))
                    return outputtext[(pp + 1):]
        if(outputtext[:6] == 'sound:'):
            for p in range(len(outputtext) - 6):
                pp = p + 6
                if outputtext[pp] == ':':
                    htmlstr = "<audio controls> <source src=\"file://"
                    htmlstr = htmlstr + outputtext[6:pp]
                    htmlstr = htmlstr + "\" type=\"audio/wav\">"
                    htmlstr = htmlstr + "Your browser does not support the audio element."
                    htmlstr = htmlstr + "</audio>"
                    self.Display(HTML(htmlstr))
                    print(htmlstr)
                    return outputtext[(pp + 1):]
        if (outputtext[:7] == 'string:'):
            return outputtext[7:]
        if (outputtext[:4] == 'tex:'):
            for p in range(len(outputtext) - 4):
                pp = p + 4
                if outputtext[pp] == ':':
                    lentex = int(outputtext[4:pp])
                    self.Display(Latex('$' + outputtext[(pp + 1):(pp + lentex + 1)] + '$'))
                    return outputtext[(pp + lentex + 2):]

#        if self.plot_settings.get('backend', None) == 'inline':
#            plot_dir = tempfile.mkdtemp()
#            self._make_figs(plot_dir)
#            for fname in os.listdir(plot_dir):
#                filename = os.path.join(plot_dir, fname)
#                try:
#                    if fname.lower().endswith('.svg'):
#                        im = SVG(filename)
#                    else:
#                        im = Image(filename)
#                    self.Display(im)
#                except Exception as e:
#                    self.Error(e)

    def get_kernel_help_on(self, info, level=0, none_on_fail=False):
        obj = info.get('help_obj', '')
        if not obj or len(obj.split()) > 1:
            if none_on_fail:
                return None
            else:
                return ""
        resp = self.do_execute_direct('? %s' % obj)
        return resp

    def get_completions(self, info):
        """
        Get completions from kernel based on info dict.
        """
        resp = ""
        return resp

    def handle_plot_settings(self):
        pass

    def _make_figs(self, plot_dir):
            oldcmd = """
            figHandles = get(0, 'children');
            for fig=1:length(figHandles);
                h = figHandles(fig);
                filename = fullfile('%s', ['MathicsFig', sprintf('%%03d', fig)]);
                saveas(h, [filename, '.%s']);
                close(h);
            end;
            """ % (plot_dir, self._plot_fmt)
#            super(MathicsKernel, self).do_execute_direct(cmd.replace('\n', ''))

if __name__ == '__main__':
    from IPython.kernel.zmq.kernelapp import IPKernelApp
    IPKernelApp.launch_instance(kernel_class=MathicsKernel)
