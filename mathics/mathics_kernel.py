from __future__ import print_function

from metakernel import MetaKernel, ProcessMetaKernel, REPLWrapper, u
from IPython.display import Image, SVG
from IPython.display import Latex,HTML

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
Switch[Head[#1],
	  Null,
		res={Null,\"\"},
          Graphics,            
            fn=\"{sessiondir}/session-figure\"<>ToString[$Line]<>\".svg\";
            Export[fn,#1,"SVG"];
            res={\"svg:\"<>fn,\"- graphic -\"},
          Graphics3D,            
            fn=\"{sessiondir}/session-figure\"<>ToString[$Line]<>\".jpg\";
            Export[fn,#1,"JPG"];
            res={\"image:\"<>fn,\"- graphic3d -\"},
          Sound,
            fn=\"{sessiondir}/session-sound\"<>ToString[$Line]<>\".wav\";
            Export[fn,#1,"wav"];
            res={\"sound:\"<>fn,\"- sound -\"},
          _,            
            res={ToString[TeXForm[#1]],ToString[InputForm[#1]]}
       ];
    \"['\"<> res[[1]]<>\"', '\"<>res[[2]]<>\"']\"
    ]&;
$DisplayFunction=Identity;
"""

    _session_dir=""
    _first = True
    initfilename=""
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
        self._initstring=self._initstring.replace("{sessiondir}",self._session_dir)
        self.initfilename=self._session_dir+'/init.m'
        print(self.initfilename)
        f= open(self.initfilename,'w')
        f.write(self._initstring)
        f.close()
        replwrapper=REPLWrapper(self.language_info['exec'], orig_prompt, change_prompt,
                           prompt_emit_cmd=prompt_cmd,echo=True)

        return replwrapper

    def do_execute_direct(self, code):
        if(self._first):
            super(MathicsKernel, self).do_execute_direct("Get[\""+ self.initfilename+"\"]")
            self._first=False            
        resp = super(MathicsKernel, self).do_execute_direct("$MyPrePrint["+code+"]")
        while(True):
            if(resp[0]!=u'='):
                resp=resp[1:]
                continue
            resp=resp[1:]
            break
        linresp=resp.splitlines()
        resp=u''
        for i,l in enumerate(linresp):
            if l==u' ':
                if len(resp)==0:
                    continue
                else:
                    if  resp[-1]==u'\\' and linresp[i+1][0]==u'>':
                        resp=resp[:-1]
                        linresp[i+1]=linresp[i+1][1:]
                        continue                    
            else:
                resp=resp+l
        while(resp[0]==u'>' or resp[0]==u' '):
            resp=resp[1:]
        i=0

        output=None

        openbra=False
        initpos=0
        for i in xrange(len(resp)-1):
            if (not openbra) and resp[i-1]==u"[" and resp[i]==u"'":
                initpos=i+1
                openbra=True
                continue
            if openbra and resp[i]==u"'" and resp[i-1]!=u'\\' and resp[i+1]==u',': 
                finpos=i
                output=resp[initpos:finpos]
                break
        if(output[:4]=='Null'):
            return ""           
        if(output[:4]=='svg:'):
            self.Display(SVG(output[4:]))
            return resp
        if(output[:6]=='image:'):
            self.Display(Image(output[6:]))
            return resp
        else:
            if(output[:6]=='sound:'): 
		htmlstr="<audio controls> <source src=\"file://"
		htmlstr=htmlstr+output[6:]
		htmlstr=htmlstr+ "\" type=\"audio/wav\">"
		htmlstr=htmlstr+"Your browser does not support the audio element."
		htmlstr=htmlstr+"</audio>"
		self.Display(HTML(htmlstr))
                return htmlstr
            else:
                self.Display(Latex("$"+output+"$"))
                if self.language_info['exec']=='mathics':
                    return resp[finpos+4:-7]
                else:
                    return resp[finpos+4:-6]
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















