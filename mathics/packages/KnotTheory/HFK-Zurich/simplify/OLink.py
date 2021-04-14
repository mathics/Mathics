import copy
##import visu
class OLink:
    def __init__(self,c,entry):
        self.word=copy.deepcopy(c)
        self.entry=entry
    def toStringRepr(self):
        return "OLink("+str(self.word)+","+str(self.entry)+")"
##    def draw(self):
##        import Tkinter
##        root=Tkinter.Tk()
##        fig=Tkinter.Canvas(root,width=800,height=800)
##        fig.pack()
##        visu.drawBraid(10,10,790,790,self.word,self.entry,10,fig)
##        root.mainloop() 
if __name__ == "__main__":
    k=OLink([(2,0),(3,0)],0)
    k.draw()
