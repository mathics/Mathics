import Tkinter
import visu2
class diapoUnknotting(Tkinter.Frame):
    def __init__(self, history, master=None):
        Tkinter.Frame.__init__(self, master)
        self.pack()
        self.board=Tkinter.Canvas(self,width=1000,height=500)
        self.board.pack()
        self.nextButton=Tkinter.Button(self,text=" Next ",command=self.next)
        self.previousButton=Tkinter.Button(self,text=" Previous ",command=self.previous)
        self.nextButton.pack()
        self.previousButton.pack()
        self.diapo=[]
        tmp=history[1]
        while(tmp!=0):
            self.diapo=[tmp]+self.diapo
            tmp=tmp.predecessor
        self.diapoIndex=0
        visu2.drawRectDia2(self.diapo[self.diapoIndex],self.board)
    def next(self):
        if self.diapoIndex+1==len(self.diapo):
            self.quit()
            self.destroy()
            return
        self.diapoIndex+=1
        visu2.drawRectDia2(self.diapo[self.diapoIndex],self.board)
    def previous(self):
        if self.diapoIndex!=0:
            self.diapoIndex-=1
            visu2.drawRectDia2(self.diapo[self.diapoIndex],self.board)
            
