import Tkinter
import tkMessageBox
class mainWindow:
    def __init__(self):
        self.root=Tkinter.Tk()
        self.root.title("Link main program")
        menuBar=Tkinter.Menu(self.root)
        self.root["menu"]=menuBar
        fileMenu=Tkinter.Menu(menuBar)
        menuBar.add_cascade(label="file",menu=fileMenu)
        fileMenu.add_command(label="exit",command=self.close)
        self.actionMenu=Tkinter.Menu(menuBar)
        menuBar.add_cascade(label="action",menu=self.actionMenu)
        self.actionMenu.add_command(label="Draw a link",command=self.drawKnot)
        self.actionMenu.add_command(label="View unknotting",command=self.viewHistory)
        self.actionMenu.add_command(label="Try to unknot",command=self.unKnot)
##        self.actionMenu.add_command(label="load drawn link",command=self.loadLink)
        self.currentInputWindows=0
        self.history=-1
        self.currentLink=-1
        self.root.mainloop()
    def close(self):
        self.root.quit()
        self.root.destroy()
    def drawKnot(self):
        import inputLink
        self.root.quit()
        if self.currentInputWindows!=0:
            self.currentInputWindows.destroy()
        self.currentInputWindows=inputLink.inputWindow(self.root)
##        self.currentInputWindows.grid(column=0,columnspan=2,row=1)
        self.currentInputWindows.bind("<<loadLink>>",self.loadLink)
        self.root.mainloop()
    def unKnot(self):
        if self.currentLink==-1:
            tkMessageBox.showinfo(title="Error!",message="No link loaded")
            return
        import RectDia
        dd=RectDia.RectDia([])
        dd.fromOlink(self.currentLink)
        import unknot
        import fastUnknot
        print dd.toString()
        tmpResult=fastUnknot.unknot(dd)
        tkMessageBox.showinfo(title="Result",message=tmpResult[0])
        self.history=tmpResult
    def loadLink(self,event):
        self.currentLink=self.currentInputWindows.result
        self.currentInputWindows.destroy()
    def viewHistory(self):
        if self.history==-1:
            tkMessageBox.showinfo(title="Error!",message="No history loaded")
            return
        if self.currentInputWindows!=0:
            self.currentInputWindows.destroy()
        import diapoUnknotting
        self.root.quit()
        self.currentInputWindows=diapoUnknotting.diapoUnknotting(self.history,self.root)
        self.root.mainloop()
if __name__ == "__main__":
    import profile
##    profile.run("
    mainWindow()
##    ")
