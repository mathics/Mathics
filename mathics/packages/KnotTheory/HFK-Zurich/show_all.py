import wx
class floerDiagram(wx.Frame):
    def __init__(self,data):
        wx.Frame.__init__(self,None,title="Diagram")
        self.data=data+([],[],[])
        self.__draw()
        self.Bind(wx.EVT_SIZE, self.OnSize)
        self.Bind(wx.EVT_PAINT, self.OnPaint)
    def OnSize(self,evt):
        self.__draw()
        self.Refresh()
    def OnPaint(self,evt):
        wx.BufferedPaintDC(self, self.buffer)
    def __draw(self):
        w, h = self.GetClientSize()
        self.buffer = wx.EmptyBitmap(w, h)
        dc = wx.BufferedDC(wx.ClientDC(self), self.buffer)
        self.__show(dc,self.data[0],self.data[1],self.data[2])
    def __show(self,dc,diag,ell,genList):
        w, h = dc.GetSize()
        size=6
        n=len(diag)
        incx=w/(n+1)
        incy=h/(n+1)
        bkgColor="steel blue"
        brush=wx.Brush(bkgColor)
        dc.SetBackground(brush)
        dc.Clear()
        tmp=[[] for i in range(n)]
        for i,dp in enumerate(diag):
            for p in dp:
                tmp[p].append(i)
        for i,dp in enumerate(tmp):
            dc.SetPen(wx.Pen("black",2))
            dc.DrawLine(incx*(dp[0]+1)+size,incy*(n-i),incx*(dp[1]+1)-size,incy*(n-i))
        for i,dp in enumerate(diag):
            dc.SetPen(wx.Pen(bkgColor,size*2))
            dc.DrawLine(incx*(i+1),incy*(n-dp[0])+size,incx*(i+1),incy*(n-dp[1])-size)
            dc.SetPen(wx.Pen("black",2))
            dc.DrawLine(incx*(i+1),incy*(n-dp[0])+size,incx*(i+1),incy*(n-dp[1])-size)
            for p in dp:
                dc.SetBrush(wx.Brush("light gray"))
                dc.DrawCircle(incx*(i+1),incy*(n-p),size)
        if ell!=[]:
            for i,dp in enumerate(ell[1]):
                if dp==-1:continue
                dc.SetPen(wx.Pen("red",size*3,wx.FDIAGONAL_HATCH))
                if dp[0]<0 or dp[0]>len(ell[1]): s0=0
                else:
                    if not ell[0][dp[0]]==-1 and (ell[0][dp[0]][0]==i or ell[0][dp[0]][1]==i):s0=1
                    else:s0=0
                if dp[1]<0 or dp[1]>len(ell[1]): s1=0
                else:
                    if not ell[0][dp[1]]==-1 and (ell[0][dp[1]][0]==i or ell[0][dp[1]][1]==i):s1=1
                    else:s1=0
                dc.DrawLine(incx*(dp[0]+1)-size*s0,incy*(n-i),incx*(dp[1]+1)+size*s1,incy*(n-i))
                
            for i,dp in enumerate(ell[0]):
                if dp==-1:continue
                dc.SetPen(wx.Pen("yellow",size*3,wx.BDIAGONAL_HATCH))
                dc.DrawLine(incx*(i+1),incy*(n-dp[0])-size,incx*(i+1),incy*(n-dp[1])+size)
        if genList!=[]:
            for col,gen in enumerate(genList):
                dc.SetPen(wx.Pen(["red","yellow","pink","brown","blue","black","dark gray"][col],1))
                dc.SetBrush(wx.Brush(["red","yellow","pink","brown","blue","black","dark gray"][col]))
                for i in range(n):
                    if gen.perm[i]==-1:continue
                    dc.DrawCircle(incx*(i+1)+gen.xShift[i]*size, incy*(n-gen.perm[i])-gen.yShift[i]*size,4)
def show(*data):
    app=wx.PySimpleApp()
    frm=floerDiagram(data)
##    frm2=floerDiagram(data)
    frm.Show()
##    frm2.Show()
    app.MainLoop()
if __name__ == "__main__":
    app=wx.PySimpleApp()
    app.MainLoop()
    from genGen import gen
    while(1):
        data=eval(raw_input("?"))
        frm=floerDiagram(data)
        frm.Show()
        

##([[1, 6], [0, 2], [1, 4], [0, 3], [2, 5], [4, 7], [6, 8], [5, 7], [3, 8]],([[1, 6], [0, 2], [1, 4], [0, 3], [2, 5], [4, 7], [6, 8], [5, 7], -1], [[1, 3], [0, 2], [1, 4], -1, [2, 5], [4, 7], [0, 6], [5, 7], [6, 8]]),[gen([1, 0, 4, 2, 5, 6, 8, 7, -1],[1, -1, 1, 1, -1, 1, -1, -1, 0],[1, 1, -1, 1, -1, 1, -1, -1, 0],0),    gen([6, 1, 4, 0, 2, 5, 8, 7, -1],[-1, -1, 1, 1, -1, 1, -1, -1, 0],[-1, 1, -1, 1, 1, 1, -1, -1, 0],0)])




        
