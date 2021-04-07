import OLink
###################### first some plane geometry needed later
constradius=1## when 2 points are nearer they are considered identical!
def dist(p1,p2):## the squared distance between 2 points
    return (p1[0]-p2[0])*(p1[0]-p2[0])+(p1[1]-p2[1])*(p1[1]-p2[1])
def distToLine(p,l):##distance between the point p and the segment l
    from math import sqrt
    k1=sqrt(dist(p,l[0])*1.0)
    k2=sqrt(dist(p,l[1])*1.0)
    k=sqrt(dist(l[0],l[1])*1.0)
    peri=(k1+k2+k)/2
    h=(sqrt(peri*(peri-k1)*(peri-k2)*(peri-k))*2)/k
    if k1*k1-h*h>k*k or k2*k2-h*h>k*k : return min((k1,k2))
    return h
def intersect(l1,l2):## intersection of line or segments return coordinates and proportion on segements
    d1=(l1[1][0]-l1[0][0])*(l2[1][1]-l2[0][1])-(l1[1][1]-l1[0][1])*(l2[1][0]-l2[0][0]) ##determinant
    if d1==0: return 0 ##if paralleles
    gx=l2[0][0]-l1[0][0]
    gy=l2[0][1]-l1[0][1]
    ds=gx*(l2[1][1]-l2[0][1])-gy*(l2[1][0]-l2[0][0])
    dt=((l1[1][0]-l1[0][0])*gy-(l1[1][1]-l1[0][1])*gx)
    return(l1[0][0]+(ds*(l1[1][0]-l1[0][0]))/d1,l1[0][1]+(ds*(l1[1][1]-l1[0][1]))/d1,ds*1.0/d1,-dt*1.0/d1)
###################### The transformation in a link itself
def underOn(l1,l2,dx,dy):## of two (intersecting) line find which is like / and \
##    def ori(l):
##        return l[0][0]*dx+l[0][1]*dy<l[1][0]*dx+l[1][1]*dy
##    l1p=l1[:]
##    l2p=l2[:]
##    if ori(l1): l1p=(l1[1],l1[0])
##    if ori(l2): l2p=(l2[1],l2[0])
##    (dx,dy)=(-dy,dx)
##    return ori((l1p[0],l2p[0]))
    l1p=l1[:]
    l2p=l2[:]
    if l1[1][1]>l1[0][1]: l1p=(l1[1],l1[0])
    if l2[1][1]>l2[0][1]: l2p=(l2[1],l2[0])
    return (l1[0][0]-l1[1][0])/(l1[0][1]-l1[1][1])>(l2[0][0]-l2[1][0])/(l2[0][1]-l2[1][1])
def locate(x,y,dx,dy,l1,l2,lines):
    res=0
    for l in lines:
        if l==l1 or l==l2: continue
        k=intersect(((x,y),(x-dy,y+dx)),l)
        if k[3]>=0 and k[3]<=1 and k[2]>0:res+=1
    return res
def uOrn(v1x,v1y,v2x,v2y,dx,dy):
    d1=v1x*dx+v1y*dy
    d2=v2x*dx+v2y*dy
    if d1>0 and d2<0:return 3
    if d1<0 and d2>0:return 2
    return 0
## new underOne

## interpreting a set of lines as the diagram of a link!
def linePreparations(lines):
    expansion=len(lines)*10
    from pairing import minPairing
    infty=100000000
    matrix=[]
    for i in range(len(lines)):
        matrix.append(len(lines)*[0])
    for i in range(len(lines)):
        for j in range(len(lines)):
            matrix[i][j]=dist(lines[i][1],lines[j][0])
            if i==j: matrix[i][j]=infty
    pairing=minPairing(matrix)
    newLines=[]
    for i in range(len(lines)):
        x1=(lines[i][1][0]+lines[pairing[i]][0][0])/2
        y1=(lines[i][1][1]+lines[pairing[i]][0][1])/2
        newLines.append([[0,0],[x1,y1]])
    for i in range(len(lines)):
        newLines[pairing[i]][0][0]=(lines[i][1][0]+lines[pairing[i]][0][0])/2
        newLines[pairing[i]][0][1]=(lines[i][1][1]+lines[pairing[i]][0][1])/2
    return newLines
def lineToOLink(lines):
    res=[]
    vertex=[]
    inter=[]
    dx=0
    dy=10
    entry=len(lines)
    for i in range(len(lines)):
        for j in range(i):
            l1=lines[i]
            l2=lines[j]
            tmp=intersect(l1,l2)
            if dist(l1[1],l2[0])<constradius*constradius:
                entry-=1
                boo=uOrn(l1[1][0]-l1[0][0],l1[1][1]-l1[0][1],l2[1][0]-l2[0][0],l2[1][1]-l2[0][1],dx,dy)
                if boo:
                    vertex.append((l1[1][0],l1[1][1],locate(l1[1][0],l1[1][1],dx,dy,l1,l2,lines),boo))
                continue
            if dist(l1[0],l2[1])<constradius*constradius:
                entry-=1
                boo=uOrn(l2[1][0]-l2[0][0],l2[1][1]-l2[0][1],l1[1][0]-l1[0][0],l1[1][1]-l1[0][1],dx,dy)
                if boo:
                    vertex.append((l2[1][0],l2[1][1],locate(l2[1][0],l2[1][1],dx,dy,l1,l2,lines),boo))
                continue
            if tmp[2]>=0.0 and tmp[2]<=1.0 and  tmp[3]>=0.0 and tmp[3]<=1.0:
                loc=locate(tmp[0],tmp[1],dx,dy,l1,l2,lines)
##                boo=0
##                if underOn(l1,l2,dx,dy)==0:boo=1-boo
##                if (l1[0][1]>l2[0][1] and boo) or (l1[0][1]<l2[0][1] and 1-boo): 
##                    inter.append((tmp[0],tmp[1],loc,1))
##                else:
##                    inter.append((tmp[0],tmp[1],loc,0))
##                    
                if underOn(l1,l2,dx,dy): 
                    inter.append((tmp[0],tmp[1],loc,1))
                else:
                    inter.append((tmp[0],tmp[1],loc,0))
    t=[(x[2],x[3],x[0]*dx+x[1]*dy) for x in vertex]+[(x[2],x[3],x[0]*dx+x[1]*dy) for x in inter]
    t.sort(cmp=lambda x,y :cmp(x[2],y[2]))
    print t
    return OLink.OLink([(x[1],x[0]) for x in t],entry)



################################# a gui to draw link and "read" them
import Tkinter
class inputWindow(Tkinter.Frame):
    def drawArrow(self,l):
        self.board.create_line (l[0][0],l[0][1],l[1][0],l[1][1],width=10,arrow=Tkinter.LAST,fill="#000")
        from math import sqrt
        coef=sqrt(1.0*dist(l[0],l[1]))
        coef=(coef-10)/coef## =arrow head!!!!!!!!
        self.board.create_line (l[0][0],l[0][1],int(l[0][0]+coef*(l[1][0]-l[0][0])),int(l[0][1]+coef*(l[1][1]-l[0][1])),width=6,fill="#40f")
    
    def __init__(self, master=None):
        Tkinter.Frame.__init__(self, master)
        self.pack()
        self.board=Tkinter.Canvas(self,width=1000,height=500)
        self.board.pack()
        self.loadButton=Tkinter.Button(self,text=" Load Link ",command=self.loadLink)
        self.loadButton.pack()
        self.result=0
        self.lines=[]
        self.depth=0
        self.board.bind("<Button-1>",self.mousePr)
        self.board.bind("<Button-3>",self.mouseDe)
        self.board.bind("<ButtonRelease-1>",self.mouseRe)
    def mousePr(self,event):
        (self.fromX,self.fromY)=(event.x,event.y)
    def mouseRe(self,event):
        if dist((self.fromX,self.fromY,self.depth),(event.x,event.y))<constradius:
            return
        self.depth+=1
        self.lines.append(((self.fromX,self.fromY,self.depth),(event.x,event.y,self.depth)))
        self.drawArrow(((self.fromX,self.fromY,self.depth),(event.x,event.y)))
    def mouseDe(self,event):
        p=(event.x,event.y)
        dis=constradius*2000
        select=-1
        for i in range(len(self.lines)):
            if distToLine(p,self.lines[i])<dis:
                select=i
                dis=distToLine(p,self.lines[i])
        if select!=-1:
            for i in self.board.find_all():
                self.board.delete(i)
            self.lines[select:select+1]=[]
            for i in self.lines:
                self.drawArrow(i)
    def loadLink(self):
        self.lines=linePreparations(self.lines)
        for i in self.board.find_all():
                self.board.delete(i)
        for i in self.lines:
            self.drawArrow(i)
        self.result=lineToOLink(self.lines)
        self.event_generate("<<loadLink>>")
## main and tests
def inputLink():
    win=inputWindow()
    win.master.title("Link drawing program")
    win.mainloop()
    return win.result            
if __name__ == "__main__":
    k=inputLink()
    k.draw()

