import Tkinter
import math
def drawRibbon(x1,y1,x2,y2,width,fill,fig):
    dx=x2-x1
    dir=1
    if dx<0:
        dir=-1
    dy=y2-y1
    color='#%d%d%d'%(0,0,0)
    fig.create_line(x1-width,y1,x2-width,y2,fill=color)
    fig.create_line(x1-width+1,y1,x2-width+1,y2,fill=color)
    
    fig.create_line(x1+width,y1,x2+width,y2)
    fig.create_line(x1+width-1,y1,x2+width-1,y2)
    color=fill
    for k in range(-width+2,width-1):
        fig.create_line(x1+k,y1,x2+k,y2,fill=color)
    return
def drawArch(x1,x2,y,height,fill,fig):
    fig.create_line(x1,y,x1,y+height,fill=fill)
    fig.create_line(x2,y,x2,y+height,fill=fill)
    fig.create_line(x1,y+height,x2+1,y+height,fill=fill)
def drawEnd(x1,x2,y,height,width,fill,fig):
    dir=1
    if height<0: dir=-1
    for k in range(-width,width+1):
        if k<-width+2 or k>width-2:
            color='#%d%d%d'%(0,0,0)
        else:
            color=fill
            
        drawArch(x1-k,x2+k,y,height+k*dir,color,fig)
def drawBraidElem(x1,y1,x2,y2,entry,elem,pos,width,fill,fig):
    d1=(max(x2,x1)-min(x2,x1))/(entry+1)
    out=entry
    if elem==2: out+=2
    if elem==3: out-=2
    d2=(max(x2,x1)-min(x2,x1))/(out+1)
    high=0
    low=0
    if elem==0:
        drawRibbon(x1+d1*(pos+1+1),y1,x1+d2*(pos+1),y2,width,fill,fig)  
        drawRibbon(x1+d1*(pos+1),y1,x1+d2*(pos+1+1),y2,width,fill,fig)  
    if elem==1:
        drawRibbon(x1+d1*(pos+1),y1,x1+d2*(pos+1+1),y2,width,fill,fig)  
        drawRibbon(x1+d1*(pos+1+1),y1,x1+d2*(pos+1),y2,width,fill,fig)  
    if elem==2:
        drawEnd(x1+d2*(pos+1),x1+d2*(pos+2),y2,-(y2-y1)/3,width,fill,fig)
    if elem==3:
        drawEnd(x1+d1*(pos+1),x1+d1*(pos+2),y1,(y2-y1)/3,width,fill,fig)
    while(high<entry):
        if high==pos:
            if elem==2 or elem<2:
                low+=2
            if elem==3 or elem<2:
                high+=2
            pos=-1
            continue
        drawRibbon(x1+d1*(high+1),y1,x1+d2*(low+1),y2,width,fill,fig)
        high+=1
        low+=1
def drawBraid(x1,y1,x2,y2,braid,entry,width,fig):
    if len(braid)==0:
        drawBraidElem(x1,y1,x2,y2,entry,-1,-1,width,'#%d%d%d'%(9,9,0),fig)
        return
    dy=(y2-y1)/len(braid)
    for i in range(len(braid)):
        drawBraidElem(x1,y1+dy*i,x2,y1+dy*(i+1),entry,braid[i][0],braid[i][1],width,'#%d%d%d'%(9,9,0),fig)
        if braid[i][0]==2: entry+=2
        if braid[i][0]==3: entry-=2
def goodDivisor(n):
    p=1
    for k in range(int(math.sqrt(n)),n+1):
        if n % k==0: return k
def drawBraidSerie(x1,y1,x2,y2,s,width,fig):
    n=len(s)
    d=goodDivisor(n)
    dx=(x2-x1)/d
    dy=(y2-y1)*d/n
    for x in range(d):
        for y in range(n/d):
            drawBraid(x1+dx*x,y1+dy*y,x1+dx*(x+1),y1+dy*(y+1),s[d*y+x].word,s[d*y+x].entry,width,fig)
def drawRectDiag(diag,fig):
    
if __name__ == "__main__":
    root=Tkinter.Tk()
    fig=Tkinter.Canvas(root,width=500,height=500)
    fig.pack()
##    drawRibbon(200,0,60,100,10,'#%d%d%d'%(9,0,0),fig)
##    drawRibbon(60,0,200,100,10,'#%d%d%d'%(9,0,0),fig)
##    drawEnd(60,200,100,-50,10,'#%d%d%d'%(9,0,0),fig)
##    drawBraidElem(10,10,500,50,8,1,2,7,'#%d%d%d'%(9,0,0),fig)
    drawBraid(10,10,500,500,[(2,0),(2,1),(1,0),(1,0),(1,2),(3,1),(3,0)],0,6,fig)
    root.mainloop()      
