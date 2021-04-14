import Tkinter
import math
import RectDia
def _l(x,y,x2,y2,dist,fig):
    fig.create_line(x*dist+dist/2,y*dist+dist/2,x2*dist+dist/2,y2*dist+dist/2,width=10,fill="#000")
    fig.create_line(x*dist+dist/2,y*dist+dist/2,x2*dist+dist/2,y2*dist+dist/2,width=6,fill="#f00")
    
def drawRectDia(diag,fig):
    n=diag.getSize()
    dist=500/n
    diag.orderPoints(1)
    diag.orderPoints(0)##the sort is order preserving for equals!!
    
    for x in range(n):
        _l(diag.points[2*x].x,diag.points[2*x].y,diag.points[2*x+1].x,diag.points[2*x+1].y,dist,fig)
    diag.orderPoints(0)
    diag.orderPoints(1)##the sort is order preserving for equals!!
    for x in range(n):
        _l(diag.points[2*x].x,diag.points[2*x].y,diag.points[2*x+1].x,diag.points[2*x+1].y,dist,fig)

def drawRectDia2(diag,fig):
    for o in fig.find_all():
        fig.delete(o)
    diag=diag.toRectDia()
    n=diag.getSize()
    dist=500/n
    diag.orderPoints(1)
    diag.orderPoints(0)##the sort is order preserving for equals!!
    
    for x in range(n):
        _l(diag.points[2*x].x,diag.points[2*x].y,diag.points[2*x+1].x,diag.points[2*x+1].y,dist,fig)
    diag.orderPoints(0)
    diag.orderPoints(1)##the sort is order preserving for equals!!
    for x in range(n):
        _l(diag.points[2*x].x,diag.points[2*x].y,diag.points[2*x+1].x,diag.points[2*x+1].y,dist,fig)
        
if __name__ == "__main__":
    root=Tkinter.Tk()
    fig=Tkinter.Canvas(root,width=500,height=500)
    fig.pack()
    root.mainloop()      
