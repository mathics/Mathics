from genGen import genGen
from genGen import gen
from generators import classifiedGen
import combinatoric
import rectDiagMisc
import homology

reload(homology)
reload(combinatoric)

####################bnd map
def isRectPunc(diag,ax,ay,bx,by):
    for i in xrange(ax,bx):
        if diag[i][0]<by and diag[i][0]>=ay:
            return 1
        if diag[i][1]<by and diag[i][1]>=ay:
            return 1
    return 0
def isRectGen(gen,a,b):##gen is is before the map! a<b
    y1=gen.perm[a]
    y2=gen.perm[b]
    for i in xrange(a+1,b):
        l=gen.perm[i]
        if l>y1 and  l<y2:
            return 1
    return 0

def isBndryLargeEll(rect,gen1,gen2):
    n=len(gen1.perm)
    diff=[]
    for i in xrange(n):
        if gen1.perm[i]!=gen2.perm[i]:
            diff.append(i)
    l=len(diff)
    if l>2:
        return 0
    if l==0:
        diffxs=[]
        diffys=[]
        for i in xrange(n):
            if gen1.xShift[i]!=gen2.xShift[i]:
                diffxs.append(i)
            if gen1.yShift[i]!=gen2.yShift[i]:
                diffys.append(i)
        if len(diffxs)+len(diffys)!=1: return 0

        if len(diffxs)==1:
            delta=diffxs[0]
            y=gen1.perm[delta]+(gen1.yShift[delta]+1)/2
            if y>rect[delta][1] and gen1.xShift[delta]<gen2.xShift[delta] or y<=rect[delta][0] and gen1.xShift[delta]>gen2.xShift[delta]:
                return 1
            else: return 0
        else:
            xx=[]
            delta=diffys[0]
            for i in xrange(n):
                if rect[i][0]==gen1.perm[delta] or rect[i][1]==gen1.perm[delta]:
                    xx.append(i)
            x=delta+(gen1.xShift[delta]+1)/2
            if x<=xx[0] and gen1.yShift[delta]>gen2.yShift[delta] or x>xx[1] and gen1.yShift[delta]<gen2.yShift[delta]:
                return 1
            else: return 0
        
    if l==2:
        if gen1.perm[diff[0]]>gen1.perm[diff[1]]: return 0
        for i in xrange(n):
            if gen1.xShift[i]!=gen2.xShift[i] or gen1.yShift[i]!=gen2.yShift[i]:
                if i!=diff[0] and i!=diff[1]:
                    return 0
        (a,b)=diff
        if gen1.xShift[a]!=gen2.xShift[a] or gen1.xShift[b]!=gen2.xShift[b] or gen1.yShift[a]!=gen2.yShift[b] or gen2.yShift[a]!=gen1.yShift[b]:
            return 0
        if isRectGen(gen1,a,b) or isRectPunc(rect,a+(gen1.xShift[a]+1)/2,
                                             gen1.perm[a]+(gen1.yShift[a]+1)/2,
                                             b+(gen1.xShift[b]+1)/2,
                                             gen1.perm[b]+(gen1.yShift[b]+1)/2):
            return 0
        return 1
    if l==1:
        raise Error,"1 of diff"
    raise Error,"error in boundary"

if __name__ == "__main__":
    ##rect=[[0,2],[1,3],[0,2],[1,3]]
    ##rect=[[0,2],[1,3],[0,2],[1,3]]
    rect=[[0,1],[0,1]]

    rect=[[1,4],[0,2],[1,3],[2,4],[0,3]]
##    rect=[[0,1],[0,2],[1,2]]

    
##    a=gen([-1, 4, 3, 0, 2], [0, -1, -1, 1, -1], [0, 1, 1, 1, -1])
##    b=gen([-1, 4, 3, 0, 2], [0, -1, 1, 1, -1], [0, 1, 1, 1, -1])
##    print isBndryLargeEll(rect,a,b)


    tmp=classifiedGen(rect,0)[0]
    print "second phase"
    import profile
    profile.run("tmp=homology.chain2DToHomv2(tmp,lambda x,y:isBndryLargeEll(rect,x,y))")
##    tmp=homology.chain2DToHomv2(tmp,lambda x,y:isBndryLargeEll(rect,x,y))
    
    def format(s,l):
        if len(s)<l:
            return " "*(l-len(s))+s
        return s
    for i in tmp:
        s=""
        for j in i:
            s+=format(str(j),5)+" "
        print s
    

