import rectDiagMisc
import generators
from genGen import gen
from copy import deepcopy
from math import atan2
def maslovIndex(g,rect):
    (p1,p2)=rectDiagMisc.recToPermAndComp(rect)[0]
    tabMas=generators.maslovTab(p1)
    ipp=generators._I(p1,p1)
    g.maslov=generators._I(g.perm,g.perm)
    return generators.maslovIndex2(g,tabMas,ipp)
def isSimplePermComp(p1,p2,trans1,trans2,startx):##compute the size and whitney nb of a cycle of a perm
    x=startx
    y=p1[x]
    whitney=0
    compSize=1
    if trans2[y]<trans1[y]: dy=1
    else: dy=-1
    while(1):
        if p1[x]<p2[x]: dx=1
        else: dx=-1
        whitney-=dx*dy
        if p1[x]==y: y=p2[x]
        else: y=p1[x]
        if trans2[y]<trans1[y]: dy=1
        else: dy=-1
        whitney+=dx*dy
        if trans1[y]==x: x=trans2[y]
        else: x=trans1[y]
        if x==startx:break
        compSize+=1
    return (whitney,compSize)
##print isSimplePermComp([2,1,0],[0,1,2],[2,1,0],[0,1,2],0)
def detectSimple(n,gen1,gen2,ellDir):##1=accepted 0 don't know -1 refused
    nbMobile=0
    trans1=[-1]*n
    trans2=[-1]*n
    for i in xrange(n):
        if gen1.perm[i]!=gen2.perm[i]:
            nbMobile+=1
    if nbMobile==0:
        nbSMobile=0
        for i in xrange(n):
            if gen1.xShift[i]!=gen2.xShift[i] or gen1.yShift[i]!=gen2.yShift[i]:
                nbSMobile+=1
            else:
                if ellDir[0][i]==-1 or ellDir[1][gen1.perm[i]]==-1:
                    return 0
        if nbSMobile==1:
            return 1 ## accepting because of bdMG verification only
        else:
            return 0
    if nbMobile==2:##rectangle, to elaborate for replacing version
        for i in xrange(n):
            if gen1.perm[i]==gen2.perm[i] and (ellDir[0][i]==-1 or gen1.xShift[i]!=gen2.xShift[i] or gen1.yShift[i]!=gen2.yShift[i]):##hole
                return 0
            trans1[gen1.perm[i]]=i
            trans2[gen2.perm[i]]=i
        for i in xrange(n):
            if trans1[i]==trans2[i] and ellDir[1][i]==-1:
                return 0
        ##at this point it is a possibly horny rectangle
        return 1 ## accepting because of bdMG verification only
    else:##nbMobile >2 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        for i in xrange(n):
            if gen1.perm[i]==gen2.perm[i] and (ellDir[0][i]==-1 or gen1.xShift[i]!=gen2.xShift[i] or gen1.yShift[i]!=gen2.yShift[i]):##hole
                return 0
            trans1[gen1.perm[i]]=i
            trans2[gen2.perm[i]]=i
        start=-1#is there only one component? (preparation)
        for i in xrange(n):
            if trans1[i]==trans2[i]:
                if ellDir[1][i]==-1:
                    return 0
            else: start=trans1[i]
        #is there only one component?
        tmp=isSimplePermComp(gen1.perm,gen2.perm,trans1,trans2,start)
        if tmp[1]==nbMobile and tmp[0]==4: return 1
        else: return 0
def decideOrdering(ell):
    ordering=[]
    for b in [0,1]:
        for i,e in enumerate(ell[b]):
            if e!=-1:
                ordering.append((b,i,1))
                ordering.append((b,i,-1))
    ordering.reverse()
    return ordering
def cPj(x,y,xs,ys,cx,cy):
    x=x+xs*0.3-cx
    y=y+ys*0.3-cy
    return -atan2(x,y)##zero on top positiveness is ok
def isBetween(a1,a2,a3):
    return a1<=a2<=a3 or a3<=a1<=a2 or a2<=a3<=a1
def isThrough(vh,cx,cy,full,p1,p2,g,d):
##    print cx,cy,full,p1,p2,g,d
    a=cPj(p1[0],p1[1],p1[2],p1[3],cx,cy)
    c=cPj(p2[0],p2[1],p2[2],p2[3],cx,cy)
    b=cPj(g[0],g[1],g[2],g[3],cx,cy)
##    print a,b,c
    if (vh==1)^(full==1):##1 is horizontal
        a,c=c,a
    if isBetween(a,b,c):
        if d==0 or d==2:
            if (d==0)^(full==-1)^(g[2]==-1): return -1
            else: return 1
        else:
            if (d==1)^(full==-1)^(g[3]==-1): return -1
            else: return 1
    return 0
def findNextBigonList(rect,gen1,gen2,ell,ellDir,ext,trans1,trans2):
    n=len(rect)
    if ext[0]==0:
        if ext[2]==1:
            pairs=[(ext[1],ell[0][ext[1]][1],-1,1,1,1)]##x, y, xShift, y shift
            for i in xrange(ell[0][ext[1]][1]+1,n):
                for b in [-1,1]:
                    pairs.append((ext[1],i,-1,b,1,b))
        else:
            pairs=[(ext[1],ell[0][ext[1]][0],-1,-1,1,-1)]##x, y, xShift, y shift
            for i in xrange(0,ell[0][ext[1]][0]):
                for b in [-1,1]:
                    pairs.append((ext[1],i,-1,b,1,b))
        res=[]
        for p in pairs:
            ##premiere selection -any ellipse,-on the ellipse?
            if ell[1][p[1]]==-1 or p[0]<ell[1][p[1]][0] or p[0]>ell[1][p[1]][1]: continue
            cx,cy=(ell[1][p[1]][0]+ell[1][p[1]][1])*0.5, p[1]
            a=cPj(trans1[p[1]],p[1],gen1.xShift[trans1[p[1]]],gen1.yShift[trans1[p[1]]],cx,cy)
            c=cPj(trans2[p[1]],p[1],gen2.xShift[trans2[p[1]]],gen2.yShift[trans2[p[1]]],cx,cy)
            if a==c and ellDir[1][p[1]]==1:continue
            b1=cPj(p[0],p[1],p[2],p[3],cx,cy)
            b2=cPj(p[0],p[1],p[4],p[5],cx,cy)
            if ellDir[1][p[1]]==-1:
                a,c=c,a
            if isBetween(a,b1,c) and isBetween(a,b2,c): res.append(p)
    else:
        if ext[2]==1:
            pairs=[]##x, y, xShift, y shift
            for i in xrange(ell[1][ext[1]][1]+1,n):
                if ell[1][ext[1]]!=-1:
                    for b in [-1,1]:
                        pairs.append((i,ext[1],b,-1,b,1))
        else:
            pairs=[]##x, y, xShift, y shift
            for i in xrange(0,ell[1][ext[1]][0]):
                for b in [-1,1]:
                    pairs.append((i,ext[1],b,-1,b,1))
        res=[]
        for p in pairs:
            if  ell[0][p[0]]==-1 or p[1]<ell[0][p[0]][0] or p[1]>ell[0][p[0]][1]: continue
            cx,cy=p[0],(ell[0][p[0]][0]+ell[0][p[0]][1])*0.5
            a=cPj(p[0],gen1.perm[p[0]],gen1.xShift[p[0]],gen1.yShift[p[0]],cx,cy)
            c=cPj(p[0],gen2.perm[p[0]],gen2.xShift[p[0]],gen2.yShift[p[0]],cx,cy)
            if a==c  and ellDir[0][p[0]]==1:continue
            b1=cPj(p[0],p[1],p[2],p[3],cx,cy)
            b2=cPj(p[0],p[1],p[4],p[5],cx,cy)
            if ellDir[0][p[0]]==1:
                a,c=c,a
            if isBetween(a,b1,c) and isBetween(a,b2,c): res.append(p)
    return [(p,ext) for p in res]
def giveDir(vh,n12,ellDir,xs,ys):#0=right 1up 2left 3down ,n12=1 or 2
    if vh==1:#0:vertical 1 horizontal
        return (-(n12*2-3)*ellDir*ys)+1
    else:
        return (-(n12*2-3)*ellDir*xs)+2
def findObtuse(rect,gen1,gen2,ell,ellDir):
    obt=[]
    flat=[]
    n=len(rect)
    for i in xrange(n):
        if gen1.perm[i]==gen2.perm[i] and gen1.xShift[i]==gen2.xShift[i] and gen1.yShift[i]==gen2.yShift[i]:#for flat angle
            if ellDir[0][i]==-1 and ellDir[1][gen1.perm[i]]==1:
                flat.append((i,gen1.perm[i],gen2.xShift[i],gen1.yShift[i],1-gen2.xShift[i]))#last number the direction
            if ellDir[0][i]==1 and ellDir[1][gen1.perm[i]]==-1:
                flat.append((i,gen1.perm[i],gen2.xShift[i],gen1.yShift[i],2-gen1.yShift[i]))
        else:
            for k,g in [(1,gen1),(2,gen2)]:#for obtuse angle
                if g.perm[i]==-1:continue
                x,y,xs,ys=i,g.perm[i],g.xShift[i],g.yShift[i]
                d1=giveDir(0,k,ellDir[0][i],xs,ys)#vertical
                d2=giveDir(1,k,ellDir[1][g.perm[i]],xs,ys)
                if (d2==(d1+1)%4 and k==1) or (d1==(d2+1)%4 and k==2):
                    obt.append((x,y,xs,ys,(d1+2)%4,(d2+2)%4))
    return obt,flat
##def elimObtuse(obtuse,rect,gen1,gen2,ell,ellDir,ext):##later, non critical
##    return obtuse
def findAllInter(ell):##must be changeed if slow ovals
    res=dict()
    n=len(ell[0])
    for i in xrange(n):
        if ell[0][i]!=-1:
            if 0<=ell[0][i][0]<n and ell[1][ell[0][i][0]]!=-1:
                res[(i,ell[0][i][0],1,1)]=1
                res[(i,ell[0][i][0],-1,1)]=1
            if 0<=ell[0][i][1]<n and ell[1][ell[0][i][1]]!=-1:
                res[(i,ell[0][i][1],-1,-1)]=1
                res[(i,ell[0][i][1],1,-1)]=1
            for y in xrange(max(ell[0][i][0]+1,0),min(n,ell[0][i][1])):
                if ell[1][y]==-1:continue
                if ell[1][y][0]<=i<=ell[1][y][1]:
                    res[(i,y,-1,1)]=1
                    res[(i,y,-1,-1)]=1
                    res[(i,y,1,1)]=1
                    res[(i,y,1,-1)]=1
    return res
def iterate(n,p,d):
    x,y,xs,ys=p
    if d==0 or d==2:
        if (xs==-1)==(d==0): xs=-xs
        else:
            if x==0  and d==2 or x==n-1  and d==0:
                d=2-d
                ys=-ys
            else:
                x+=1-d
                xs=-xs
    else:
        if (ys==-1)==(d==1): ys=-ys
        else:
            if y==0 and d==3 or y==n-1  and d==1:
                d=4-d
                xs=-xs
            else:
                y+=2-d
                ys=-ys
    return (x,y,xs,ys),d
def aim(rect,gen1,gen2,ell,ellDir,trans1,trans2,inter,x,y,xs,ys,d):
    n=len(rect)
    start=(x,y,xs,ys)
    p=start
    lvl=0
##    print "new aim"
    while(1):
        p,d=iterate(n,p,d)
##        print p,d
        if not inter.has_key(p):
##            print "skip"
            continue
        if p==start or gen1.perm[p[0]]==p[1] and gen1.xShift[p[0]]==p[2] and gen1.yShift[p[0]]==p[3]: break
        if gen2.perm[p[0]]==p[1] and gen2.xShift[p[0]]==p[2] and gen2.yShift[p[0]]==p[3]: break
        if d%2==0:
            p1=(p[0],gen1.perm[p[0]],gen1.xShift[p[0]],gen1.yShift[p[0]])
            p2=(p[0],gen2.perm[p[0]],gen2.xShift[p[0]],gen2.yShift[p[0]])
            tmp=isThrough(0,p[0],(ell[0][p[0]][0]+ell[0][p[0]][1])/2,ellDir[0][p[0]],p1,p2,p,d)
        else:
            p1=(trans1[p[1]],p[1],gen1.xShift[trans1[p[1]]],gen1.yShift[trans1[p[1]]])
            p2=(trans2[p[1]],p[1],gen2.xShift[trans2[p[1]]],gen2.yShift[trans2[p[1]]])
            tmp=isThrough(1,(ell[1][p[1]][0]+ell[1][p[1]][1])/2,p[1],ellDir[1][p[1]],p1,p2,p,d)
##        print tmp
        if tmp==1:lvl+=tmp
        if tmp==-1 and lvl==0:
            return p
    return 0
def findCuts(obtuse,flat,rect,gen1,gen2,ell,ellDir,trans1,trans2,inter):
    cuts=[]
    for a in obtuse:
        for direction in [4,5]:
                tmp=aim(rect,gen1,gen2,ell,ellDir,trans1,trans2,inter,a[0],a[1],a[2],a[3],a[direction])
        if tmp: cuts.append(((a[0],a[1],a[2],a[3],a[direction]),tmp))
    for a in flat:
        tmp=aim(rect,gen1,gen2,ell,ellDir,trans1,trans2,inter,a[0],a[1],a[2],a[3],a[4])
        if tmp: cuts.append(((a[0],a[1],a[2],a[3],a[4]),tmp))
    return cuts
def nextOnGen(p,gen1,gen2,cuts,trans1,trans2):
    res=[]
    if p[4]==0:##=doesn't come from a cut
        for c in cuts:
            if c[0]==p[0] and c[1]==p[1] and c[2]==p[2] and c[3]==p[3] and c[4]%2==0:
                ptmp=c[5]
                p=(trans1[ptmp[1]],ptmp[1],gen1.xShift[trans1[ptmp[1]]],gen1.yShift[trans1[ptmp[1]]],[ptmp],0)
                res.append(p)
    ptmp=(p[0],gen2.perm[p[0]],gen2.xShift[p[0]],gen2.yShift[p[0]])
    for c in cuts:
        if c[0]==ptmp[0] and c[1]==ptmp[1] and c[2]==ptmp[2] and c[3]==ptmp[3] and c[4]%2==1:
            ptmp2=c[5]
            p=(trans1[ptmp2[1]],ptmp2[1],gen1.xShift[trans1[ptmp2[1]]],gen1.yShift[trans1[ptmp2[1]]],[ptmp2],0)
            res.append(p)
    p=(trans1[ptmp[1]],ptmp[1],gen1.xShift[trans1[ptmp[1]]],gen1.yShift[trans1[ptmp[1]]],[],0)
    res.append(p)
    return res
def nextOnCuts(p,cuts):#cut=(x,y,xs,ys,d,(x,y,xs,ys))
    res=[]
    for c in cuts:
        if c[1][0]==p[0]:
            res.append((c[0][0],c[0][1],c[0][2],c[0][3],[c[1]],1))
    return res
##n=0
def followUntil(start,goal,gen1,gen2,cuts,trans1,trans2,g1Acc,g2Acc):##I use the []=false trick
##    global n
##    if n>10: return 0
##    n+=1
##    print "followUntil",g1Acc,g2Acc
    if start[0]==goal[0] and start[1]==goal[1] and start[2]==goal[2] and start[3]==goal[3]:
        g1Acc[goal[0]]=goal
        for intermed in start[4]:
            g2Acc[intermed[0]]=intermed
        return [(g1Acc,g2Acc)]
    next=nextOnGen(start,gen1,gen2,cuts,trans1,trans2)+nextOnCuts(start,cuts)
##    print "next:",next
    res=[]
    for p in next:
##        if p[0]==goal[0] and p[1]==goal[1] and p[2]==goal[2] and p[3]==goal[3]:
##            g1Acc[goal[0]]=goal
##            for intermed in p[4]:
##                g2Acc[intermed[0]]=intermed
##            res.append((g1Acc,g2Acc))
        if g1Acc[p[0]]:
            continue
        tmp1=g1Acc[:]
        tmp2=g2Acc[:]
        tmp1[p[0]]=p
        for intermed in p[4]:
            tmp2[intermed[0]]=intermed
        res+=followUntil(p,goal,gen1,gen2,cuts,trans1,trans2,tmp1,tmp2)
    return res
def findAll(startx,p1,p2,trans1,trans2):##p1,p2are perms as arrays
    x=startx
    y=p1[x]
    compSize=[startx]
    while(1):
        if p1[x]==y: y=p2[x]
        else: y=p1[x]
        if trans1[y]==x: x=trans2[y]
        else: x=trans1[y]
        if x==startx:break
        compSize.append(x)
    return compSize
def findHoles(gen1,gen2,trans1,trans2,ellDir,bigon):
    acc=findAll(bigon[0][0],gen1.perm,gen2.perm,trans1,trans2)
    acc+=findAll(trans1[bigon[0][1]],gen1.perm,gen2.perm,trans1,trans2)
    res=[]
    for i in xrange(len(gen1.perm)):
        if gen1.perm[i]==-1:continue
        if gen1.perm[i]==gen2.perm[i] and gen1.xShift[i]==gen2.xShift[i] and gen1.yShift[i]==gen2.yShift[i] and ellDir[0][i]==1 and ellDir[1][gen1.perm[i]]==1:
            continue
        if acc.count(i)==0:
            m=findAll(i,gen1.perm,gen2.perm,trans1,trans2)
            res.append((i,m))
    return res
def isIn(g2,path,x,y):
    p=0
    for k in xrange(x):
        if path[k]:
            if g2[k]>=y:p+=1
            if path[k][1]>=y:p-=1
    return p
def splitGens(gen1,gen2,trans1,trans2,ellDir,g1Path,g2Acc,bigon):##g2Acc contains a point of bigon (the one of g2)
    newEllDir=deepcopy(ellDir)
    newEllDirp=deepcopy(ellDir)
    holes=findHoles(gen1,gen2,trans1,trans2,ellDir,bigon)
    isH=[0]*len(gen1.perm)
    coPath=gen2.perm[:]
    coPath[bigon[0][0]]=bigon[0][1]
    for h in holes:
        tmp=2
        if isIn(coPath,g1Path,h[0],gen1.perm[h[0]]):
            tmp=1
        for x in h[1]:
            isH[x]=tmp
    g1=gen1
    g2=gen([],[],[],0)
    g1p=gen([],[],[],0)
    g2p=gen2
    for i in xrange(len(gen1.perm)):
        tmp1=g1Path[i]
        tmp2=g2Acc[i]
        if tmp2:
            g2.perm.append(tmp2[1])
            g2.xShift.append(tmp2[2])
            g2.yShift.append(tmp2[3])
            g1p.perm.append(tmp2[1])
            if bigon[1][0]==0 and i==bigon[1][1] or bigon[1][0]==1 and tmp2[1]==bigon[1][1]:###unfinished
                g1p.xShift.append(tmp2[2]*(bigon[1][0]*2-1))
                g1p.yShift.append(tmp2[3]*(1-bigon[1][0]*2))
            else:
                g1p.xShift.append(tmp2[2])
                g1p.yShift.append(tmp2[3])
        else:
            if isH[i]==2 or isH[i]==0 and not tmp1:
                gg=gen1
                if newEllDir[0][i]!=0: newEllDir[0][i]=1
                if newEllDir[1][gen1.perm[i]]!=0: newEllDir[1][gen1.perm[i]]=1
            else:
                gg=gen2
                if newEllDirp[0][i]!=0: newEllDirp[0][i]=1
                if newEllDirp[1][gen2.perm[i]]!=0: newEllDirp[1][gen2.perm[i]]=1
            g2.perm.append(gg.perm[i])
            g2.xShift.append(gg.xShift[i])
            g2.yShift.append(gg.yShift[i])
            g1p.perm.append(gg.perm[i])
            g1p.xShift.append(gg.xShift[i])
            g1p.yShift.append(gg.yShift[i])
            
    
##    g1.show()
##    g2.show()
##    g1p.show()
##    g2p.show()
##    print "fin"
    return ((g1,g2,newEllDir),(g1p,g2p,newEllDirp))

def splitting(rect,gen1,gen2,ell,ellDir,trans1,trans2,p):
    n=len(rect)
    print "split",p
    inter=findAllInter(ell)
    obtuse,flat=findObtuse(rect,gen1,gen2,ell,ellDir)
    cuts=findCuts(obtuse,flat,rect,gen1,gen2,ell,ellDir,trans1,trans2,inter)
    print "cuts:",cuts
    pairs=[]
    if p[1][0]==0:
        tmp=trans1[p[0][1]]
        start=(tmp,gen1.perm[tmp],gen1.xShift[tmp],gen1.yShift[tmp],[])
        goal=(p[1][1],gen1.perm[p[1][1]],gen1.xShift[p[1][1]],gen1.yShift[p[1][1]])
        supp=(p[0][0],p[0][1],p[1][2],p[0][3])
    else:
        tmp=trans1[p[0][1]]
        goal=(tmp,gen1.perm[tmp],gen1.xShift[tmp],gen1.yShift[tmp])
        start=(p[0][0],gen1.perm[p[0][0]],gen1.xShift[p[0][0]],gen1.yShift[p[0][0]],[])
        supp=(p[0][0],p[0][1],p[0][2],p[1][2])
    print "start,goal,supp:",start,goal,supp
    g1Acc=[0]*n
    g2Acc=[0]*n
    g1Acc[start[0]]=start
    separation=followUntil(start,goal,gen1,gen2,cuts,trans1,trans2,g1Acc,g2Acc)
    print "sep",separation
    for g1Path,g2Acc in separation:
        g2Acc[supp[0]]=supp
        pairs.append(splitGens(gen1,gen2,trans1,trans2,ellDir,g1Path,g2Acc,p))
    return pairs
    
##debug
import wx
import show_all
app=wx.PySimpleApp()
totalG=0
##end debug

def deepBdMap(rect,gen1,gen2,ell,ellDir,ordering=0):
##    print "dbdm",totalG
##    return 1
    n=len(rect)
    if detectSimple(n,gen1,gen2,ellDir)==1:
##        print "ok direct"
        return 1
    global totalG
##    if totalG>20: return 0
    ell=deepcopy(ell)
    if ordering==0:
        ordering=decideOrdering(ell)
    ##determining the prolongations
    nextGeneration=[]
    trans1=[-1]*n
    trans2=[-1]*n
    for i in xrange(n):##opti :cut that out
        trans1[gen1.perm[i]]=i
        trans2[gen2.perm[i]]=i
    while(ordering):
        ext=ordering.pop()
        if ext[0]==0:
            cx,cy=ext[1], (ell[0][ext[1]][0]+ell[0][ext[1]][1])*0.5
            if isBetween(cPj(ext[1],gen1.perm[ext[1]],gen1.xShift[ext[1]],gen1.yShift[ext[1]],cx,cy), ((ext[2]-1)/2)*3.1415926535897931,
                         cPj(ext[1],gen2.perm[ext[1]],gen2.xShift[ext[1]],gen2.yShift[ext[1]],cx,cy))==(ellDir[0][ext[1]]==-1):
                ##finding the splitting
                nextGeneration+=findNextBigonList(rect,gen1,gen2,ell,ellDir,ext,trans1,trans2)
                ell[0][ext[1]][(ext[2]+1)/2]=[-1,100][(ext[2]+1)/2]
                break
            ell[0][ext[1]][(ext[2]+1)/2]=[-1,100][(ext[2]+1)/2]
        if ext[0]==1:##horizontal ellipse
            cx,cy=(ell[1][ext[1]][0]+ell[1][ext[1]][1])*0.5, ext[1]
            tmp1=trans1[ext[1]]
            tmp2=trans2[ext[1]]
            if isBetween(cPj(tmp2,gen2.perm[tmp2],gen2.xShift[tmp2],gen2.yShift[tmp2],cx,cy), -ext[2]*1.5707963267948966,
                         cPj(tmp1,gen1.perm[tmp1],gen1.xShift[tmp1],gen1.yShift[tmp1],cx,cy))==(ellDir[1][ext[1]]==-1):
                ##finding the splitting
                nextGeneration+=findNextBigonList(rect,gen1,gen2,ell,ellDir,ext,trans1,trans2)
                ell[1][ext[1]][(ext[2]+1)/2]=[-1,100][(ext[2]+1)/2]
                break
            ell[1][ext[1]][(ext[2]+1)/2]=[-1,100][(ext[2]+1)/2]
    
    ##operating the splitting
    pairs=[]
    for poss in nextGeneration:
        pairs+=splitting(rect,gen1,gen2,ell,ellDir,trans1,trans2,poss)
    ##controlling the splitted (and returning when needed)
##    print pairs
##    print "past split"
    for p in pairs:
##        print maslovIndex(p[0][0],rect)-maslovIndex(p[0][1],rect)
##        print maslovIndex(p[1][0],rect)-maslovIndex(p[1][1],rect)
##        print "pair!!"
##        p[0][0].show()
##        p[0][1].show()
##        p[1][0].show()
##        p[1][1].show()
        if maslovIndex(p[0][0],rect)-maslovIndex(p[0][1],rect)!=-1 or maslovIndex(p[1][0],rect)-maslovIndex(p[1][1],rect)!=-1:continue##to optimise
##        print "esssi"
        if deepBdMap(rect,p[0][0],p[0][1],ell,p[0][2],ordering) and deepBdMap(rect,p[1][0],p[1][1],ell,p[1][2],ordering):
##            print "ret1"
            return 1
    
    if 1:
        totalG+=1
        if totalG==8:
            print "##########################################################################################################"
            print rect,ell
            gen1.show()
            gen2.show()
            print "ellDir",ellDir
            print "ordering=",ordering
            print "ext:",ext
            print "nextGen",nextGeneration
            print "splitting:",splitting(rect,gen1,gen2,ell,ellDir,trans1,trans2,nextGeneration[0])
            print "obtuse"
            obtuse,flats=findObtuse(rect,gen1,gen2,ell,ellDir)
            print obtuse,flats
##            print "inter"
            inter=findAllInter(ell)
##            print inter
            print "cuts"
            print findCuts(obtuse,flats,rect,gen1,gen2,ell,ellDir,trans1,trans2,inter)
            
            frm=show_all.floerDiagram((rect,ell,[gen1,gen2]))
            frm.Show()
    return 0
##debug
app.MainLoop()
print totalG
##end debug
if __name__ == "__main__":
##    from genGen import gen
##    import wx
##    import  show_all
##    app=wx.PySimpleApp()
##    g1=gen([-1, 2, 0, 3, 4, 5], [0, 1, 1, 1, -1, -1], [0, 1, 1, -1, -1, -1],0)
##    g2=gen([-1, 2, 3, 0, 4, 5], [0, 1, 1, 1, -1, -1], [0, 1, -1, 1, -1, -1],0)
##    data=([[1,3],[2,5],[0,4],[0,3],[2,4],[1,5]],([[1,3],[2,5],[0,4],[0,3],[2,4],[1,5]],[[0,5]]*6),[g1,g2])
##    frm=show_all.floerDiagram(data)
##    frm.Show()
##    app.MainLoop()
    gen1=gen([1, 0, 4, 2, 5, 6, 8, 7, -1], [-1, -1, 1, 1, -1, 1, -1, -1, 0], [1, 1, -1, 1, -1, 1, -1, -1, 0],0)
    gen2=gen([6, 1, 4, 0, 2, 7, 8, 5, -1], [-1, -1, 1, 1, -1, 1, -1, -1, 0], [-1, 1, -1, 1, 1, -1, -1, 1, 0],0)
    rect=[[1, 6], [0, 2], [1, 4], [0, 3], [2, 5], [4, 7], [6, 8], [5, 7], [3, 8]]
    ell=([[1, 6], [0, 2], [1, 4], [0, 3], [2, 5], [4, 7], [6, 8], [5, 7], -1], [[1, 3], [0, 2], [1, 4], -1, [2, 5], [4, 7], [0, 6], [5, 7], [6, 8]])
    ellDir=([1, -1, -1, -1, -1, -1, 1, -1, 0], [1, -1, -1, 0, 1, -1, -1, -1, 1])
    print deepBdMap(rect,gen1,gen2,ell,ellDir)
