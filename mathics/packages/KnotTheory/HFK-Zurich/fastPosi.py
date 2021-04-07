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
            if gen1.perm[i]!=-1:
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
            if gen1.perm[i]!=-1:
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
##from genGen import gen
##ellDir=([-1, -1, 1, -1, 1, -1, -1, 0], [1, 1, -1, 1, 0, 1, -1, 1])
##rect=[[5, 7], [3, 6], [2, 5], [1, 4], [0, 3], [2, 6], [1, 7], [0, 4]]
##ell=([[5, 7], [3, 6], [2, 5], [1, 4], [0, 3], [2, 6], [1, 7], -1], [[4, 7], [3, 6], [2, 5], [1, 4], -1, [0, 2], [1, 5], [0, 6]])
##gen1=gen([5, 6, 3, 1, 0, 2, 7, -1],[-1, -1, -1, -1, -1, 1, 1, 0],[1, -1, -1, 1, 1, 1, -1, 0],0)
##gen2=gen([7, 5, 3, 2, 0, 6, 1, -1],[-1, -1, -1, 1, -1, 1, 1, 0],[-1, -1, -1, -1, 1, -1, 1, 0],0)
##print detectSimple(len(rect),gen1,gen2,ellDir) 
