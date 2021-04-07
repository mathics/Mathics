import psyco
psyco.log()
psyco.full()
import bdMapGeneral
reload(bdMapGeneral)
from bdMapGeneral import *

def getKnotFloerHomology(rect):
    from genGen import gen
    import generators
    import homology
    import getOptiEllipses

    reload(homology)
    reload(generators)
    
    ellCandidate=getOptiEllipses.simple(rect,1)
    print ellCandidate
    tmp=ellCandidate.pop()
    rect=tmp[3]
    print rect
    ell=(tmp[1],tmp[2])
    print "ellipses:"
    print ell
    print "score:",tmp[0]
    print "new Diagram:"
    print rectDiagMisc.toStringNice(rect)
    (tmp,bound,pool,index)=generators.classifiedGen(rect,ell,1)##INDEX
##    index-=1
    print "index",index
    print "Scale:A and M: ",bound
    print "Generators, tabulated by Maslov and Alexander grading:"
    from homology import transpose
    transposed=transpose(tmp)
    def format(s,l):
        if len(s)<l:
            return " "*(l-len(s))+s
        return s
    for i in transposed:
        s=""
        for j in i:
            s+=format(str(len(j)),6)+" "
        print s
    ##the bdMap stuff
    HDEll=hdEllipsesGen(ell[0],ell[1])
    (to0,toPlus,chEll)=hdCond(rect,HDEll)
##    print "to0,toPlus,chell"
##    print to0
##    print toPlus
##    print chEll
    delta=hdCond2(rect,HDEll,to0,toPlus)
    path=preparePath(rect,ell)
    print "path",path
    ##the deepBd stuff
    init=deepBdMap2.initWith(rect,ell)
    
##    print "deep2!"
##        print "hdEllipses"
##        print HDEll

##    def eulerP(tab):
##        k=[0]*(len(tab[0]))
##        for i in range(len(tab[0])):
##            tot=0
##            for j in range(len(tab)):
##                tot=len(tab[j][i])-tot
##            k[i]=tot
##        return k
##    print eulerP(tmp)
    cache=bdMapPsgenCache(rect,ell,pool)
    fillFValue(tmp,cache,ell,to0,toPlus,chEll,delta)##new#######################################################################
##        cache2=bdMapGolay.bdMapPsgenCache(rect,ell,pool)
##    a=raw_input("w")
    tmp2=homology.chain2DToHomv3(tmp,lambda x,y:bdMap(rect,x,y,cache,ell,to0,toPlus,chEll,delta,path,init),len(rect)-1,index)
    
    return (bound,transpose(tmp2))
def AllToString(rect):
##    import profile
##    profile.run("tmp2=getKnotFloerHomology(rect)")
##    from trace import Trace
##    tracer=Trace(count=1, trace=0, countfuncs=1, countcallers=1)
##    tracer.run("tmp2=getKnotFloerHomology(rect)")
##    r = tracer.results()
##    r.write_results()
    tmp2=getKnotFloerHomology(rect)[1]
    print "HFK:"
    def format(s,l):
        if len(s)<l:
            return " "*(l-len(s))+s
        return s
    st=''
    for i in tmp2:
        s=""
        for j in i:
            s+=format(str(j),5)+" "
        st+=s+"\n"
    return st
def batchVersion(rect):
    tmp2=getKnotFloerHomology(rect)
    return [int(tmp2[0][0]),int(tmp2[0][1]),int(tmp2[0][2]),int(tmp2[0][3]), tmp2[1]]
#######################################  debug ####################################################################
if __name__ == "__main__":
##    print hdGetPath(0   ,2,3,102,103,  5,3,6,3,-1)
##    rect=[[1,4],[0,2],[1,3],[2,4],[0,3]]#treefoil
    rect=[[3,6],[4,8],[0,2],[1,7],[0,5],[3,8],[2,6],[1,5],[4,7]]#16 crossings
##    rect=[[1,3],[2,5],[0,4],[0,3],[2,4],[1,5]]
##    rect=[[2,4],[3,6],[0,6],[0,5],[1,4],[1,5],[2,3]]#big unknot
##    rect=[[1,5],[0,2],[1,4],[3,5],[2,4],[0,3]]
##    rect=[[0,1],[0,1]]
##    rect=[[5,7],[3,6],[2,5],[1,4],[0,3],[2,6],[1,7],[0,4]]#8_19
##    rect=[[0,4],[3,6],[2,5],[1,3],[4,7],[2,6],[0,5],[1,7]]#8_20
##
##    rect=[[0,3],[1,14],[0,2],[1,9],[3,11],[2,10],[4,7],[6,9],
##          [5,8],[7,12],[11,14],[10,13],[6,12],[4,8],[5,13]]
##    rect=[[0,3],[1,13],[0,2],[1,6],[3,10],[2,9],
##          [5,8],[7,11],[10,13],[9,12],[6,11],[4,8],[5,12],[4,7]]

##Sebs
##    rect=[[0,2],[1,3],[2,5],[1,4],[3,5],[0,4]]
##    rect=[[1,6],[0,2],[1,4],[0,3],[2,5],[4,7],[6,8],[5,7],[3,8]]
##    rect=[[0,1],[0,1]]
##    rect=[[0,13],[9,11],[5,7],[6,10],[9,12],[11,13],[5,8],[7,10],[1,3],[2,6],[1,4],[3,8],[4,12],[0,2]]
    import braid2rect
##    rect=braid2rect.atlas[(12,1291)]
##    rect=braid2rect.atlas[(11,370)]##90s about
    rect=braid2rect.atlas[(11,418)]##has a multi domain!
##    rect=braid2rect.atlas[(9,15)]

##    rect=rect[6:]+rect[:6]
    import rectDiagMisc
    print rectDiagMisc.toStringNice(rect)
    print rect
    print 11
    import time
    startTime=time.clock()
##    print "second phase"
    print AllToString(rect)
    print "categ:",categ
    print "dpM2:",deepBdMap2.debug
    print "Duration:",time.clock()-startTime
##    raw_input("")
##    print deepBdMap.totalG
