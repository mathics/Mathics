from genGen import genGen
import rectDiagMisc
def cycleX(rect,i):
    return rect[i:]+rect[:i]
def cycleY(rect,i):
    n=len(rect)
    return [[min((x[0]+i)%n,(x[1]+i)%n),max((x[0]+i)%n,(x[1]+i)%n)] for x in rect]
def genCycles(rect):
    n=len(rect)
    res=[]
    for i in range(n):
        for j in range(n):
            res.append(cycleY(cycleX(rect,i),j))
    return res
def __areUnlinked2(self,i,j,k,l):
    return (k-i)*(j-k)*(l-i)*(j-l)>0
def caIfpo(rect,i,d):
    pass    
def gen1Ca(rect):
    pass
##print genCycles([[3, 6], [1, 5], [4, 6], [0, 2], [1, 3], [2, 5], [0, 4]])
def genEll(rect):
    top=[]
    bottom=[]
    for i in xrange(len(rect)):
        if rect[i][1]==len(rect)-1: top.append(i)
        if rect[i][0]==0: bottom.append(i)
    score=1000000000
    for a,b in [(0,rect[0][0]),(0,rect[0][1]),(len(rect)-1,rect[len(rect)-1][0])
                ,(len(rect)-1,rect[len(rect)-1][1]), (top[0],len(rect)-1),
                (top[1],len(rect)-1),(bottom[0],0),(bottom[1],0)]:
        ellx=rect[:]
        elly=rectDiagMisc.transpose(rect)
        ellx[a]=-1
        elly[b]=-1
        tmp=evalEll(ellx,elly)
        if tmp<score:
            sellx=ellx
            selly=elly
            score=tmp
    return (score,sellx,selly)
def evalEll(ellx,elly):
    return genGen(ellx,elly,1)
def main(rect,retry):
    print "opti, Ell"
    ellCandidate=[]
    l=genCycles(rect)
    score=1000000000
    for r in l:
        tmp=genEll(r)
        tmp=(tmp[0],tmp[1],tmp[2],r)
        ellCandidate.append(tmp)
        if tmp[0]<score:
            score=tmp[0]
            sellx=tmp[1]
            selly=tmp[2]
            opti=r
            print rectDiagMisc.toStringNice(r)
            print score
##            print evalEll(sellx,selly)
##    print tmp[0],sellx,selly,opti
##    return (score,sellx,selly,opti)
    ellCandidate.sort()
    ellCandidate=ellCandidate[:retry]
    ellCandidate.reverse()
    return ellCandidate
def simple(r,retry):
    ellCandidate=[]
    score=1000000000
    tmp=genEll(r)
    tmp=(tmp[0],tmp[1],tmp[2],r)
    ellCandidate.append(tmp)
    if tmp[0]<score:
        score=tmp[0]
        sellx=tmp[1]
        selly=tmp[2]
        opti=r
        print rectDiagMisc.toStringNice(r)
        print score
##            print evalEll(sellx,selly)
##    print tmp[0],sellx,selly,opti
##    return (score,sellx,selly,opti)
    ellCandidate.sort()
    ellCandidate=ellCandidate[:retry]
    ellCandidate.reverse()
    return ellCandidate
