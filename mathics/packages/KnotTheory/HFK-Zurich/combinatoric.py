####################################################### some combinatoric first
def genPerm(n):
    if n==0: return [[]]
    res=[]
    prev=genPerm(n-1)
    for p in prev:
        for i in range(n-1):
            k=p[i]
            tmp=p+[k]
            tmp[i]=n
            res.append(tmp)
        res.append(p+[n])
    return res
def genPermElem(elem):
    if not elem: return [[]]
    tot=[]
    for i in range(len(elem)):
        tmp=genPermElem(elem[:i]+elem[i+1:])        
        tot+=[[elem[i]]+x for x in tmp]
    return tot
##print genPermElem(range(4))
def fact(n):
    res=1
    for i in xrange(n):
        res*=i+1
    return res
##print fact(3)
def tpSort(gr):##only for DAG
    n=len(gr)
    bb=[0]*n
    g=-1
    for rien in range(n):
        g=0
        for j in xrange(n):
            m=5+n## 5 is arbitrary!!
            for i in xrange(n):
                if gr[i][j]!=0:
                    if bb[i]<m:
                        m=bb[i]
            if m!=5+n:
                bb[j]=m+1
                g=1
        if g==-1: break
    return bb
##print topologicalSorting([[0,1,0],[0,0,1],[0,0,0]])
def matrixProduct(a,b):
##    print a
##    print b
    return [[sum([a[j][i]*column[j] for j in xrange(len(a))]) for i in range(len(a[0]))] for column in b]
##    res=[]
##    a1=len(a[0])
##    a2=len(a)
##    for column in b:
##        cc=[]
##        for i in xrange(a1):
##            ss=0
##            for j in xrange(a2):
##                if column[j]:
##                    ss+=a[j][i]
##            cc.append(ss)
##        res.append(cc)
##    return res
##    import numpy
##    a=numpy.matrix(numpy.array(a))
##    b=numpy.matrix(numpy.array(b))
##    return b*a
##print matrixProduct([[1,2]],[[0],[1]])
def classify(l,func):
    dic=dict()
    tot=[]
    for i in l:
        tmp=func(i)
        if dic.has_key(tmp):
            tot[dic[tmp]].append(i)
        else:
            dic[tmp]=len(tot)
            tot.append([i])
    return tot
##print classify([1,2,3,4,5,6,7],lambda x:x%3)
def putInTable2d(l,func):
    tmp=[func(i) for i in l]
    bounds=[]
    n=0
    for d in range(2):
##        if n%500==0:
##            print "InTable:"
##            print n
        n+=1
        mx=-1000
        mn=1000
        for r in tmp:
            mx=max(r[d],mx)
            mn=min(r[d],mn)
        bounds.append((mn,mx))
##    print bounds
    res=[[[] for j in xrange(bounds[1][1]-bounds[1][0]+1)] for i in range(bounds[0][1]-bounds[0][0]+1)]
    for i in xrange(len(l)):
        (res[tmp[i][0]-bounds[0][0]][tmp[i][1]-bounds[1][0]]).append(l[i])
    return (res,bounds)
##print putInTable2d(range(40),lambda x:(x%6,x%5))

def golayCode(n):
    if n==0: return []
    return golayCode(n-1)+[n]+golayCode(n-1)
##print golayCode(5)
def golaySign(l):
    rot=[1]*40
    for e in xrange(len(l)):
        tmp=l[e]-1
        l[e]*=rot[tmp]
        rot[tmp]*=-1
golayCache=[golayCode(i) for i in range(2)]+[-1]*30
for l in golayCache:
    if l!=-1: golaySign(l)
##print golayCache[4]
##def multiDim(dim):##incorrect not zero but []!!
##    if len(dim)==1: return [0]*dim[0]
##    tmp=dim[1:]
##    return [multiDim(tmp) for i in xrange(dim[0])]
##def putInTable(l,func):##incorrect not zero but []!!
##    if len(l)==0: return ([],[])
##    tmp=[func(i) for i in l]
##    dim=len(tmp[0])
##    bounds=[]
##    for d in range(dim):
##        mx=-1000
##        mn=1000
##        for r in tmp:
##            mx=max(r[d],mx)
##            mn=min(r[d],mn)
##        bounds.append((mn,mx))
##    res=multiDim([i[1]-i[0]+1 for i in bounds])
##    for i in xrange(len(l)):
##        nn=res
##        for d in xrange(dim-1):
##            nn=nn[tmp[i][d]]
##        nn[tmp[i][dim-1]]=l[i]
##    return (res,bounds)
