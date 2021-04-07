from genGen import gen
def z(trProd):
    c=0
    for j in xrange(len(trProd)):
        for i in xrange(j):
            if trPod[i]<=trProd[j]:
                c+=1
    return c%2
##def expandTr(a,b):##a<b is precond
##    return range(a,b)+range(b-2,a-1,-1)
##print expandTr(1,1)
##def toTransProd(p):
##    print p
##    if p:
##        tmp=len(p)-1
##        tmp2=p.index(tmp)
##        tmp3=p[:-1]
##        if tmp==tmp2:
##            return toTransProd(tmp3)
##        tmp3[tmp2]=p[tmp]
##        print tmp , tmp2 , tmp3
##        return toTransProd(tmp3)+expandTr(tmp2,tmp)
##    return []
def toTransProd2(p):
    p=p[:]
    k=len(p)-1
    prod=[]
    while(k>=0):
        if p[k]<k:
            i=p.index(k)
            p=p[:i]+p[i+1:k+1]+[i]+p[k+1:]
            prod+=range(i,k)
        k-=1
    prod.reverse()
    return prod
def multTr(prod,n):
    p=range(n)
    for t in prod:
        p[t],p[t+1]=p[t+1],p[t]
    return p
def mult(p1,p2):
    return [p2[p1[i]] for i in xrange(len(p1))]
def inverse(p):
    r=[0]*len(p)
    for i in xrange(len(p)):
        r[p[i]]=i
    return r
def cocycle(p1,p2):
    return (z(toTransProd2(p1))+z(toTransProd2(p2))+z(toTransProd2(mult(p1,p2))))%2
def rectSignPerm(p1,a,b):
    p2=p1[:]
    p2[a],p2[b]=p2[b],p2[a]#####################################33possible inversion needed############
    return cocycle(mult(inverse(p1),p2),p1)
def rectSign(g,a,b):
    return sign
def digonSign(g,a,HV):
    return sign
