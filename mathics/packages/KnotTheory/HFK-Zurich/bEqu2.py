def setV(sys,var):
    i=-1
    while(len(sys)>i+1):
        i+=1
        if sys[i][0]==sys[i][2]:
            if sys[i][1]==sys[i][3]:
                if var[sys[i][0]]!=-sys[i][1]:
                    var[sys[i][0]]=sys[i][1]
                    del sys[i:i+1]
                    i=-1
                    continue
                else:
                    return 0
            else:
                del sys[i:i+1]
                i-=1
                continue
        if var[sys[i][0]]!=0:
            if var[sys[i][0]]==sys[i][1]:
                del sys[i:i+1]
                i-=1
                continue
            else:
                if var[sys[i][2]]==-sys[i][3]:
                    return 0
                var[sys[i][2]]=sys[i][3]
                re=1
                del sys[i:i+1]
                i=-1
                continue
        if var[sys[i][2]]!=0:
            if var[sys[i][2]]==sys[i][3]:
                del sys[i:i+1]
                i-=1
                continue
            else:
                var[sys[i][0]]=sys[i][1]
                re=1
                del sys[i:i+1]
                i=-1
                continue
    if not sys: return 1
    return (sys,var)
def check(sys,sol):
    for e in sys:
        if sol[e[0]]*e[1]+sol[e[2]]*e[3]==-2:
            return 0
    return 1
def bEqu(sys,var):
    tmp=setV(sys,var)
##    print var,sys
    if tmp==1: return var
    if tmp==0: return 0
    v1=var[:]
    v2=var[:]
    v2[sys[0][0]]=1
    v1[sys[0][0]]=-1
    return bEqu(sys[:],v1) or bEqu(sys[:],v2)
##print setV([[0,1,5,-1],[2,-1,0,-1],[2,1,5,1],[2,-1,2,-1]],[0,0,-1,0,0,0])
def bEqu0(sys,var):
    for i in xrange(pow(2,len(var)-var.count(1)-var.count(-1))):
        tmp=i
        nvar=var[:]
        for k in xrange(len(var)):
            if var[k]==0:
                nvar[k]=(tmp%2)*2-1
                tmp/=2
        f=0
        for e in sys:
            if nvar[e[0]]*e[1]+nvar[e[2]]*e[3]<-1:
                f=1
                break
        if f==0:
            print nvar
            return 1
    return 0
def test(a,b):
    from random import randint
    res=[]
    for i in range(b):
        res.append((randint(0,a-1),randint(0,1)*2-1,randint(0,a-1),randint(0,1)*2-1))
##    print 3res
    return res

##print bEqu([(11, 1, 11, 1), (0, 1, 0, 1), (10, 1, 10, 1), (0, 1, 0, 1), (13, 1, 13, 1), (1, 1, 9, 1),
##            (9, -1, 1, -1), (1, 1, 12, 1), (12, -1, 1, -1), (2, 1, 8, 1), (8, -1, 2, -1), (2, 1, 2, 1),
##            (3, 1, 7, 1), (7, -1, 3, -1), (3, 1, 9, 1), (9, -1, 3, -1), (4, 1, 8, 1), (8, -1, 4, -1),
##            (4, 1, 4, 1), (13, 1, 13, 1), (5, 1, 5, 1), (7, 1, 7, 1), (5, 1, 5, 1), (11, 1, 11, 1),
##            (6, 1, 6, 1), (10, 1, 10, 1), (6, 1, 6, 1), (12, 1, 12, 1), (11, 1, 11, 1), (10, 1, 10, 1),
##            (13, 1, 13, 1), (0, 1, 0, 1), (10, 1, 10, 1), (0, 1, 0, 1), (0, 1, 0, 1), (11, 1, 11, 1),
##            (0, 1, 0, 1), (0, 1, 0, 1), (13, 1, 13, 1), (11, 1, 11, 1), (1, 1, 9, 1), (1, 1, 1, 1),
##            (1, 1, 1, 1), (10, 1, 10, 1), (1, 1, 1, 1), (1, 1, 1, 1), (11, 1, 11, 1), (1, 1, 1, 1),
##            (1, 1, 12, 1), (10, 1, 10, 1), (11, 1, 11, 1), (2, 1, 8, 1), (2, 1, 9, 1), (2, 1, 2, 1),
##            (7, 1, 7, 1), (3, 1, 7, 1), (3, 1, 9, 1), (4, 1, 8, 1), (4, 1, 4, 1), (4, 1, 4, 1),
##            (10, 1, 10, 1), (4, 1, 4, 1), (4, 1, 4, 1), (11, 1, 11, 1), (4, 1, 4, 1), (4, 1, 12, 1),
##            (4, 1, 4, 1), (4, 1, 4, 1), (13, 1, 13, 1), (10, 1, 10, 1), (11, 1, 11, 1), (12, 1, 12, 1),
##            (5, 1, 5, 1), (7, 1, 7, 1), (5, 1, 5, 1), (5, 1, 5, 1), (10, 1, 10, 1), (5, 1, 5, 1), (5, 1, 5, 1),
##            (11, 1, 11, 1), (10, 1, 10, 1), (6, 1, 6, 1), (10, 1, 10, 1), (6, 1, 6, 1), (6, 1, 6, 1), (12, 1, 12, 1)],[0]*14)
##for j in range(1000):
##    tmp=test(8,20)
##    
##    if bEqu0(tmp[:],8*[0])!=bEqu(tmp[:],8*[0]):print tmp

