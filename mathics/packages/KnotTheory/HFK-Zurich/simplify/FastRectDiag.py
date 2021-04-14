import OLink
import RectDia
import copy
from inputLink import inputLink
class FastRectDiag:
    def __init__(self,tab):
        if isinstance(tab, RectDia.RectDia):
            tab=[(p.x,p.y) for p in tab.points]
        self.predecessor=0
        self.complexity=len(tab)/2
        self.xSorted=[]
        self.ySorted=[]
        for i in range(self.complexity):
            self.xSorted+=[-1,-1]
            self.ySorted+=[-1,-1]
        for p in tab:
            if self.xSorted[p[0]*2+0]==-1:
                self.xSorted[p[0]*2+0]=p[1]
            else:
                if p[1]>self.xSorted[p[0]*2+0]:
                    self.xSorted[p[0]*2+1]=p[1]
                else:
                    (self.xSorted[p[0]*2+0],self.xSorted[p[0]*2+1])=(p[1],self.xSorted[p[0]*2+0])
            if self.ySorted[p[1]*2+0]==-1:
                    self.ySorted[p[1]*2+0]=p[0]
            else:
                if p[0]>self.ySorted[p[1]*2+0]:
                    self.ySorted[p[1]*2+1]=p[0]
                else:
                    (self.ySorted[p[1]*2+0],self.ySorted[p[1]*2+1])=(p[0],self.ySorted[p[1]*2+0])
    def copy(self):
        h=copy.copy(self)
        h.xSorted=h.xSorted[:]
        h.ySorted=h.ySorted[:]
        return h
    def order(self):
        for i in xrange(self.complexity):
            if self.xSorted[2*i]>self.xSorted[2*i+1]:
                (self.xSorted[2*i],self.xSorted[2*i+1])=(self.xSorted[2*i+1],self.xSorted[2*i])
            if self.ySorted[2*i]>self.ySorted[2*i+1]:
                (self.ySorted[2*i],self.ySorted[2*i+1])=(self.ySorted[2*i+1],self.ySorted[2*i])
    def xySorted(self,tab):
        a=[-1]*(2*self.complexity)
        for i in xrange(2*self.complexity):
            o=tab[i]
            if a[2*o]==-1:
                a[2*o]=i/2
            else: a[2*o+1]=i/2
        return a
    def getSize(self):
        return self.complexity
    def __areUnlinked(self,i,j,k,l):
        if (i-j)*(i-k)*(i-l)*(k-j)*(l-j)*(l-k)==0:return 0
        if i<k<j or j<k<i: boo=1
        if i<l<j or j<l<i: boo2=1
        return boo==boo2
    def __areUnlinked2(self,i,j,k,l):
        return (k-i)*(j-k)*(l-i)*(j-l)>0
    def castleX(self,nb):
        tmp11=self.xSorted[nb*2]        
        tmp12=self.xSorted[nb*2+1]
        tmp21=self.xSorted[(nb+1)%self.complexity*2]        
        tmp22=self.xSorted[(nb+1)%self.complexity*2+1]
        if self.ySorted[tmp11*2]==nb:
            self.ySorted[tmp11*2]=(nb+1)%self.complexity
        else:
            self.ySorted[tmp11*2+1]=(nb+1)%self.complexity
        if self.ySorted[tmp12*2]==nb:
            self.ySorted[tmp12*2]=(nb+1)%self.complexity
        else:
            self.ySorted[tmp12*2+1]=(nb+1)%self.complexity
        if self.ySorted[tmp21*2]==(nb+1)%self.complexity:
            self.ySorted[tmp21*2]=nb
        else:
            self.ySorted[tmp21*2+1]=nb
        if self.ySorted[tmp22*2]==(nb+1)%self.complexity:
            self.ySorted[tmp22*2]=nb
        else:
            self.ySorted[tmp22*2+1]=nb
        (self.xSorted[nb*2],self.xSorted[(nb+1)%self.complexity*2])=(self.xSorted[(nb+1)%self.complexity*2],self.xSorted[nb*2])
        (self.xSorted[nb*2+1],self.xSorted[(nb+1)%self.complexity*2+1])=(self.xSorted[(nb+1)%self.complexity*2+1],self.xSorted[nb*2+1])
        return self
    def castleY(self,nb):
        tmp11=self.ySorted[nb*2]        
        tmp12=self.ySorted[nb*2+1]
        tmp21=self.ySorted[(nb+1)%self.complexity*2]        
        tmp22=self.ySorted[(nb+1)%self.complexity*2+1]
        if self.xSorted[tmp11*2]==nb:
            self.xSorted[tmp11*2]=(nb+1)%self.complexity
        else:
            self.xSorted[tmp11*2+1]=(nb+1)%self.complexity
        if self.xSorted[tmp12*2]==nb:
            self.xSorted[tmp12*2]=(nb+1)%self.complexity
        else:
            self.xSorted[tmp12*2+1]=(nb+1)%self.complexity

        if self.xSorted[tmp21*2]==(nb+1)%self.complexity:
            self.xSorted[tmp21*2]=nb
        else:
            self.xSorted[tmp21*2+1]=nb
        if self.xSorted[tmp22*2]==(nb+1)%self.complexity:
            self.xSorted[tmp22*2]=nb
        else:
            self.xSorted[tmp22*2+1]=nb
        (self.ySorted[nb*2],self.ySorted[(nb+1)%self.complexity*2])=(self.ySorted[(nb+1)%self.complexity*2],self.ySorted[nb*2])
        (self.ySorted[nb*2+1],self.ySorted[(nb+1)%self.complexity*2+1])=(self.ySorted[(nb+1)%self.complexity*2+1],self.ySorted[nb*2+1])
        return self
    def castle(self,nb,direction):
        if direction:
            if self.__areUnlinked2(self.ySorted[(nb+1)%self.complexity*2],self.ySorted[(nb+1)%self.complexity*2+1],self.ySorted[nb*2],self.ySorted[nb*2+1]):
                return self.castleY(nb)
                
        else:
            if self.__areUnlinked2(self.xSorted[(nb+1)%self.complexity*2],self.xSorted[(nb+1)%self.complexity*2+1],self.xSorted[nb*2],self.xSorted[nb*2+1]):
                return self.castleX(nb)
        return 0
    def isCastle(self,nb,direction):
        if direction:
            return self.__areUnlinked2(self.ySorted[(nb+1)%self.complexity*2],self.ySorted[(nb+1)%self.complexity*2+1],self.ySorted[nb*2],self.ySorted[nb*2+1])
        else:
            return self.__areUnlinked2(self.xSorted[(nb+1)%self.complexity*2],self.xSorted[(nb+1)%self.complexity*2+1],self.xSorted[nb*2],self.xSorted[nb*2+1])
    def __has(self,x,y):
        return self.xSorted[x*2+0]==y or self.xSorted[x*2+1]==y
    def isdestabilisation(self,x,y):
        nn=self.__has(x,y)
        mn=self.__has((x-1)%self.complexity,y)
        nm=self.__has(x,(y-1)%self.complexity)
        mm=self.__has((x-1)%self.complexity,(y-1)%self.complexity)
        if mn and nm and nn and (not mm): return 0
        if mm and mn and nn and (not nm): return 1
        if mm and nm and nn and (not mn): return 2
        if mm and mn and nm and (not nn): return 3
        return -1
    def isdestabilisable(self):
        self.order()
        for x in range(self.complexity):
            if self.ySorted[x*2+1]-self.ySorted[x*2]==1:
                return (1,x)
            if self.xSorted[x*2+1]-self.xSorted[x*2]==1:
                return (0,x)   
        return 0
    def __areUnlinked3(self,a,b,d):
        if d==0:
            return self.__areUnlinked2(self.xSorted[a*2],self.xSorted[a*2+1],self.xSorted[b*2],self.xSorted[b*2+1])
        else:
            return self.__areUnlinked2(self.ySorted[a*2],self.ySorted[a*2+1],self.ySorted[b*2],self.ySorted[b*2+1])
    def chainCastle(self,a,b,d):
        tmp=self
        if a>b: b+=self.complexity
        interval=b-a-1
        for i in range(interval):
            (tmp,tmp.predecessor)=(tmp.copy(),tmp)
            if tmp.__areUnlinked3(a%self.complexity,(a+1)%self.complexity,d):
                tmp.castle(a%self.complexity,d)
                a=(a+1)%self.complexity
            else:
                tmp.castle((b-1)%self.complexity,d)
                b=(b-1)%self.complexity
        if a==self.complexity-1:
            (tmp,tmp.predecessor)=(tmp.copy(),tmp)
            tmp.cycle(d)
##        print "simplifiable"
##        print tmp.toRectDia().toStringNice()
        return tmp
    
    def isdestabilisableAdvanced(self):
        if self.isdestabilisable(): return 0
        self.order()
        fw=[0]*self.complexity
        bw=[0]*self.complexity
        for d in (0,1):
            for i in xrange(self.complexity):
                n=1
                while(self.__areUnlinked3(i,(i+n)%self.complexity,d)):
                    n+=1
                fw[i]=n-1
                n=1
                while(self.__areUnlinked3(i,(i-n)%self.complexity,d)):
                    n+=1
                bw[i]=n-1
            for i in xrange(self.complexity):
                if d==0:
                    x1=self.ySorted[2*i]
                    x2=self.ySorted[2*i+1]
                else:
                    x1=self.xSorted[2*i]
                    x2=self.xSorted[2*i+1]
                if fw[x1]+bw[x2]+1>=x2-x1:
##                    print (x1,x2)
##                    print i
##                    print self.toRectDia().toStringNice()
                    return self.chainCastle(x1,x2,d)
                if fw[x2]+bw[x1]+1>=x1+self.complexity-x2:
##                    print (x2,x1)
##                    print i
##                    print self.toRectDia().toStringNice()
                    return self.chainCastle(x2,x1,d)
        return 0
    def cycle(self,d):
        if d==0:
            self.xSorted=self.xSorted[len(self.xSorted)-2:]+self.xSorted[:len(self.xSorted)-2]
            self.ySorted=self.xySorted(self.xSorted)
        else:
            self.ySorted=self.ySorted[len(self.ySorted)-2:]+self.ySorted[:len(self.ySorted)-2]
            self.xSorted=self.xySorted(self.ySorted)
    def m_destabilisation(self,direction,row):
        self.complexity-=1
        xS=[]
        yS=[]
        if direction==0:
            for i in xrange(self.complexity+1):
                if i!=row:
                    a=self.xSorted[2*i]
                    b=self.xSorted[2*i+1]
                    if a>self.xSorted[2*row]: a-=1
                    if b>self.xSorted[2*row]: b-=1
                    xS+=[a,b]
            self.xSorted=xS
            self.ySorted=self.xySorted(xS)
        else:
            for i in xrange(self.complexity+1):
                if i!=row:
                    a=self.ySorted[2*i]
                    b=self.ySorted[2*i+1]
                    if a>self.ySorted[2*row]: a-=1
                    if b>self.ySorted[2*row]: b-=1
                    yS+=[a,b]
            self.ySorted=yS
            self.xSorted=self.xySorted(yS)
            
    def succCa(self):
        acc=[]
        for i in range(self.complexity):
            if self.isCastle(i,0): acc.append(self.copy().castle(i,0))
            if self.isCastle(i,1): acc.append(self.copy().castle(i,1))
        return acc
    def fastsuccCa(self,dico):
        acc=[]
        for i in xrange(self.complexity):
            if self.isCastle(i,0):
                if not dico.has_key(self.hashCastle(i,0)): acc.append(self.copy().castle(i,0))
            if self.isCastle(i,1):
                if not dico.has_key(self.hashCastle(i,1)): acc.append(self.copy().castle(i,1))
        return acc
    def hashCastle(self,i,d):
        self.castle(i,d)
        h1=self.hashInt()
        self.castle(i,d)
        return h1    
    def hashInt(self):
        n=self.complexity
        res=0
        for i in xrange(n):
            res*=n
            res+=self.xSorted[i*2+0]
            res*=n
            res+=self.xSorted[i*2+1]
        return res
    def toRectDia(self):
        return RectDia.RectDia([(i,self.xSorted[2*i]) for i in range(len(self.xSorted)/2)]+[(i,self.xSorted[2*i+1])for i in range(len(self.xSorted)/2)])
if __name__ == "__main__":
    dd=FastRectDiag([(0,0),(0,4),(1,2),(1,8),(2,7),(2,9),(3,6),(3,8),(4,1),(4,3),(5,2),(5,7),(6,0),(6,3),(7,1),(7,5),(8,4),(8,6),(9,5),(9,9)])
    dd.complexity=7
    dd.xSorted=[2, 6, 1, 5, 4, 6, 3, 5, 0, 3, 1, 4, 0, 2]
    dd.ySorted=[4, 6, 1, 5, 0, 6, 3, 4, 2, 5, 1, 3, 0, 2]
    print dd.toRectDia().toStringNice()
    des=dd.isdestabilisable()
    tmp=dd.copy()
    tmp.m_destabilisation(des[0],des[1])
##    dd.m_destabilisation(1,3)
    print tmp.toRectDia().toStringNice()
