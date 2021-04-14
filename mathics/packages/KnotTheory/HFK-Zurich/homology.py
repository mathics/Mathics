import random
import copy
import combinatoric
import genGen

def transpose(mat):
    return [[mat[j][i] for j in range(len(mat))] for i in range(len(mat[0]))]
##print transpose([[1,0],[1,0]])
def makeSquare(mat):
    x=len(mat)
    if x==0: return mat
    y=len(mat[0])
    if x>y:
        for i in range(x):
            mat[i]+=[0]*(x-y)
    else:
        for i in range(y-x):
            mat.append([0]*(y))
    return mat
def exch(a,b,mat):#exch second coord
    for i in xrange(len(mat)):
        (mat[i][a],mat[i][b])=(mat[i][b],mat[i][a])
def invPerm(p):#permutations with 0!
    k=[0]*len(p)
    for i in range(len(p)):
        k[p[i]]=i
    return k
def ensure(i,mat,perm):
    for x in range(i,len(mat)):
        for y in range(i,len(mat[0])):
            if mat[x][y]==1:
                (perm[i],perm[x])=(perm[x],perm[i])
                (mat[x],mat[i])=(mat[i],mat[x])
                exch(y,i,mat)
                return 1
    return -1
##print ensure(0,[[0,0,1],[0,1,1],[1,1,0]],[0,1,2])
def z2GElimHorizontal(mat):
    n=len(mat)
    nn=len(mat[0])
    perm=range(n)
    for i in range(n):
        if ensure(i,mat,perm)==-1:
            return (perm,mat)
        for j in range(i+1,nn):
            if mat[i][j]==1:
                for ii in xrange(i,n):
                    mat[ii][j]=(mat[ii][j]+mat[ii][i])%2
    return (perm,mat)
##z2GElimHorizontal([[0, 0, 0, 0, 0, 0, 1, 0, 0, 0],
##                         [0, 1, 0, 0, 0, 0, 0, 0, 0, 0],
##                         [0, 0, 1, 1, 0, 0, 0, 0, 0, 0],
##                         [0, 0, 0, 0, 0, 0, 1, 0, 1, 0],
##                         [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
##                         [0, 0, 0, 0, 0, 1, 0, 0, 0, 0],
##                         [0, 0, 1, 0, 0, 1, 0, 0, 0, 0],
##                         [0, 0, 0, 0, 0, 1, 0, 0, 0, 0],
##                         [0, 1, 0, 0, 0, 0, 0, 0, 0, 0],
##                         [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]])

def trace(mat):
    n=0
    for i in range(min(len(mat),len(mat[0]))):
        n+=mat[i][i]
    return n
def completeSolution(mat,sol):
    for i in range(len(mat[0])-1,-1,-1):
        sp=0
        for j in range(len(mat)):
            sp+=mat[j][i]*sol[j]
        sol[i]+=sp
        sol[i]%=2
    return sol
def findKerBase(mat):##rectangleMat???
    n=len(mat)
    (perm,mat)=z2GElimHorizontal(copy.deepcopy(mat))
##    print mat
    ll=[]
    ll2=[]
    for i in range(trace(mat),len(mat)):
        sol=[0]*n
        sol[i]=1
##        print sol
        sol=completeSolution(mat,sol)   
        ll.append(sol)
##        print sol
    for l in ll:
        lp=[0]*len(mat)
        for j in range(len(mat)):
            lp[perm[j]]=l[j]
        ll2.append(lp)
    return ll2
def imageDim(mat):
    mat=z2GElimHorizontal(mat)[1]
    return trace(mat)
def calcMat(l1,l2,bndFunc):
    mat=[]
    for x in range(len(l1)):
##        if x%100==0:print x
        row=[]
        for y in xrange(len(l2)):
            row.append(bndFunc(l1[x],l2[y]))
        mat.append(row)
    return mat
def calcSqMat(l1,l2,bndFunc):
##    print (l1,l2)
    mat=[]
    for x in range(len(l1)):
        row=[]
        for y in xrange(len(l2)):
            row.append(bndFunc(l1[x],l2[y]))
        mat.append(row)
##    print makeSquare(mat)
    return makeSquare(mat)
def calcZ2Hom(l1,l2,l3,bndFunc):####only square mat?
    if len(l2)==0:
        return 0
    if len(l1)==0:
        if len(l3)==0:
            return len(l2)
        return len(l2)-imageDim(calcSqMat(l2,l3,bndFunc))###fkb
    if len(l3)==0:
        return len(l2)-imageDim(calcSqMat(l1,l2,bndFunc))
    print (len(l1),len(l2),len(l3))
    tmp=isZero(combinatoric.matrixProduct(calcMat(l2,l3,bndFunc),calcMat(l1,l2,bndFunc)))
    if tmp:
        (a,b)=tmp
        print (l1[a].perm,l1[a].xShift,l1[a].yShift,l3[b].perm,l3[b].xShift,l3[b].yShift)
    return len(l2)-imageDim(calcSqMat(l2,l3,bndFunc))-imageDim(calcSqMat(l1,l2,bndFunc))

def isZero(mat):
    res=0
    for i in xrange(len(mat)):
        for j in xrange(len(mat[0])):
            if mat[i][j]%2!=0:
                print "nonZ"
                print (i,j)
                res+=1
    return 0
def decideIndex(chain,n):
    col=[0]*len(chain[0])
    mx=-1
    for j in range(len(chain[0])):
        for i in range(len(chain)-1):
            col[j]+=len(chain[i][j])*len(chain[i+1][j])
        if col[j]>mx:
            mx=col[j]
            index=j
    index=index-(n/2)
    if index<0:index=0
    return index
def chain2DToHomv3(chain,bndFunc,n,index="no"):
    print index
    if index=="no": index=decideIndex(chain,n)
    tot1=0
    tot0=0
    imageDimTab=[[0]*len(i) for i in chain]

    
##    ttmp2=calcMat(chain[12][14],chain[13][14],bndFunc)
##    ttmp=calcMat(chain[13][14],chain[14][14],bndFunc)
##    print "product"
##    ttt=combinatoric.matrixProduct(ttmp,ttmp2)
##    print isZero(ttt)
##    print bndFunc(chain[13][14][90],chain[14][14][115])
##    a=calcMat([chain[12][14][2]],chain[13][14],bndFunc)[0]
##    print a
##    b=[x[0] for x in calcMat(chain[13][14],[chain[14][14][115]],bndFunc)]
##    print b
##    return "r"
    for j in range(len(chain)):
        if j>=index and j<index+n: continue
        for i in range(len(chain[0])-1):
            if len(chain[j][i])!=0 and len(chain[j][i+1])!=0:
                tmpMat=calcMat(chain[j][i],chain[j][i+1],bndFunc)
                for a in tmpMat:
                    for b in a:
                        tot1+=b
                        if b==0:tot0+=1
##                makeSquare(tmpMat)
##                if i==12 and j==12:
####                    print bndFunc(chain[i][j][3],chain[i+1][j][7])
####                    print tmpMat
##                    print len(chain[i][j]),len(chain[i+1][j])
##
##                    for k in range(len(chain[i][j])):
##                        b=0
##                        print k
##                        chain[i][j][k].show()
##                        
##                    print "to"
##                    for k in range(len(chain[i+1][j])):
##                        b=0
##                        print k
##                        chain[i+1][j][k].show()
                imageDimTab[j][i]=imageDim(tmpMat)
            if len(chain[j][i])>0 or len(chain[j][i+1])>0: print "Image",i,j,":",len(chain[j][i]),len(chain[j][i+1]),imageDimTab[j][i]
##            f=open("C:\Program Files\python5\pyprog\Floer\s.txt","a")
##            f.write(str(("Image",i,j,":",imageDimTab[i][j])))
##            f.close()
    res=[[0]*len(chain[0]) for i in range(len(chain))]
    for i in range(1,len(chain[0])):
        for j in range(len(chain)):
            if j>=index and j<index+n: continue
            res[j][i]=len(chain[j][i])-imageDimTab[j][i-1]-imageDimTab[j][i]
    for j in range(len(chain)):
        if j>=index and j<index+n: continue
        res[j][0]=len(chain[j][0])-imageDimTab[j][0]
    if index!="no":
        deconv(res,index,n)
    print tot1,"/",tot0+tot1
    return res

def getBin(n):
    l=[0]*(n+1)
    l[0]=1
    for i in range(n):
        for j in range(i+1,0,-1):
            l[j]+=l[j-1]
    return l
def deconv(res,index,n):
    ex=getBin(n)
    print ex
    ex[0]=0
    (x,y)=(len(res[0]),len(res))
##    print index,n
##    print res
    for i in range(index):
        for k in range(len(res[0])):
            mul=res[i][k]
            if mul!=0:
                for j in range(n+1):
                    a=k+j
                    b=i+j
                    if a>=0 and b>=0 and a<x and b<y:
##                        print b>=index , b<index+n
                        if b>=index and b<index+n:
                            pass
                        else:
                            res[b][a]-=ex[j]*mul
##                            print b,a,j,k,i
    ex[0]=1
    for k in range(len(res[0])-1,-1,-1):
        for i in range(y-1,index+n-1,-1):
            mul=res[i][k]
            if mul!=0:
                for j in range(n+1):
                    a=k-j
                    b=i-j
                    if a>=0 and b>=0 and a<x and b<y:
                        if b>=index and b<index+n:
                            if ex[j]==1:res[b][a]+=mul
                        else:res[b][a]-=ex[j]*mul
    return res
