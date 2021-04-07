################################## rectangularDiagram related
def transpose(rD):## xshorted pairs of y coord. to vice-versa
    res=[[-1,-1] for i in range(len(rD))]
    for i in range(len(rD)):
        for j in [0,1]:
            tmp=(rD[i][j],i)
            if res[tmp[0]][0]==-1: res[tmp[0]][0]=tmp[1]
            else:
                res[tmp[0]][1]=tmp[1]
    return res
def recToPermAndComp(rect):
    tr=transpose(rect)
##    print tr
    n=len(rect)
    dec=[[0,0] for i in range(n)]
    c=0
    k=0
    while(1):
        coord=-1
        for i in range(2*n): 
            if dec[i/2][i%2]==0:
                coord=i
                break
        if coord==-1: break
        xy=coord
        k+=1
        par=k
        while(1):
            dec[xy/2][xy%2]=par
##            print dec
            c+=1
            if par>0:
                xy+=1-2*(xy%2)
            else:
                if tr[rect[xy/2][xy%2]][0]==xy/2: tmp=tr[rect[xy/2][xy%2]][1]
                else: tmp=tr[rect[xy/2][xy%2]][0]
                if rect[tmp][0]==rect[xy/2][xy%2]: xy=2*tmp
                else: xy=2*tmp+1
            par=-par
            if xy==coord:break
    p1=[0]*n
    p2=[0]*n
    for i in range(n):
        if dec[i][0]>0:
            (p1[i],p2[i])=(rect[i][0],rect[i][1])
        else:
            (p1[i],p2[i])=(rect[i][1],rect[i][0])
    return ((p1,p2),dec)
##print recToPermAndComp([[0,3],[2,4],[1,3],[0,2],[1,4],[5,6],[5,6]])
def getWindingNbTable(p1,p2):###########unsure!! check for shifts
    n=len(p1)
    tab=[[0]*(n+1) for i in range(n+1)]
    for i in range(n):
        for j in range(1,n+1):
                tab[i+1][j]=tab[i][j]
        if p1[i]<p2[i]:
            for j in range(p1[i]+1,p2[i]+1):
                tab[i+1][j]=tab[i+1][j]+1
        else:
            for j in range(p2[i]+1,p1[i]+1):
                tab[i+1][j]=tab[i+1][j]-1
    return tab

def toStringNice(rect):
        s=''
        
        n=len(rect)
        tab=[[" "]*n for i in range(n)]
        for i in range(n):
            for j in [0,1]:
                tab[i][rect[i][j]]="o"
        for i in range(n):
            for j in range(rect[i][0]+1,rect[i][1]):
                tab[i][j]="|"
        rr=transpose(rect)
        for i in range(n):
            for j in range(rr[i][0]+1,rr[i][1]):
                if tab[j][i]=="|": tab[j][i]="+"
                else:tab[j][i]="-"
##        print tab
        for i in range(n-1,-1,-1):
            for j in range(n):
                s+=tab[j][i]
            s+="""
"""
        return s
##print toStringNice([[0, 4], [3, 6], [2, 5], [1, 3], [4, 7], [2, 6], [0, 5], [1, 7]])


