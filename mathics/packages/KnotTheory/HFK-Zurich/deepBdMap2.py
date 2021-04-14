from genGen import gen
##import wx
##import  show_all
def areGenEqual(gen1,gen2):
    if gen1.perm==gen2.perm and gen1.xShift==gen2.xShift and gen1.yShift==gen2.yShift:
        return 1
    else: return 0
def copyGen(g):
    return gen(g.perm[:],g.xShift[:],g.yShift[:],0)
def findAge(gen,rect,tr):##next in line!
    n=len(gen.perm)
    age=0
    for i in xrange(n):##opti:loop in reverse directions with returns
        if gen.perm[i]!=-1:
            tmp=i
            if tmp<tr[gen.perm[i]][0]:
                age=max(age,2*(n+gen.perm[i])+2)
            if tmp>tr[gen.perm[i]][1]:
                age=max(age,2*(n+gen.perm[i])+1)
    if age>0:
        return age
    for i in xrange(n):
        if gen.perm[i]!=-1:        
            tmp=gen.perm[i]+(gen.yShift[i]+1)/2
            if tmp<=rect[i][0]:
                age=max(age,2*i+2)
            if tmp>rect[i][1]:
                age=max(age,2*i+1)
    return age
def findIsPoss(perm1,perm2):
    res=0
    for j,h in enumerate(perm1):
        if perm2[j]!=h: res-=1
        for i in xrange(j):
            if i!=-1:
                if perm1[i]<h: res+=2
                if perm2[i]<perm2[j]: res-=2 
    return res>=0

def listPossRectMp(rect,ell):##ell is only there to avoid where there is no oval!
    n=len(rect)
    coord=[]
    for x in range(n):
        if ell[0][x]!=-1:
            for y in range(n):
                if ell[1][y]!=-1:
                    for ss in [(1,1),(1,-1),(-1,1),(-1,-1)]:
                        coord.append((x,y,ss[0],ss[1]))
    possRect={}
    for ll in coord:
        for ur in coord:
            if ll[0]<ur[0] and ll[1]<ur[1]:
                b=0
                for x in range(ll[0]+(ll[2]+1)/2,ur[0]+(ur[2]+1)/2):
                    if ur[1]+(ur[3]+1)/2>rect[x][1]>=ll[1]+(ll[3]+1)/2 or ur[1]+(ur[3]+1)/2>rect[x][0]>=ll[1]+(ll[3]+1)/2:
                        b=1
                        break
                if b==0: possRect[(ll,ur)]=((ll[0],ur[1],ll[2],ur[3]),(ur[0],ll[1],ur[2],ll[3]))
    return possRect
def listRect(gen,possRect,immobile):
    n=len(gen.perm)
    res=[]
    pts=[(i,gen.perm[i],gen.xShift[i],gen.yShift[i]) for i in xrange(n)]
    for j in pts:
        if gen.perm[j[0]]==-1:continue
        if immobile[j[0]]: continue
        for i in xrange(j[0]):
            if gen.perm[i]==-1:continue
            if immobile[i]: continue
            if possRect.has_key((pts[i],j)):
                b=0
                for k in xrange(i+1,j[0]):
                    if gen.perm[i]<gen.perm[k]<gen.perm[j[0]]:
                        b=1
                        break
                if b: continue
                ng=copyGen(gen)
                ng.perm[i],ng.perm[j[0]]=ng.perm[j[0]],ng.perm[i]
                ng.yShift[i],ng.yShift[j[0]]=ng.yShift[j[0]],ng.yShift[i]
                res.append((ng,-1))
    return res
def listBigon(gen,rect,tr,immobile):
    n=len(gen.perm)
    res=[]
    for i in xrange(n):
        if gen.perm[i]!=-1 and not immobile[i]:
            tmp=i+(gen.xShift[i]+1)/2
            if tmp<=tr[gen.perm[i]][0] and gen.yShift[i]==1 or tmp>tr[gen.perm[i]][1] and gen.yShift[i]==-1:
                ng=copyGen(gen)
                ng.yShift[i]*=-1
                if gen.perm[i]==rect[i][0] or gen.perm[i]==rect[i][1]:
                    age=-1
                else: age=2*(n+gen.perm[i])+2-(ng.yShift[i]+1)/2
                res.append((ng,age))
            tmp=gen.perm[i]+(gen.yShift[i]+1)/2
            if tmp<=rect[i][0] and gen.xShift[i]==1 or tmp>rect[i][1] and gen.xShift[i]==-1:
                ng=copyGen(gen)
                ng.xShift[i]*=-1
                age=2*i+2-(ng.xShift[i]+1)/2
                res.append((ng,age))
    return res
def listReverseBigon(gen,rect,tr,immobile):
    n=len(gen.perm)
    res=[]
    for i in xrange(n):
        if gen.perm[i]!=-1 and not immobile[i]:
            tmp=i##carefull!!!!!!!!! changed from above!
            if tmp<tr[gen.perm[i]][0] and gen.yShift[i]==-1 or tmp>tr[gen.perm[i]][1] and gen.yShift[i]==1:##carefull!!!!!!!!! changed
                ng=copyGen(gen)
                ng.yShift[i]*=-1
                age=2*(n+gen.perm[i])+1+(ng.yShift[i]+1)/2##carefull!!!!!!!!! changed
                res.append((ng,age))
            tmp=gen.perm[i]+(gen.yShift[i]+1)/2
            if tmp<=rect[i][0] and gen.xShift[i]==-1 or tmp>rect[i][1] and gen.xShift[i]==1:
                ng=copyGen(gen)
                ng.xShift[i]*=-1
                age=2*i+1+(ng.xShift[i]+1)/2##carefull!!!!!!!!! changed
                res.append((ng,age))
    return res
debug=[0]*128
def deepBdMapRec(genStart,genGoal,depth,init,immobile,upDown=0,inherited=-1,hmap=-1):##init should contains what doesn't change without the diag changing
    if not findIsPoss(genStart.perm,genGoal.perm): return 0
    possRect,rect,tr=init
    if hmap==-1: hmap=dict()
    acc=[]
    age=findAge(genStart,rect,tr)
    if upDown==1 and age==0:##first condition: the steps on the path disappear
        return areGenEqual(genStart,genGoal)
##    print "("+repr(rect)+","+"[gen("+genStart.toString()+",0),"+"gen("+genGoal.toString()+",0)])"
##    print depth, "I"*(19-depth)+genStart.toString(),age,inherited
    if age<=inherited or age==0:
        age=-1
    if upDown==1:##the second value!
        acc+=[b for b in listReverseBigon(genStart,rect,tr,immobile) if b[1]>=age]
    else:
        if age<1:
            acc+=listRect(genStart,possRect,immobile)
        acc+=[b for b in listBigon(genStart,rect,tr,immobile) if b[1]>=age and (b[1]==-1 or b[1]!=inherited)]
##    for i in acc: print i[0].toString(),i[1]
    parity=0
    for g in acc:
        ttt=(tuple(g[0].perm),tuple(g[0].xShift),tuple(g[0].yShift))
        if hmap.has_key(ttt):
            if hmap[ttt]>=g[1]:
                continue
        if upDown==0 and areGenEqual(g[0],genGoal):
            parity+=1
            continue
        if depth>1:
            tmp=deepBdMapRec(g[0],genGoal,depth-1,init,immobile,1-upDown,g[1],hmap)
            if tmp==0: hmap[ttt]=g[1]
            parity+=tmp
    global debug
    debug[parity]+=1
    return parity
def initWith(rect,ell):
    tr=[[] for i in xrange(len(rect))]
    for i,p in enumerate(rect):
        tr[p[0]].append(i)
        tr[p[1]].append(i)
    return (listPossRectMp(rect,ell),rect,tr)  ##0 in init is possRect#1 in init is transposed rect
if __name__ == "__main__":
    data=([[2, 6], [1, 5], [4, 7], [2, 9], [0, 3], [1, 6], [0, 4], [3, 8], [7, 9], [5, 8]],
          ([[2, 6], [1, 5], [4, 7], -1, [0, 3], [1, 6], [0, 4], [3, 8], [7, 9], [5, 8]],
           [[4, 6], [1, 5], [0, 3], [4, 7], [2, 6], [1, 9], [0, 5], [2, 8], [7, 9], -1]),
          [gen([6, 2, 4, -1, 0, 1, 3, 8, 7, 5],[-1, -1, -1, 0, 1, 1, -1, 1, -1, -1],[-1, -1, 1, 0, 1, 1, -1, -1, 1, 1],0),
           gen([6, 2, 4, -1, 1, 3, 0, 8, 7, 5],[-1, -1, -1, 0, 1, 1, -1, 1, -1, -1],[-1, -1, 1, 0, -1, -1, 1, -1, 1, 1],0)])
    init=initWith(data[0],data[1])
##    print findAge(gen1,rect,init[2])
    gen1=data[2][0]
    gen2=data[2][1]
    immo=[1,1,1,1,0,0,0,1,1,1]
##    for j in listRect(gen1,init[0]):j[0].show()
##    deepBdMapRec(gen1,gen1,19,init)
##    import profile
##    profile.run("tmp=deepBdMapRec(gen1,gen2,99,init,immo)")
    tmp=deepBdMapRec(gen1,gen2,99,init,immo)
    print tmp
    
