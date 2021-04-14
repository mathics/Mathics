##opti eliminer de toP ce qui est dans to0
import combinatoric
import deepBdMap2
reload(deepBdMap2)
import fastPosi
##import bdMapGolay
import rectDiagMisc

def hdEllipsesGen(ellx,elly):
    resx=[]
    for i in xrange(len(ellx)):
        if ellx[i]==-1:
            resx.append(-1)
        else:
            resx.append((1+5*i,1+4*ellx[i][0],4+5*i,2+4*ellx[i][1]))
    resy=[]
    for i in xrange(len(elly)):
        if elly[i]==-1:
            resy.append(-1)
        else:
            if ellx[elly[i][0]]!=-1 and (ellx[elly[i][0]][0]==i or ellx[elly[i][0]][1]==i):dlx=0
            else: dlx=2
            if ellx[elly[i][1]]!=-1 and (ellx[elly[i][1]][0]==i or ellx[elly[i][1]][1]==i):ury=5
            else: ury=3
            dlx+=elly[i][0]*5
            ury+=elly[i][1]*5
            resy.append((dlx,0+i*4,ury,3+i*4))
    return (resx,resy)##the ell parameter of bdMap
def convert(x,y,xs,ys):
    return (x*5+((xs+1)/2)*3+1,y*4+((ys+1)/2)*3)
def hdGetPath(HV,dlx,dly,urx,ury,srcx,srcy,dstx,dsty,rotation):
    vert=[]
    if HV==0:##(horizontal)
        if srcy==dsty:
            if srcx==dstx:
                if rotation==-1:
                    vert.append((dly,ury,urx))#
                    vert.append((ury,dly,dlx))#
                    return vert
            if srcx<dstx and rotation==-1 and srcy==dly or srcx>dstx and rotation==-1 and srcy==ury:
                vert.append((ury,dly,dlx))
                vert.append((dly,ury,urx))
            if srcx>dstx and rotation==1 and srcy==dly or srcx<dstx and rotation==1 and srcy==ury:
                vert.append((dly,ury,dlx))
                vert.append((ury,dly,urx))
        else:
            if (srcy<dsty)==(rotation==1):
                vert.append((dsty,srcy,urx))
            else:
                vert.append((dsty,srcy,dlx))
    else:
        if srcx==dstx:
            if srcy==dsty:
                if rotation==1:
                    vert.append((ury,dly,dlx))
                    vert.append((dly,ury,urx))
                    return vert
            if (srcy<dsty)==((rotation==1)==(srcx==dlx)):##yes = 3 parts
                ##le "dossier"
                if srcx==urx: dossier=dlx
                else: dossier=urx
                if srcy>dsty: vert.append((ury,dly,dossier))
                else: vert.append((dly,ury,dossier))
                ##le trou
                if srcy>dsty:
                    vert.append((srcy,ury,srcx))
                    vert.append((dly,dsty,srcx))
                else:
                    vert.append((srcy,dly,srcx))
                    vert.append((ury,dsty,srcx))
            else:
                vert.append((srcy,dsty,dstx))
        else:
            if srcx<dstx and rotation==1:
                vert.append((srcy,dly,srcx))
                vert.append((dly,dsty,dstx))
            if srcx<dstx and rotation==-1:
                vert.append((srcy,ury,srcx))
                vert.append((ury,dsty,dstx))
            if srcx>dstx and rotation==1:
                vert.append((srcy,ury,srcx))
                vert.append((ury,dsty,dstx))
            if srcx>dstx and rotation==-1:
                vert.append((srcy,dly,srcx))
                vert.append((dly,dsty,dstx))
    return vert
def sortVert(vert,n):
    tab=[[] for i in xrange(n)]    
    for v in vert:
        tab[v[2]].append(v)
    return tab
def getTable(x,y,vert):
    tab=[[0]*y for i in xrange(x+1)]
    sortedVert=sortVert(vert,x+1)
    for i in xrange(x):
        for j in xrange(y):
            tab[i+1][j]=tab[i][j]
        for j in sortedVert[i]:
            if j[0]>j[1]:
                for k in xrange(j[1],j[0]):
                    tab[i+1][k]-=1
            else: 
                for k in xrange(j[0],j[1]):
                    tab[i+1][k]+=1
    return tab[1:]
def hdCond(rect,ell):##here ell are hdEll!
    to0=[]
    for i in xrange(len(rect)):
        for j in [0,1]:
            to0.append((i*5+2,rect[i][j]*4+1,[]))
    from findDom import connComp
    toPlus=connComp(ell[0],ell[1])
    ##new#########
    for j in xrange(len(toPlus)):
        toPlus[j]=(toPlus[j][0],toPlus[j][1],[])
    ##eliminate useless toPlus
    tmp=[]
    for c in toPlus:
        b=0
        for e in ell[0]:
            if e==-1:continue
            if e[0]<=c[0] and e[1]<=c[1] and e[2]>c[0] and e[3]>c[1]:
                b=1
                break
        for e in ell[1]:
            if e==-1:continue
            if e[0]<=c[0] and e[1]<=c[1] and e[2]>c[0] and e[3]>c[1]:
                b=1
                break
        if b :tmp.append(c)
    toPlus=tmp 
    chEllx=[]
    chElly=[]
    iiii=-1
    for i in xrange(len(ell[0])):
        if ell[0][i]!=-1:
            iiii+=1
            e=ell[0][i]
            tmp=[]
            for j,c in enumerate(to0+toPlus):##inversed!!
                if e[0]<=c[0] and e[1]<=c[1] and e[2]>c[0] and e[3]>c[1]:
                    tmp.append(j)
                    c[2].append(iiii)
            chEllx.append(tmp)
        else:chEllx.append(-1)
    for i in xrange(len(ell[0])):
        if ell[1][i]!=-1:
            iiii+=1
            e=ell[1][i]
            tmp=[]
            for j,c in enumerate(to0+toPlus):##inversed!!
                if e[0]<=c[0] and e[1]<=c[1] and e[2]>c[0] and e[3]>c[1]:
                    tmp.append(j)
                    c[2].append(iiii)
            chElly.append(tmp)
        else:chElly.append(-1)
    for t in xrange(len(to0)):
        to0[t]=(to0[t][0],to0[t][1],to0[t][2],len(to0[t][2]))
    for t in xrange(len(toPlus)):
        toPlus[t]=(toPlus[t][0],toPlus[t][1],toPlus[t][2],len(toPlus[t][2]))
    return (to0,toPlus,(chEllx,chElly))
    ##to0,toPlus contain hd coord., influential ell and nb of influ. ell. chell contains for each ell the position influenced in parallel to to0+toPlus called ll 
def hdCond2(rect,ell,to0,toPlus):
    ll=[]
    n=len(rect)
    for i in to0+toPlus:
        ll.append((i[0],i[1]))
    delta=dict()
    for i in range(n):
        for dd in [0,1]:#0 vertical ell
            if ell[dd][i]==-1: continue
            coord=[]
            for y in range(n):
                for dx,dy in [(-1,-1),(-1,1),(1,-1),(1,1)]:
                    (x1,y1)=convert((i,y)[dd],(y,i)[dd],dx,dy)
                    if dd:
                        if (y1==ell[1][i][1] or y1==ell[1][i][3]) and x1>ell[1][i][0] and x1<ell[1][i][2]:
                            coord.append((x1,y1,1,y,i,dx,dy))
                    else:
                        if (x1==ell[0][i][0] or x1==ell[0][i][2]) and y1>ell[0][i][1] and y1<ell[0][i][3]:
                            coord.append((x1,y1,0,i,y,dx,dy))
            for x in coord:
                for y in coord:
                    hdVert=hdGetPath(1-dd,ell[dd][i][0],ell[dd][i][1],ell[dd][i][2],ell[dd][i][3],x[0],x[1],y[0],y[1],1-dd*2)#debug
                    deltap=[]
                    cc=0
                    for p in ll:
                        ssum=0
                        for l in hdVert:
                            if p[0]>=l[2]:
                                if p[1]<l[1] and p[1]>=l[0]:#debug!!
                                    ssum+=1
                                if p[1]>=l[1] and p[1]<l[0]:#debug!!
                                    ssum-=1
                        if ssum!=0: deltap.append((cc,ssum))
                        cc+=1
                    if (x[3]*3+x[5],x[4]*3+x[6])==(y[3]*3+y[5],y[4]*3+y[6]): deltap=[]#################################
                    delta[(x[2],x[3]*3+x[5],x[4]*3+x[6] ,y[3]*3+y[5],y[4]*3+y[6])]=deltap
    print "hdCond2Ready"
    return delta
def bdMapFirstPart(rect,gen1,gen2,ell):
    diff=[]
    n=len(rect)
    immobile=[]
    diffY=[0]*n
    for i in xrange(n):
        if gen1[i]!=gen2[i]:
            diff.append(i)
            diffY[gen1[i]]=1
    ##permutation points in the domain? obvious negativity?
    tab=[0]*(2*n)##is 2* necessary??
    insidePerm=[]
    insideWarning=0
    for i in xrange(n):
        if gen1[i]==gen2[i] and tab[2*gen1[i]]==0: immobile.append(1)
        else: immobile.append(0)
        if gen1[i]==-1: continue
        if gen1[i]<gen2[i]:
            for k in xrange(2*gen1[i],2*gen2[i]):
                tab[k]+=2
            tab[2*gen1[i]]-=1
            tab[2*gen2[i]]+=1
        if gen1[i]>gen2[i]:
            for k in xrange(2*gen2[i],2*gen1[i]):
                tab[k]-=2
            tab[2*gen1[i]]-=1
            tab[2*gen2[i]]+=1
        if tab[2*gen1[i]]>2 or tab[2*gen2[i]]>2:
            insideWarning=1##perm point inside
        for k in xrange(n):###############    dangerous
            if tab[2*k+1]<0:
                return 0 ##obvious(outside ellipse) negativity
    lenDiff=len(diff)
    vert=[]
    for i in diff:
        vert.append((gen1[i],gen2[i],i))
    tmp=[[] for i in xrange(n)]
    for i in xrange(n):
        if gen1[i]!=-1 and gen2[i]!=gen1[i]:
            tmp[gen1[i]]=[i]+tmp[gen1[i]]
            tmp[gen2[i]].append(i)
    hori=[]
    for i in xrange(n):
        if tmp[i]!=[]:
            hori.append((tmp[i][0],tmp[i][1],i))
    for i in xrange(n):
        if gen1[i]==-1:continue
        if ell[1][gen1[i]]==-1 or ell[0][i]==-1: continue
        if gen1[i]==gen2[i]:# and insidePerm.count(i)==1:
            diff.append(i)
            hori.append((i,i,gen1[i]))
##    print gen1,gen2,immobile
    return (diff,lenDiff,vert,hori,insidePerm,insideWarning,immobile)##if no problem from pseudogen diff is a "gift"
   
def bdMapPsgenCache(rect,ell,pool):
    res=[[0]*len(pool) for i in xrange(len(pool))]
    for iii in xrange(len(pool)):
        for jjj in xrange(len(pool)):
            res[iii][jjj]=bdMapFirstPart(rect,pool[iii],pool[jjj],ell)
    return res
def preparePath(rect,ell):##we need a knot!!
    x=ell[0].index(-1)
    hole=x
    y=rect[x][1] if ell[1][rect[x][0]]==-1 else rect[x][0]
    side=0 if rect[x][0]==y else 1
    path=[]
    for i in xrange(len(rect)-1):
        path.append((x*2+side,y))##le premier est de type y!
        tmp=ell[1][y]
        x= tmp[1] if tmp[0]==x else tmp[0]
        side=0 if rect[x][0]==y else 1
        path.append((x*2+side,x))
        tmp=ell[0][x]
        y= tmp[1] if tmp[0]==y else tmp[0]
        side=0 if rect[x][0]==y else 1
    path.append((x*2+side,y))##the last comp of path is additionall
    return path
def fillLl(gen1,gen2,cache,to0,toPlus,chEll,delta,diff,hori):
    lTp=len(toPlus)
    lT0=len(to0)
    ll=[0]*(lT0+lTp)
    for i in diff:
        tmp=delta[(0,i*3+gen1.xShift[i],gen1.perm[i]*3+gen1.yShift[i],  i*3+gen2.xShift[i],gen2.perm[i]*3+gen2.yShift[i])]
        for j in tmp:
            ll[j[0]]+=j[1]
    for k in hori:
        h0=gen1.perm.index(k)
        h1=gen2.perm.index(k)
        tmp=delta[(1,h0*3+gen1.xShift[h0],gen1.perm[h0]*3+gen1.yShift[h0],  h1*3+gen2.xShift[h1],gen2.perm[h1]*3+gen2.yShift[h1])]
        for j in tmp:
            ll[j[0]]+=j[1]
    return ll
def fillFValue(genPool,cache,ell,to0,toPlus,chEll,delta):
    ref=-1
    for i in genPool:##find a reference Element
        for j in i:
            if j:
                ref=j[0]
                break
        if ref!=-1:break
    diff,hori=[],[]
    for i in xrange(len(ell[0])):
        if ell[0][i]!=-1:
            diff.append(i)
        if ell[1][i]!=-1:
            hori.append(i)
    for i in genPool:
        for j in i:
            for gen in j:
                gen.llRefGen=fillLl(ref,gen,cache,to0,toPlus,chEll,delta,diff,hori)
                gen.llGenRef=fillLl(gen,ref,cache,to0,toPlus,chEll,delta,diff,hori)
categ=[0]*20
def bdMap(rect,gen1,gen2,cache,ell,to0,toPlus,chEll,delta,path,init):
    global categ
    firstPart=cache[gen1.psNb][gen2.psNb]
    if firstPart==0:
        categ[0]+=1
        return 0
    n=len(rect)
    (diff,lenDiff,vert,hori,insidePerm,insideWarning,immobile)=firstPart
##    return deepBdMap2.deepBdMapRec(gen1,gen2,99,init,immobile)
##    print "("+repr(rect)+","+repr(ell)+","+"[gen("+gen1.toString()+",0),"+"gen("+gen2.toString()+",0)])"
    total=0
    ll=gen1.llGenRef[:]
    lenll=len(ll)
    for i in xrange(lenll):
        ll[i]+=gen2.llRefGen[i]
    height=0
    for i in xrange(0,n-1):
        height=-ll[path[i*2][0]]
        for j in chEll[1][path[i*2][1]]:##lr premier est de type y
            ll[j]+=height
        height=-ll[path[i*2+1][0]]
        for j in chEll[0][path[i*2+1][1]]:
            ll[j]+=height
##    for i in xrange(0,2*(n-1)):
##        height=-ll[path[i][0]]
##        for j in path[i][1]:##lr premier est de type y
##            ll[j]+=height
    categ[1]+=1
    if ll[path[2*(n-1)][0]]!=0:
        return 0
    categ[2]+=1
    for i in xrange((n-1)*2,lenll):
        if ll[i]<0:
            return 0
    categ[3]+=1
##    k0=ell[0].index(-1)##optimisable cachable
##    k1=ell[1].index(-1)+n-1
##    ellDir=(tmp[0:k0]+[0]+tmp[k0:n-1],tmp[n-1:k1]+[0]+tmp[k1:])
##    print ellDir
##    print "("+repr(rect)+","+repr(ell)+","+"[gen("+gen1.toString()+",0),"+"gen("+gen2.toString()+",0)])"
##    return 1
##    print "fore"
##    if not insideWarning and fastPosi.detectSimple(n,gen1,gen2,ellDir)==1: return 1
    categ[4]+=1
    tmp= deepBdMap2.deepBdMapRec(gen1,gen2,99,init,immobile)
    return tmp%2
