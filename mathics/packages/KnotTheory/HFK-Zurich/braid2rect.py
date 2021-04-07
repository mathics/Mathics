def spaceRect(rect,y):
    for i in range(len(rect)):
        for b in [0,1]:
            if rect[i][b]>=y:
                rect[i][b]+=1
def spaceColumn(col,y):
    for i in range(len(col)):
        if col[i]>=y:
                col[i]+=1
def findMax(rect):
    mx=-1
    for i in range(len(rect)):
        for b in [0,1]:
            mx=max(rect[i][b],mx)
    return mx    
def braidToRect(br,n):
    start=range(n)
    end=range(n)
    rect=[]
    for gen in br:
        if gen[0]==0:
            height=end[gen[1]]
            height2=end[gen[1]+1]
            spaceColumn(start,height)
            spaceRect(rect,height)
            rect.append([height,height2+1])
            end[gen[1]+1]=height+1
            spaceColumn(end,height+2)
        else:
            spaceColumn(start,end[gen[1]+1]+1)
            spaceRect(rect,end[gen[1]+1]+1)
            rect.append([end[gen[1]],end[gen[1]+1]+1])
            tmp=end[gen[1]+1]
            spaceColumn(end,tmp)
            end[gen[1]]=tmp
##        print (start,end,rect)
    mx=findMax(rect)
    for i in range(len(start)):
        rect=[[start[len(start)-i-1],mx+i+1]]+rect+[[end[len(start)-i-1],mx+i+1]]
    return rect
##print braidToRect([[0,0],[1,0]],2)

def elim(tab):
    res=[]
    for e in tab:
        if e: res.append(e)
    return res
def rdBraid(s):
    tmp=s[1:-1]
    tmp=tmp.split(",")
    mx=-1
    res=[]
    for kk in tmp:
        k=int(kk)
        if k<0:
            res.append((0,-k-1))
            mx=max(mx,-k)
        else:
            res.append((1,k-1))
            mx=max(mx,k)
    return (res,mx+1)



###############application
import pickle

if __name__ == "__main__":
    pass
##
##    br=open("braidList.txt","r")
##    rawList=[elim(kn.split(" ")) for kn in br.read().split("\n")]
##    br.close()
##    atlas=dict()
##    import simplify.diagSimplify
##
##    for kn in rawList:
##        tmp=rdBraid(kn[2])
##        atlas[(int(kn[0]),int(kn[1]))]=simplify.diagSimplify.simplify(
##            braidToRect(tmp[0],tmp[1]),5000)
##        if len(atlas)%100==0: print len(atlas)
##    ##the result is the knot dico called atlas!
##    sav=open("knotAtlas.pic","wb")
##    pickle.dump(atlas,sav)
##    sav.close()
##    print "Atlas ready"

av=open("knotAtlas.pic","rb")
atlas=pickle.load(av)
av.close()
if __name__ == "__main__":
    s=""
    print atlas[(7,2)]
    for i in range(13):
        for j in range(1,len(atlas)+1):
            if atlas.has_key((i,j)):
                s+=str((i,j))+": "+str(atlas[(i,j)])+"\n"
            else:
                break
        print "passage"
    av=open("knotAtlasV1.txt","w")
    av.write(s)
    av.close()
    ##print atlas
