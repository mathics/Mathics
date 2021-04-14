import FastRectDiag
import fastUnknot2
def tangleToRect(tangle):##takes a pseudo braid return a coll a point forming the recDiag
##    print tangle
    points=[]
    section=[-1,1]
    forbidden=[-1,1]
    def findFree(s,e):
        x=(s+e)/2.0
        while(1):
            x=(s+x)/2.0
            if forbidden.count(x)==0:
                return x
    levelCounter=0
    for level in tangle:
        if level[0]==3:
            points.append((section[level[1]+1],levelCounter))
            points.append((section[level[1]+2],levelCounter))
            section[level[1]+1:level[1]+3]=[]
        if level[0]==2:
            tmp1=findFree(section[level[1]],section[level[1]+1])
            tmp2=findFree(tmp1,section[level[1]+1])
            points.append((tmp1,levelCounter))
            points.append((tmp2,levelCounter))
            forbidden+=[tmp1,tmp2]
            section[level[1]+1:level[1]+1]=[tmp1,tmp2]
        if level[0]==0:
            tmp1=findFree(section[level[1]+2],section[level[1]+3])
            points.append((tmp1,levelCounter))
            points.append((section[level[1]+1],levelCounter))
            forbidden+=[tmp1]
            section[level[1]+3:level[1]+3]=[tmp1]
            section[level[1]+1:level[1]+2]=[]
        if level[0]==1:
            tmp1=findFree(section[level[1]],section[level[1]+1])
            points.append((tmp1,levelCounter))
            points.append((section[level[1]+2],levelCounter))
            forbidden+=[tmp1]
            section[level[1]+2:level[1]+3]=[]
            section[level[1]+1:level[1]+1]=[tmp1]
        levelCounter+=1
    forbidden.sort()
    coll=[]
    for x in range(len(forbidden)):
        for p in points:
            if(p[0]==forbidden[x]):
                coll.append([x-1,p[1]])
    return coll
def fromCollToRectDiag(coll):##organize a coll. of points diag to a ordered diag
##    print coll
    d=[0]*(len(coll)/2)
    for i in coll:
        x=i[0]
        if d[x]==0:
            d[x]=[i[1]]
        else:
            if d[x][0]>i[1]:
                d[x]=[i[1],d[x][0]]
            else:
                d[x]=[d[x][0],i[1]]
    return d
def fromBraidToTangle(word,width):
    return [[2,i] for i in range(width)]+word+[[3,width-i-1] for i in range(width)]
def fromWordToRectDiag(word,width):
    return tangleToRect(fromBraidToTangle(word,width))
def forAnna(word,width):
    return (fastUnknot2.unknot(fromWordToRectDiag(word,width),20000)[1]).toRectDia().toStringNice()
##print forAnna([[1,0],[1,0],[1,0]],2)
def readLetterBraid(s):
    b=[]
    mm=0
    for l in s:
        n=ord(l)
        if n>96:
            b.append([0,n-97])
            if n-97>mm: mm=n-97
        else:
            b.append([1,n-65])
            if n-65>mm: mm=n-65
    return (b,mm+2)
def knot_source():
    tab=[]
    s=""
    subFile=open("braidRep.txt")
    sub=subFile.read()
    subFile.close()
    tab=sub.split("\n")
    tab=[[x.split(" ")[0],x.split(" ")[2]] for x in tab]
    for i in tab:
        print i[0]
        print ""
        bbb=readLetterBraid(i[1])
        tmp= forAnna(bbb[0],bbb[1])
        print tmp
        print ""
        s+=i[0]+"\n"+tmp+"\n"
    f=open("c://knot.txt",'w')
    f.write(s)
    f.close()
##knot_source()
##########################3from DTcode now
def FromDT(s):
    tab=s.split("\n")
    for i in tab:
        r="BR[DTCode["
        for c in i:
            n=ord(c)
            if n>96:
                r+=str((n-96)*2)+","
            else:
                r+="-"+str((n-64)*2)+","
        print r[:len(r)-1]+"]]"

def extractBraid(s):
    res=""
    for i in s:
        if i!="\n": res+=i
    s=res
    tab=s.split("{")
    tab=[(i.split("}"))[0] for i in tab]
    del tab[0:1]
    tab=[i.split(",") for i in tab]
    print tab
    s=""
    for i in tab:
        b=[]
        mm=0
        for jj in i:
            j=int(jj)
            if j>0:
                b.append([0,j-1])
                if j>mm:mm=j-1
            else:
                b.append([1,-j-1])
                if -j>mm:mm=-j-1
        print b
        tmp= forAnna(b,mm)
        print tmp
        print ""
        s+="\n"+tmp+"\n"
    f=open("c://temp//knot16cr.txt",'w')
    f.write(s)
    f.close()
##extractBraid("""
##Out[9]=
##BR[7,{1,2,3,-4,5,6,5,-4,5,-4,-3,-2,-1,-4,-3,-2,-4,-3,-4,5,-4,3,2,5,-4,3,5,-4,-6,5}]
##
##Out[10]=
##BR[9,{1,2,-3,4,5,6,7,6,-5,-4,3,-2,-1,6,-5,4,3,-2,6,-5,4,3,4,6,5,6,-7,6,-8,-7,6,-5,-4,-3,2,-5,6,7,6,-5,-4,-3,-5,-4,-5,8}]
##
##Out[11]=
##BR[7,{1,2,3,4,-5,4,-3,-2,-1,4,4,-3,-2,-3,-5,6,-5,4,-3,2,4,-3,4,6}]
##
##Out[12]=
##BR[7,{1,2,
##  3,4,5,-6,5,-4,-3,-2,-1,5,5,-4,-3,-2,-4,3,4,5,5,5,5,6,5,-4,-3,2,5,-4,3,5,-4,5}]
##
##Out[13]=
##BR[9,{1,2,
##  3,4,5,6,7,6,-5,-4,-3,-2,-1,6,-5,-4,-3,-2,6,6,-5,-4,-3,-5,4,5,6,-7,6,-8,7,
##    6,-5,-4,3,2,6,-5,4,3,6,-5,4,6,5,6,-7,8}]
##
##Out[14]=
##BR[5,{1,-2,3,-4,-4,3,3,-2,-1,-2,-4,-4,-4,-4,-4,3,-2,3}]
##
##Out[15]=
##BR[7,{1,2,
##  3,4,-5,-6,-5,-4,-3,-2,-1,-5,6,-5,4,-3,2,4,-3,4,-5,-5,4,4,-3,-2,-3,-5}]
##
##Out[16]=
##BR[7,{1,2,
##  3,4,5,-6,5,-4,-3,-2,-1,5,5,-4,-3,-2,-4,3,4,5,5,-6,5,-4,-3,2,5,-4,3,5,-4,5}]
##
##Out[17]=
##BR[7,{1,2,
##  3,4,-5,6,-5,-4,-3,-2,-1,4,-3,2,4,-3,4,-5,-5,4,4,-3,-2,-3,-6,-5,-5,-5}]
##
##Out[18]=
##BR[9,{1,2,
##  3,4,5,-6,7,-6,-5,-4,-3,-2,-1,-5,-4,-3,-2,5,-4,3,5,-4,5,-6,-6,
##    5,5,-4,-3,2,-4,3,4,-7,8,7,-6,5,-6,-7,-6,-8}]
##
##Out[19]=
##BR[7,{1,2,-3,4,-3,-2,-1,-3,-2,-3,-5,6,-5,4,-3,2,4,-3,4,6,-5,-5}]
##
##Out[20]=
##BR[9,{1,2,-3,4,5,6,7,6,-5,-4,3,-2,-1,6,-5,4,3,-2,6,-5,4,3,4,6,5,6,-7,8,-7,
##    6,-5,-4,-3,2,-5,6,-5,-4,-3,-5,-4,-5,-7,-8}]
##
##Out[21]=
##BR[7,{1,2,
##  3,-4,5,-4,-3,-2,-1,-4,-3,-2,-4,3,4,5,5,-6,5,-4,-3,2,5,-4,3,5,-4,5,-6,-6}]
##
##Out[22]=
##BR[7,{1,2,
##  3,-4,5,-4,-3,-2,-1,-6,5,-4,-3,-2,-4,-3,-4,-6,5,-4,3,2,5,-4,3,5,-4,-6}]
##
##Out[23]=
##BR[7,{1,2,-3,4,-5,4,-3,-2,-1,-3,-2,-3,6,-5,4,-3,2,4,-3,4,6,-5}]
##
##Out[24]=
##BR[7,{1,-2,-3,-4,5,-4,3,2,-1,5,-4,3,2,3,5,4,
##    5,-6,5,-4,-3,-2,-4,5,-4,-3,-4,-6,-6,-6}]
##
##Out[25]=
##BR[9,{1,2,
##  3,4,5,6,-7,-8,-7,-6,-5,-4,-3,-2,-1,6,-5,4,3,-2,6,-5,4,3,4,6,5,6,-7,
##    6,-5,-4,-3,2,-5,6,-5,-4,-3,-5,-4,-5,8,-7}]
##
##Out[26]=
##BR[7,{1,2,-3,-4,-5,6,-5,4,3,-2,-1,6,-5,4,3,-2,6,-5,4,3,-5,6,-5,4,-5,
##    6,-5,-4,-3,2,-5,-4,-3,-5,-4,-5}]
##
##Out[27]=
##BR[7,{1,2,3,4,-5,6,-5,-5,4,-3,-2,-1,4,4,-3,-2,-3,-5,-6,-5,4,-3,2,4,-3,4}]
##
##Out[28]=
##BR[7,{1,2,
##  3,4,5,-6,5,-4,-3,-2,-1,5,5,-4,-3,-2,-4,3,4,5,5,5,6,5,5,-4,-3,2,5,-4,3,5,-4,
##    5}]
##
##Out[29]=
##BR[7,{1,2,
##  3,-4,5,-6,5,-4,-3,-2,-1,5,-4,-3,-2,-4,-3,-4,-6,5,-4,3,2,5,-4,3,5,-4}]
extractBraid("""
Out[30]=
BR[7,{1,2,
  3,-4,5,-4,-6,5,-4,-3,-2,-1,5,-4,-3,-2,-4,-3,-4,6,5,-4,3,2,5,-4,3,5,-4,5}]

Out[31]=
BR[7,{1,2,
  3,-4,5,5,5,-6,5,5,-4,-3,-2,-1,-4,-3,-2,-4,3,4,5,6,5,-4,-3,2,5,-4,3,5,-4,5}]

Out[32]=
BR[7,{1,2,-3,-4,-5,6,-5,4,3,-2,-1,6,-5,4,3,-2,6,-5,4,-3,6,-5,6,-5,4,-5,
    6,-5,-4,3,2,-4,-3,-5,-4,-5}]

Out[33]=
BR[7,{1,2,-3,4,-5,4,4,-3,-2,-1,-3,2,3,4,5,4,6,-5,-5,4,4,-3,-2,-5,4,-3,4,-6}]

Out[34]=
BR[7,{1,2,
  3,-4,5,-6,5,5,-4,-3,-2,-1,-4,3,2,4,3,5,6,5,-4,5,5,5,-4,-3,-2,-4,-3,5,-4,5}]

Out[35]=
BR[9,{1,2,
  3,4,-5,6,-7,6,6,-5,-4,-3,-2,-1,-5,4,3,-2,5,4,-3,6,7,6,-5,-8,-7,6,-5,4,5,
    6,7,6,-5,-4,3,2,-4,-3,-5,-4,6,-5,6,8}]

Out[36]=
BR[7,{1,2,
  3,4,-5,6,-5,-4,-3,-2,-1,4,-3,-2,-3,-5,4,-3,2,4,-3,4,-5,-5,-6,-5,-5,4}]

Out[37]=
BR[7,{1,2,3,4,-5,6,-5,-4,-3,-2,-1,-5,4,4,-3,-2,-3,6,-5,4,-3,2,4,-3,4,-5}]

Out[38]=
BR[7,{1,2,-3,-4,-5,6,-5,4,3,-2,-1,6,-5,4,-3,-2,6,-5,4,-3,6,-5,4,4,6,-5,
    4,3,2,-4,3,-5,-4,-5}]

Out[39]=
BR[7,{1,2,
  3,4,5,-6,5,-4,-3,-2,-1,5,-4,-3,-2,5,-4,-3,5,-4,-5,6,-5,4,3,2,6,5,4,3,-6,
    5,4,-6,5,-6}]

Out[40]=
BR[7,{1,2,
  3,4,-5,6,-5,-4,-3,-2,-1,5,-4,-3,-2,5,-4,-3,5,-4,-5,6,-5,4,3,2,6,5,4,3,-6,
    5,4,-6,5,6}]

Out[41]=
BR[7,{1,2,
  3,4,5,-6,5,-4,-3,-2,-1,-5,-4,-3,-2,5,-4,-3,5,-4,-5,6,-5,4,3,2,6,5,4,3,-6,
    5,4,-6,-5,-6}]

Out[42]=
BR[7,{1,2,
  3,4,-5,6,-5,-4,-3,-2,-1,-5,-4,-3,-2,5,-4,-3,5,-4,-5,6,-5,4,3,2,6,5,4,3,-6,5,
    4,-6,-5,6}]

Out[43]=
BR[7,{1,2,
  3,4,5,-6,5,-4,-3,-2,-1,5,-4,-3,-2,-5,-4,-3,5,-4,-5,6,-5,4,3,2,6,5,
    4,3,6,-5,4,6,5,-6}]

Out[44]=
BR[7,{1,2,
  3,4,-5,6,-5,-4,-3,-2,-1,5,-4,-3,-2,-5,-4,-3,5,-4,-5,6,-5,4,3,2,6,5,
    4,3,6,-5,4,6,5,6}]

Out[45]=
BR[7,{1,2,
  3,4,-5,6,-5,-4,-3,-2,-1,-5,-4,-3,-2,-5,-4,-3,5,-4,-5,6,-5,4,3,2,6,5,4,
    3,6,-5,4,6,-5,6}]

Out[46]=
BR[7,{1,2,
  3,4,5,-6,5,-4,-3,-2,-1,5,-4,-3,-2,5,-4,-3,-5,-4,-5,6,-5,4,3,
    2,6,-5,4,3,-6,5,4,-6,5,-6}]

Out[47]=
BR[7,{1,2,
  3,4,-5,6,-5,-4,-3,-2,-1,-5,-4,-3,-2,5,-4,-3,-5,-4,-5,6,-5,4,3,2,6,-5,4,3,-6,
    5,4,-6,-5,6}]

Out[48]=
BR[7,{1,2,
  3,4,5,-6,5,-4,-3,-2,-1,5,-4,-3,-2,5,-4,-3,-5,-4,-5,6,-5,4,3,2,6,-5,4,3,
    6,5,4,-6,5,-6}]

Out[49]=
BR[7,{1,2,
  3,4,-5,6,-5,-4,-3,-2,-1,-5,-4,-3,-2,5,-4,-3,-5,-4,-5,6,-5,4,3,2,6,-5,4,3,6,
    5,4,6,-5,6}]

Out[50]=
BR[7,{1,2,
  3,4,-5,6,-5,-4,-3,-2,-1,-5,-5,4,-3,-2,4,-3,-4,6,5,-4,3,2,5,4,3,-5,4,-5}]

Out[51]=
BR[7,{1,2,
  3,4,5,-6,5,-4,-3,-2,-1,5,5,4,-3,-2,4,-3,-4,-6,5,-4,3,2,5,4,3,-5,4,-5}]

Out[52]=
BR[7,{1,2,
  3,4,-5,6,-5,-4,-3,-2,-1,-5,-5,-4,-3,-2,4,-3,-4,6,5,-4,3,2,5,4,3,5,-4,5}]

Out[53]=
BR[7,{1,2,
  3,4,5,-6,5,-4,-3,-2,-1,5,5,-4,-3,-2,4,-3,-4,-6,5,-4,3,2,5,4,3,5,-4,5}]

Out[54]=
BR[7,{1,2,
  3,4,-5,6,-5,-4,-3,-2,-1,-5,-5,4,-3,-2,-4,-3,-4,6,5,-4,3,2,5,-4,3,-5,4,-5}]

Out[55]=
BR[7,{1,2,
  3,4,5,-6,5,-4,-3,-2,-1,5,5,4,-3,-2,-4,-3,-4,-6,5,-4,3,2,5,-4,3,-5,4,-5}]

Out[56]=
BR[7,{1,2,
  3,4,-5,6,-5,-4,-3,-2,-1,-5,-5,4,-3,-2,-4,-3,-4,6,5,-4,3,2,5,-4,3,5,4,-5}]

Out[57]=
BR[7,{1,2,
  3,4,5,-6,5,-4,-3,-2,-1,5,5,4,-3,-2,-4,-3,-4,-6,5,-4,3,2,5,-4,3,5,4,5}]

Out[58]=
BR[7,{1,2,
  3,4,-5,6,-5,-4,-3,-2,-1,-5,-5,-4,-3,-2,-4,-3,-4,6,5,-4,3,2,5,-4,3,5,-4,-5}]

Out[59]=
BR[7,{1,2,
  3,4,-5,6,-5,-4,-3,-2,-1,-5,-5,-4,-3,-2,-4,-3,-4,6,5,-4,3,2,5,-4,3,5,-4,5}]

Out[60]=
BR[7,{1,2,3,4,-5,4,-3,-2,-1,4,-3,-2,4,-3,-4,6,5,-4,3,2,5,4,3,-5,4,-5,6,6}]

Out[61]=
BR[7,{1,2,3,-4,5,-4,-3,-2,-1,-4,-3,-2,4,-3,-4,-6,5,-4,3,2,5,4,3,5,-4,5,-6,-6}]

Out[62]=
BR[7,{1,2,3,4,-5,4,-3,-2,-1,4,-3,-2,-4,-3,-4,6,5,-4,3,2,5,-4,3,-5,4,-5,6,6}]
""")
##        
##FromDT("""bdegahjclmfnpoki
##bdegahjclofnpikm
##bdegahjcmfnoilpk
##bdegahjcmfnoipkl
##bdegahjcmfnoiplk
##bdegahjcmfnopikl
##bdegahjcmfnopilk
##bdegahjcmfonilpk
##bdegahjcmfopnilk
##bdegahjcmfponilk
##bdegahjcmkfnoipl
##bdegahjcmkfoipln
##bdegahjcmkfonipl
##bdegahjcmlfnpoki
##bdegahjcmnfiokpl
##bdegahjcnkfopiml
##bdegahjcnkfpoiml
##bdegahjcnmfiokpl
##bdegahjcoflnpkim
##bdegahjcofmnpkil
##bdegahjcolfnpikm
##bdegahkclmnfpoji
##bdegahkclmofipjn
##bdegahkclmofnipj
##bdegahkclnfjoipm
##bdegahkclnfjopim
##bdegahkclnfjpoim
##bdegahkclnfpiojm
##bdegahkclnfpjoim
##bdegahkclnmfpoji
##bdeGaHJCKMFNOIPL
##bdeGaHJCKMFnoIpl
##bdeGaHJCKmFNOiPL
##bdeGaHJCKmFnoipl
##bdeGahjCkMfNOIPL
##bdeGahjCkMfnoIpl
##bdeGahjCkmfnoipl
##bdegaHJcKMFNOIPL
##bdegaHJcKmFnoipl
##bdegahJcKMFNOIPL
##bdegahJckmFnoipl
##bdeGaHJCKMFOIPLN
##bdeGaHJCKmFoipln
##bdeGahjCkMfOIPLN
##bdeGahjCkmfoipln
##bdegaHJcKMFOIPLN
##bdegaHJcKmFoipln
##bdegahJcKMFOIPLN
##bdegahJckmFoipln
##bdegahjcKMfOIPLN
##bdegahjckMfOIPLN
##bdeGaHJCKMFONIPL
##bdeGahjCkmfonipl
##bdegaHJcKMFONIPL
##""")
