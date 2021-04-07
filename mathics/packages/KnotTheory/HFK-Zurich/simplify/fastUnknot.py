import RectDia
##import FastRectDiag
from RectDia import RectDia
from FastRectDiag import FastRectDiag
import profile
def unknot(diag):
    diag=FastRectDiag(diag)
    print diag.xSorted
    print diag.ySorted
    counter=0
    n=diag.complexity
    stack=[diag]
    hmap=dict({diag.hashInt():0})
    while(1):
        if len(stack)==0: return ("not an unknot",diag)
        else: diag=stack.pop()
##        diag.draw()##debug!!
        if diag.complexity<3: return ("not knotted",diag)
        if diag.isdestabilisable():
            print "reduction!"
            print len(stack)
##            diag.draw()##debug!!
##            print diag.xSorted
##            print diag.ySorted
            des=diag.isdestabilisable()
            tmp=diag.copy()
            tmp.m_destabilisation(des[0],des[1])
            tmp.predecessor=diag
            diag=tmp
##            print len(diag.points)
##            print diag.xSorted
##            print diag.ySorted
            stack=[diag]
##            stack[0].draw()##debug!!
            hmap=dict({diag.hashInt():0})
        succ=diag.fastsuccCa(hmap)
##        print len(diag.succCa())
        for k in succ:
##            if not hmap.has_key(k.hashInt()):
            k.predecessor=diag
            stack=[k]+stack
            hmap[k.hashInt()]=0
        if len(hmap)>=500+counter or len(hmap)<=-500+counter:
            counter=len(hmap)
            print "Please wait!"
            print len(hmap)
if __name__ == "__main__":
    dd=RectDia([(0,0),(0,4),(1,2),(1,8),(2,7),(2,9),(3,6),(3,8),(4,1),(4,3),(5,2),(5,7),(6,0),(6,3),(7,1),(7,5),(8,4),(8,6),(9,5),(9,9)])
##    dd=RectDia([(0,23),(0,6),(1,21),(1,7),(2,19),(2,11),(3,0),(3,4),(4,5),(4,18),(5,3),(5,1),(6,7),(6,2),(7,16),(7,8),(8,11),(8,6),(9,13),(9,5),(10,12),(10,4),(11,9),(11,3),(12,15),(12,8),(13,21),(13,13),(14,10),(14,1),(15,15),(15,9),(16,17),(16,14),(17,16),(17,12),(18,18),(18,10),(19,14),(19,0),(20,20),(20,17),(21,22),(21,19),(22,23),(22,20),(23,22),(23,2)])
##    dd.draw()
##    import profile
    profile.run("print unknot(dd)")
##    print unknot(dd)
