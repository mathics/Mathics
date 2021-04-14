from RectDia import RectDia
import profile
def unknot(diag):
    counter=0
    n=diag.getSize()
    stack=[(diag,0)]
    hmap=dict({diag.hashInt():0})
    while(1):
        if len(stack)==0:
            print diag.toString()
            return "not an unknot"
            
        else:
            (diag,depth)=stack.pop()
            
##        diag.draw()##debug!!
        if diag.getSize()<3: return "not knotted"
        succDe=diag.succTrDe()
        if len(succDe)!=0:
            print "reduction!"
            print len(stack)
            print depth
##            diag.draw()##debug!!
            diag=succDe[0]
##            print len(diag.points)
            stack=[(diag,0)]
            depth=0    
##            stack[0].draw()##debug!!
            hmap=dict({diag.hashInt():0})
        succ=diag.succCa()
        print len(succ)
        for k in succ:
            if not hmap.has_key(k.hashInt()):
                stack=[(k,depth+1)]+stack
                hmap[k.hashInt()]=0
        if len(hmap)>=500+counter or len(hmap)<=-500+counter:
            counter=len(hmap)
            print "Please wait!"
            print len(hmap)
            print depth

def unknotByFlipe(diag):
    while 1:
        n=diag.getSize()
        succ=diag.succfl()
        for k in succ:
            if k.getSize()<n:
                print "reduction"
                diag=k
                break
        if n==diag.getSize():break
    print "finished"
    diag.draw()
        
if __name__ == "__main__":
##    dd=RectDia([(0,0),(0,3),(1,1),(1,2),(2,2),(2,3),(3,0),(3,1)])##c
##    dd=RectDia([(0,0),(2,0),(0,2),(2,2),(1,1),(1,3),(3,1),(3,3)])##bracelet
##    dd=RectDia([(2,0),(1,1),(0,2),(0,4),(1,3),(2,2),(3,1),(4,0),(4,3),(3,4)])##threefoil
##    dd=RectDia([(2,0),(1,1),(0,2),(0,4),(1,3),(2,2),(3,1),(4,0),(4,3),(3,5),(5,4),(5,5)])##threefoil  dd=RectDia([(0,0),(0,4),(1,2),(1,8),(2,7),(2,9),(3,6),(3,8),(4,1),(4,3),(5,2),(5,7),(6,0),(6,3),(7,1),(7,5),(8,4),(8,6),(9,5),(9,9)])
##    dd=RectDia([(0,0),(0,4),(1,2),(1,8),(2,7),(2,9),(3,6),(3,8),(4,1),(4,3),(5,2),(5,7),(6,0),(6,3),(7,1),(7,5),(8,4),(8,6),(9,5),(9,9)])
    dd=RectDia([(0,23),(0,6),(1,21),(1,7),(2,19),(2,11),(3,0),(3,4),(4,5),(4,18),(5,3),(5,1),(6,7),(6,2),(7,16),(7,8),(8,11),(8,6),(9,13),(9,5),(10,12),(10,4),(11,9),(11,3),(12,15),(12,8),(13,21),(13,13),(14,10),(14,1),(15,15),(15,9),(16,17),(16,14),(17,16),(17,12),(18,18),(18,10),(19,14),(19,0),(20,20),(20,17),(21,22),(21,19),(22,23),(22,20),(23,22),(23,2)])
##    dd=RectDia([(0,0),(1,0),(1,1),(2,1),(2,2),(0,2)])
    dd.draw()
    print unknot(dd)
    
