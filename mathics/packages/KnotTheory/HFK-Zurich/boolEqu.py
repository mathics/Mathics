def setV(sys,next):
    while(next):
        i=0
        (var,val)=next.pop()
        while(i<len(sys)):
##            print sys,next
            for j in [0,1]:
                if sys[i][j*2]==var:
                    if sys[i][j*2+1]==val:
                        del sys[i:i+1]
                        if not sys: return 1
                        i-=1
                        break
                    else:
                        if sys[i][(1-j)*2]==var and sys[i][(1-j)*2+1]!=val:
                            return 0
                        else:
                            next.append((sys[i][(1-j)*2],sys[i][(1-j)*2+1]))
                            del sys[i:i+1]
                            if not sys: return 1
                            i-=1
                            break
                    
            i+=1
def bEqu(sys,equ):
    i=0
    while(i<len(sys)):
        if sys[i][0]==equ[0] and sys[i][1]!=equ[1]:
            sys[i][0]=equ[2]
            sys[i][1]=equ[3]
        if sys[i][2]==equ[2] and sys[i][3]!=equ[3]:
            sys[i][2]=equ[2]
            sys[i][3]=equ[3]
        if sys[i][0]==sys[i][2]:
            
        i+=1
print setV([[2,0,0,0],[2,1,5,3],[2,0,2,0]],[(2,1)])
