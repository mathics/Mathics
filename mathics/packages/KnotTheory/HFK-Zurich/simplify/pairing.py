infty=1000000
def cheapestPath(states,transitionCost):
    #states is of the form [[0,0,0],[0,0],[0,0,0],[(0,0),(0,0)]] with (0,0)'s in the last column
    for n in range(1,len(states)):
        for s in range(len(states[len(states)-n-1])):
            cost=infty+1
            for t in range(len(states[len(states)-n])):
                tmp=states[len(states)-n][t][1]+transitionCost(len(states)-n-1,s,t)
                if cost>tmp:
                    cost=tmp
                    states[len(states)-n-1][s]=(t,cost)
    start=-1
    cost=infty
    for s in range(len(states[0])):
        if cost>states[0][s][1]:
            start=s
            cost=states[0][s][1]
    path=[start]
    for n in range(len(states)-1):
        start=states[n][start][0]
        path.append(start)
    return (path,cost)
def minPairing(matrix):
    pairing=range(len(matrix))
    while(improve(pairing,matrix)):
        pass
    return pairing
def weight(pairing,matrix):
    s=0
    for i in range(len(pairing)):
        s+=matrix[i][pairing[i]]
    return s
def improve(pairing,matrix):
    l=len(matrix)
    for start in range(l):
        states=[]
        for i in range(l+1):
            states.append(l*[(0,0)])
        def transitionCost(n,x,y):
            if x==y and y==start and n==0: return infty
            if n==0 and x!=start: return infty
            if n==l-1 and y!=start: return infty
            if x==y: return 0
            return matrix[x][pairing[y]]-matrix[x][pairing[x]]
        path=cheapestPath(states,transitionCost)
##        print path
##        print pairing
        if path[1]>=0: continue
        path=path[0]
        visit=l*[-1]
        for i in range(len(path)):
            if visit[path[i]]>=0 and path[i-1]!=path[i]:
                cycleStart=visit[path[i]]
                end=i
                break
            visit[path[i]]=i
        tmp=pairing[path[cycleStart]]
        for i in range(cycleStart,end-1):
            pairing[path[i]]=pairing[path[i+1]]
        pairing[path[end-1]]=tmp
        return 1
    return 0
from random import randint
def rMatrix(l):
    states=[]
    for i in range(l):
        states.append(l*[0])
    for i in range(l):
        for j in range(l):
            states[i][j]=randint(1,1000)
    return states
    
def trtr(n,x,y):
    if x==0 and y==1 and n==0:
        return 2
    if x==1 and y==0 and n==2:
        return 0
    return 5
if __name__ == "__main__":
    matrix=[[10,2,3,5],[2,10,1,3],[3,1,10,2],[5,3,2,10]]
    for i in range(1):
        matrix=rMatrix(60)
        print matrix
        print minPairing(matrix)
## 20:3s 40:18s 60: 61 s
