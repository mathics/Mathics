def nn(mat,i,j,di,inv):
    r=0
    if j<yLen/di
    for c in di:
        r*=2
        r+=mat[i][j*di+c]
    return r
    
def compressedMat(mat):
    xLen=len(mat)
    yLen=len(mat[0])
    yl=yLen/16+1
    tab=[nn(mat,i,j,16,0) for j in xrange(yl) for i in range(xLen))]
    
    
