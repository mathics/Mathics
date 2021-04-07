#! /usr/bin/python
import sys
f=open("in","r")
s=f.read()
f.close()
s=s.replace("{","[")
s=s.replace("}","]")
rect=eval(s)
for pair in rect:
    pair[0]-=1
    pair[1]-=1
tmp=sys.stdout
sys.stdout=sys.stderr
import highLevel
##import braid2rect
import rectDiagMisc
import simplify.diagSimplify
rect=simplify.diagSimplify.simplify(rect,5000)
res= highLevel.batchVersion(rect)
sys.stdout=tmp
f=open("out","w")
f.write(str(res))
f.close()
##print res
