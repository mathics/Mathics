import highLevel
import braid2rect
import rectDiagMisc
##reload(bdMapGeneral)
s="i"
while(s!="y" and s!="n"):
    s=raw_input("Do you want to enter a knot by its rectangular diagram (the alternative is its table location (for knots with strictly less than 13 crossings)?(y/n)")
if s=="n":
    i=int(raw_input("knot nb of crossings:"))
    j=int(raw_input("knot nb:"))
    if braid2rect.atlas.has_key((i,j)):
        rect=braid2rect.atlas[(i,j)]
        print "KNOT:",i,"n",j
        print rect
        print rectDiagMisc.toStringNice(rect)
        print highLevel.AllToString(rect)
    else:
        print "Not in the table!"
else:
    print "Please enter the rectangular diagram as a list of pairs of coordinates:"
    print "(example: [[0,0],[0,1],[1,0],[1,1]] is the trivial knot (the 4 corners of a rectangle))"
    s=raw_input("")
    rect=eval(s)
    print "How hard do you want the program to try to simplify your rectangular diagram?(0-100000) 0: no simplification 5000: pretty good compromise"
    print "Warning! The running time of the program is not linear in this number!"
    br=int(raw_input(""))
    import simplify.diagSimplify
    if br!=0: rect=simplify.diagSimplify.simplifyPoints(rect,br)
    print rectDiagMisc.toStringNice(rect)
    print highLevel.AllToString(rect)
##raw_input("w")
