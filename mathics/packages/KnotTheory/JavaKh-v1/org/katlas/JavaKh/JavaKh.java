package org.katlas.JavaKh;
import java.io.BufferedReader;
import java.io.InputStreamReader;
public class JavaKh {
    public static boolean using_h = false;
    public static void main(String args[]) throws java.io.IOException {
	switch (args.length) {
	case 0:
	    BaseRing.setRing("Rational");
	    break;
	case 1:
	    if (args[0].equals("-Q"))
		BaseRing.setRing("Rational");
	    else if (args[0].equals("-Z"))
		BaseRing.setRing("Int");
	    else if (args[0].equals("-U")) {
		using_h = true;
		BaseRing.setRing("Int");
	    } else
		printHelp();
	    break;
	case 2:
	    if (args[0].equals("-mod")) {
		int p = Integer.parseInt(args[1]);
		if (p == 0)
		    BaseRing.setRing("Rational");
		else {
		    BaseRing.setRing("ModP");
		    ModP.setP(p);
		}
	    } else
		printHelp();
	    break;
	default:
	    printHelp();
	}
	//Komplex.checkReidemeister();
	BufferedReader br=new BufferedReader(new InputStreamReader(System.in));
	while (true) {
	    int knot[][] = Komplex.getPD(br);
	    if (knot == null)
		break;
	    Komplex k = Komplex.generateFast(knot, Komplex.getSigns(knot));
	    assert k.check(true);
	    System.out.println("\"" + k.Kh() + "\"");
	    //k.debugPrint();
	}
	br.close();
    }

    public static void printHelp() {
	System.out.println("Usage: java JavaKh [OPTION]\n"
			   +"Options specify the base ring class:\n"
			   +"  -Q        Rationals (default)\n"
			   +"  -Z        Integers\n"
			   +"  -mod p    Modulus p\n"
			   +"  -U        Universal homology (over Z)\n"
			   +"Any other option gives this message");
	System.exit(0);
    }
}
