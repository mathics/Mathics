/*
 * Created on Nov 30, 2005
 */
package wikilink;

public class wikilink {

	public static void main(String[] args) {
		if(args.length == 0) {
			System.err.println("usage: \"wikilink set ...\" or \"wikilink get ...\"");
			return;
		}
		
		String[] xargs = new String[args.length - 1];
		System.arraycopy(args, 1, xargs, 0, args.length - 1);
		
		if(args[0].equals("set")) {
			wikiset.main(xargs);
		} else if(args[0].equals("get")) {
			wikiget.main(xargs);
		} else {
			System.err.println("Unknown wikilink command: " + args[0]);
		}
	}
	
}
