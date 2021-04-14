/*
 * Created on Nov 30, 2005
 */
package wikilink;

import java.io.IOException;

import org.apache.commons.httpclient.HttpException;
import org.jdom.JDOMException;

public class wikiget {

	static String tooFewArgs = "wikiget requires at least two arguments!\n" + 
		"Usage: wikiget wikiBaseURL pagename";

	public static void main(String[] args) {
		if(args.length < 2) {
			System.err.println(tooFewArgs);
			return;
		}
	
	String baseURL = args[0], pagename = args[1];
	
	MediawikiConnection conn = new MediawikiConnection(baseURL);
	
	try {
		System.out.println(conn.getPageText(pagename));
	} catch (HttpException e) {
		e.printStackTrace();
	} catch (IOException e) {
		e.printStackTrace();
	} catch (JDOMException e) {
		e.printStackTrace();
	}
	
}
}
