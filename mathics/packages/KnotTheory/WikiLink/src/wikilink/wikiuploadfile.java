/*
 * Created on Aug 31, 2005
 */
package wikilink;

import java.io.IOException;

/**
 * @author Scott Morrison
 */
public class wikiuploadfile {

	static String tooFewArgs = "wikiuploadfile requires at least four arguments!\n" +
		"Usage: wikiuploadfile wikiBaseURL username password filename";
	
	public static void main(String[] args) {
		if(args.length < 4) System.err.println(tooFewArgs);
		
		String baseURL = args[0], username = args[1], password = args[2], filename = args[3];
		String description = "";
		for(int i = 4; i < args.length; i++) {
			description = description + args[i];
		}
		
		MediawikiConnection conn = new MediawikiConnection(baseURL, username, password);
		
		boolean result = false;
		try {
			result = conn.uploadFile(filename, description);
		} catch (IOException e) {
			System.err.println("IOException while attempting upload: " + filename);
			e.printStackTrace();
		}
		if(! result) System.err.println("Upload may have failed: " + filename);
	}
}
