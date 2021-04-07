/*
 * Created on Nov 30, 2005
 */
package wikilink;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class wikiset {

	static String tooFewArgs = "wikiset requires at least four arguments!\n"
			+ "Usage: wikiset wikiBaseURL username password pagename";

	public static void main(String[] args) {
		if (args.length < 4) {
			System.err.println(tooFewArgs);
			return;
		}

		String baseURL = args[0], username = args[1], password = args[2], pagename = args[3];

		StringBuffer in = new StringBuffer();
		String line;
		BufferedReader r = new BufferedReader(new InputStreamReader(System.in));
		try {
			while ((line = r.readLine()) != null) {
				in.append(line);
				in.append("\n");
			}
		} catch (IOException e) {
			e.printStackTrace();
		}

		MediawikiConnection conn = new MediawikiConnection(baseURL, username,
				password);
		if (conn.setPageTexts(new String[][] { new String[] { pagename,
				in.toString() } }).length > 0) {
			System.err.println("Failed to set text of " + pagename);
		}

	}
}
