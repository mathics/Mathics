/*
 * Created on Nov 30, 2005
 */
package wikilink;


public class wikidelete {

	static String tooFewArgs = "wikiset requires four arguments!\n"
			+ "Usage: wikidelete <wikiBaseURL> <username> <password> <pagename> [reason]";

	public static void main(String[] args) {
		if (args.length < 4) {
			System.err.println(tooFewArgs);
			return;
		}

		String baseURL = args[0], username = args[1], password = args[2], pagename = args[3];

		StringBuffer reason = new StringBuffer();
		for (int i = 4; i < args.length; ++i) {
			reason.append(args[i]);
			reason.append(" ");
		}

		MediawikiConnection conn = new MediawikiConnection(baseURL, username,
				password);
		if (!conn.deletePage(pagename, reason.toString())) {
			System.err.println("Failed to delete " + pagename);
		}

	}
}
