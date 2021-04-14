/*
 * Created on Jul 15, 2007
 */
package wikilink;

import java.io.IOException;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.commons.httpclient.HttpException;
import org.jdom.JDOMException;

public class Test {

	public static void main(String[] args) throws HttpException, IOException, JDOMException {
		MediawikiConnection connection = new MediawikiConnection("http://katlas.math.toronto.edu/w/index.php", "ScottTestRobot", "foobar");
		String date = new Date().toString();
		System.out.println("Setting two pages, one at a time.");
		connection.setPageText("Sandbox/wikilink1", "First edit, at " + date);
		connection.setPageText("Sandbox/wikilink2", "Second edit, also at " + date);
		System.out.println("Getting a single page, \"Sandbox/wikilink1\".");
		System.out.println(connection.getPageText("Sandbox/wikilink1"));
		System.out.println("Getting two pages at once:");
		Map<String, String> pages = connection.getPageTexts("Sandbox/wikilink1", "Sandbox/wikilink2");
		for(Entry<String, String> page : pages.entrySet()) {
			System.out.println(page.getKey());
			System.out.println(page.getValue());
		}
		
		date = new Date().toString();
		System.out.println("Setting two pages at once.");
		Map<String, String> texts = new HashMap<String, String>();
		texts.put("Sandbox/wikilink3", "Third edit, at " + date);
		texts.put("Sandbox/wikilink4", "Fourth edit, also at " + date);
		connection.setPageTexts(texts);
		System.out.println("Getting a single page, \"Sandbox/wikilink3\".");
		System.out.println(connection.getPageText("Sandbox/wikilink3"));
		System.out.println("Getting two pages at once:");
		pages = connection.getPageTexts("Sandbox/wikilink3", "Sandbox/wikilink4");
		for(Entry<String, String> page : pages.entrySet()) {
			System.out.println(page.getKey());
			System.out.println(page.getValue());
		}	
		
	}
	
}
