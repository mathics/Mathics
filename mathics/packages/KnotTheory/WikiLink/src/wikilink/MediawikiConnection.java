/*
 This page contains the source code for MediawikiConnection.java, the java component of Scott Morrison's [[WikiLink` package]].

 <pre>
 */
package wikilink;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.httpclient.DefaultHttpMethodRetryHandler;
import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.NameValuePair;
import org.apache.commons.httpclient.methods.GetMethod;
import org.apache.commons.httpclient.methods.PostMethod;
import org.apache.commons.httpclient.methods.multipart.FilePart;
import org.apache.commons.httpclient.methods.multipart.MultipartRequestEntity;
import org.apache.commons.httpclient.methods.multipart.Part;
import org.apache.commons.httpclient.methods.multipart.StringPart;
import org.apache.commons.httpclient.params.HttpMethodParams;
import org.apache.commons.lang.StringEscapeUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.jdom.Namespace;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.SAXBuilder;

/**
 * @author Scott Morrison
 */
public class MediawikiConnection {
	private static Log log = LogFactory.getLog(MediawikiConnection.class);

	static {
		System.setProperty("org.apache.commons.logging.Log",
				"org.apache.commons.logging.impl.SimpleLog");
		System.setProperty("org.apache.commons.logging.simplelog.showdatetime",
				"true");
		System
				.setProperty(
						"org.apache.commons.logging.simplelog.log.org.apache.commons.httpclient",
						"error");
	}

	private String baseURL;

	private String username, password;

	private HttpClient client;

	public MediawikiConnection(String baseURL) {
		initialise(baseURL, "", "");
	}

	public MediawikiConnection(String baseURL, String username, String password) {
		initialise(baseURL, username, password);
	}

	private void initialise(String baseURL, String username, String password) {
		this.baseURL = baseURL;
		this.username = username;
		this.password = password;
		renewClient();
	}

	private boolean renewClient() {
		if (client == null) {
			log.debug("Creating new http client");
			client = new HttpClient();
			client.getHttpConnectionManager().getParams().setConnectionTimeout(
					5000);
			return doLogin(username, password);
		}
		return true;
	}

	private void disposeClient() {
		client = null;
	}

	private void sleep() {
		try {
			Thread.sleep(5000);
		} catch (InterruptedException e) {
			// yawn!!! Better get back to work.
			e.printStackTrace();
		}
	}

	private boolean doLogin(String username, String password) {
		if (username == "") {
			log.info("No username specified, not logging in.");
			return true;
		}

		log.info("Attempting to log in.");

		PostMethod post = new PostMethod(baseURL
				+ "?title=Special:Userlogin&action=submit");

		// enable automatic retrying
		post.getParams().setParameter(HttpMethodParams.RETRY_HANDLER,
				new DefaultHttpMethodRetryHandler(3, false));

		NameValuePair[] data = { new NameValuePair("wpName", username),
				new NameValuePair("wpPassword", password),
				new NameValuePair("wpRemember", "1"),
				// ugh... this changed between 1.4beta5 and 1.4.7
				// for 1.4beta5
				new NameValuePair("wpLoginAttempt", "1"),
				// for 1.4.7
				new NameValuePair("wpLoginattempt", "Log in") };

		post.setRequestBody(data);

		int status = 1;
		try {
			status = client.executeMethod(post);
			if (status == 302) {
				log.info("Login succeeded");
				return true;
			} else {
				log.info("Login failed, status code: " + status);
				log.info("Response body: " + post.getResponseBodyAsString());
				return false;
			}
		} catch (HttpException e) {
			// uhoh... the HttpClient object is probably in a bad state.
			// We'll dispose it, and then sleep so we don't just trying logging
			// in again immediately.
			log.warn(e);
			e.printStackTrace();
			disposeClient();
			sleep();
			return false;
		} catch (IOException e) {
			// who cares
			log.warn(e);
			e.printStackTrace();
			disposeClient();
			sleep();
			return false;
		} finally {
			post.releaseConnection();
		}
	}

	public String getPageText(String title) throws HttpException, IOException,
			JDOMException {
		renewClient();

		String URL = baseURL + "?title=Special:Export/" + title;
		GetMethod get = null;
		try {
			get = new GetMethod(URL);

			// enable automatic retrying
			get.getParams().setParameter(HttpMethodParams.RETRY_HANDLER,
					new DefaultHttpMethodRetryHandler(3, false));

			client.executeMethod(get);

			InputStream is = get.getResponseBodyAsStream();

			SAXBuilder builder = new SAXBuilder();
			Document doc = builder.build(is);

			Element root = doc.getRootElement();
			Namespace ns = root.getNamespace();
			Element page = root.getChild("page", ns);

			String text = "";
			if (page != null)
				text = page.getChild("revision", ns).getChild("text", ns)
						.getText();

			return text;
		} finally {
			// finally, make sure we release the connection!
			if (get != null)
				get.releaseConnection();

		}
	}

	static String[][] mapToArray(Map map) {
		String[][] array = new String[map.size()][2];
		Iterator iter = map.keySet().iterator();
		String title;
		int i = 0;
		while (iter.hasNext()) {
			title = (String) iter.next();
			array[i][0] = title;
			array[i][1] = (String) map.get(title);
			i++;
		}
		return array;
	}

	static Map<String, String> arrayToMap(String[][] array) {
		Map<String, String> map = new HashMap<String, String>();
		for (int i = 0; i < array.length; i++) {
			map.put(array[i][0], array[i][1]);
		}
		return map;
	}

	public Map<String, String> getPageTexts(String... titles) {
		return getPageTexts(new HashSet<String>(Arrays.asList(titles)));
	}

	public Map<String, String> getPageTexts(Set<String> titles) {
		Map<String, String> pageTexts = new HashMap<String, String>();
		if (titles.size() == 0)
			return pageTexts;

		renewClient();

		Set<String> excessTitles = new HashSet<String>();

		int titleLimit = 1000;

		String titlesString = "";

		int i = 0;
		Iterator<String> iter = titles.iterator();
		while (iter.hasNext() && i++ < titleLimit) {
			titlesString = titlesString + iter.next() + "\r\n";
		}
		while (iter.hasNext()) {
			excessTitles.add(iter.next());
		}

		String URL = baseURL + "?title=Special:Export";
		PostMethod post = new PostMethod(URL);
		post.getParams().setParameter(HttpMethodParams.RETRY_HANDLER,
				new DefaultHttpMethodRetryHandler(3, false));
		post.getParams().setIntParameter(HttpMethodParams.SO_TIMEOUT, 60000);

		NameValuePair[] data = { new NameValuePair("action", "submit"),
				new NameValuePair("curonly", "true"),
				new NameValuePair("pages", titlesString), };

		post.setRequestBody(data);

		InputStream is = null;
		try {
			client.executeMethod(post);
			is = post.getResponseBodyAsStream();
			// evil by hand debugging:
			// System.out.println(post.getResponseBodyAsString());
		} catch (HttpException e) {
			// uhoh... the HttpClient object is probably in a bad state.
			e.printStackTrace();
			disposeClient();
			post.releaseConnection();
			return pageTexts;
		} catch (IOException e) {
			// who cares
			e.printStackTrace();
			disposeClient();
			post.releaseConnection();
			return pageTexts;
		}

		SAXBuilder builder = new SAXBuilder();
		try {
			Document doc = builder.build(is);
			String title;
			String text;

			Element root = doc.getRootElement();
			Namespace ns = root.getNamespace();
			@SuppressWarnings("unchecked")
			List<Element> pages = root.getChildren("page", ns);

			for (Element page : pages) {
				text = page.getChild("revision", ns).getChild("text", ns)
						.getText();
				title = page.getChild("title", ns).getText().replace(' ', '_');
				pageTexts.put(title, text);
			}

		} catch (IOException e) {
			e.printStackTrace();
			disposeClient();
		} catch (JDOMException e) {
			e.printStackTrace();
		}

		post.releaseConnection();

		pageTexts.putAll(getPageTexts(excessTitles));

		return pageTexts;
	}

	public String[][] filterSetPageTexts(String[][] edits) {
		return mapToArray(filterSetPageTexts(arrayToMap(edits)));
	}

	public Map<String, String> filterSetPageTexts(Map<String, String> edits) {
		Map<String, String> pageTexts = getPageTexts(edits.keySet());
		for (String title : pageTexts.keySet()) {
			String newText = (String) edits.get(title);
			String oldText = (String) pageTexts.get(title);
			if ((newText != null) && (oldText != null)
					&& newText.equals(oldText)) {
				edits.remove(title);
			}
		}
		return edits;
	}

	private Map<String, String> setPageTextsOnce(Map<String, String> edits,
			String summary) {
		Map<String, String> failedEdits = new HashMap<String, String>();
		for (String title : edits.keySet()) {
			String text = edits.get(title);
			if (!setPageText(title, text)) {
				failedEdits.put(title, text);
			}
		}
		return failedEdits;
	}

	private Map<String, String> setFilteredPageTexts(Map<String, String> edits,
			String summary) {
		return setPageTextsOnce(filterSetPageTexts(edits), summary);
	}

	public Map<String, String> setPageTexts(Map<String, String> edits,
			String summary) {
		return setFilteredPageTexts(setFilteredPageTexts(edits, summary),
				summary);
	}

	public Map<String, String> setPageTexts(Map<String, String> edits) {
		return setPageTexts(edits, "");
	}

	public String[][] setPageTexts(String[][] edits, String summary) {
		return mapToArray(setPageTexts(arrayToMap(edits), summary));
	}

	public String[][] setPageTexts(String[][] edits) {
		return setPageTexts(edits, "");
	}

	public boolean setPageText(String title, String text) {
		return setPageText(title, text, "");
	}

	public boolean setPageText(String title, String text, String summary) {
		renewClient();
		if (client == null) {
			log.warn("No http client available.");
			return false;
		}

		String URL = baseURL + "?title=" + title + "&action=edit";

		// first, load the edit page, to get an 'edittime' and 'edittoken'
		String editTime = "", startTime = "";
		String editToken = "";
		String mungedWikiSource = "";
		GetMethod get = new GetMethod(URL);
		// enable automatic retrying
		get.getParams().setParameter(HttpMethodParams.RETRY_HANDLER,
				new DefaultHttpMethodRetryHandler(3, false));

		String response = "";

		try {
			client.executeMethod(get);
			response = get.getResponseBodyAsString();
		} catch (HttpException e) {
			// uhoh... the HttpClient object is probably in a bad state. We'll
			// dispose it in a moment.
			e.printStackTrace();
			disposeClient();
			return false;
		} catch (IOException e) {
			// who cares
			e.printStackTrace();
			disposeClient();
			return false;
		} finally {
			get.releaseConnection();
		}

		// now sift through the response looking for the editime and edittoken
		java.util.regex.Pattern regex1 = java.util.regex.Pattern
				.compile("value=\"([0-9]+)\" name=\"wpEdittime\"");
		java.util.regex.Matcher matcher1 = regex1.matcher(response);
		if (matcher1.find())
			editTime = matcher1.group(1);

    // now sift through the response looking for the editime and edittoken
    regex1 = java.util.regex.Pattern
        .compile("value=\"([0-9]+)\" name=\"wpStarttime\"");
    matcher1 = regex1.matcher(response);
    if (matcher1.find())
      startTime = matcher1.group(1);
		
		java.util.regex.Pattern regex2 = java.util.regex.Pattern
				.compile("value=\"([0-9a-z]+\\+?\\\\?)\" name=\"wpEditToken\"");
		java.util.regex.Matcher matcher2 = regex2.matcher(response);
		if (matcher2.find())
			editToken = matcher2.group(1);

		// while we're at it, let's get the (html-munged) wiki source as well.
		java.util.regex.Pattern regex3 = java.util.regex.Pattern.compile(
				"<textarea .*? name=\"wpTextbox1\" .*?>(.*?)</textarea>",
				java.util.regex.Pattern.DOTALL);
		java.util.regex.Matcher matcher3 = regex3.matcher(response);
		if (matcher3.find())
			mungedWikiSource = matcher3.group(1);

		String wikiSource = StringEscapeUtils.unescapeHtml(mungedWikiSource)
				.trim();

		if (wikiSource.equals(text.trim())) {
			// don't need to do anything
			log
					.info("The wikitext appearing in the edit box agrees with the text we're trying to set, so there's no need to actual submit an edit.");
			return true;
		}

		PostMethod post = new PostMethod(URL);
		post.getParams().setParameter(HttpMethodParams.RETRY_HANDLER,
				new DefaultHttpMethodRetryHandler(3, false));
		post.getParams().setIntParameter(HttpMethodParams.SO_TIMEOUT, 10000);

		int status = 1;
		try {

			// Wiki expects UTF-8 data for text and summary
			// Maybe should do the same for title?

			String utf8_text = null;
			String utf8_summary = null;
			try {
				byte ba[] = text.getBytes("UTF-8");
				utf8_text = new String(ba, "ISO-8859-1");
				byte ba2[] = summary.getBytes("UTF-8");
				utf8_summary = new String(ba2, "ISO-8859-1");
			} catch (Exception ex) {
				ex.printStackTrace();
				disposeClient();
				return false;
			}

			NameValuePair[] data = {
					new NameValuePair("wpTextbox1", utf8_text),
					new NameValuePair("wpEdittime", editTime),
					new NameValuePair("wpStarttime", startTime),
          new NameValuePair("wpEditToken", editToken),
					new NameValuePair("wpSummary", utf8_summary),
					new NameValuePair("wpSave", "Save page") 		
			};

			post.setRequestBody(data);

			status = client.executeMethod(post);
		} catch (HttpException e) {
			// uhoh... the HttpClient object is probably in a bad state.
			e.printStackTrace();
			disposeClient();
		} catch (IOException e) {
			// who cares
			e.printStackTrace();
			disposeClient();
		} finally {
			// debug!
			post.releaseConnection();
		}
		
		return (status == 302);
	}

	public boolean deletePage(String title, String reason) {
		renewClient();
		if (client == null) {
			log.warn("No http client available.");
			return false;
		}

		String URL = baseURL + "?title=" + title + "&action=delete";

		// first, load the edit page, to get an 'edittime' and 'edittoken'
		String editTime = "";
		String editToken = "";
		GetMethod get = new GetMethod(URL);
		// enable automatic retrying
		get.getParams().setParameter(HttpMethodParams.RETRY_HANDLER,
				new DefaultHttpMethodRetryHandler(3, false));

		String response = "";

		try {
			client.executeMethod(get);
			response = get.getResponseBodyAsString();
		} catch (HttpException e) {
			// uhoh... the HttpClient object is probably in a bad state. We'll
			// dispose it in a moment.
			e.printStackTrace();
			disposeClient();
			return false;
		} catch (IOException e) {
			// who cares
			e.printStackTrace();
			disposeClient();
			return false;
		} finally {
			get.releaseConnection();
		}

		// now sift through the response looking for the editime and edittoken
		java.util.regex.Pattern regex1 = java.util.regex.Pattern
				.compile("value=\"([0-9]+)\" name=\"wpEdittime\"");
		java.util.regex.Matcher matcher1 = regex1.matcher(response);
		if (matcher1.find())
			editTime = matcher1.group(1);

		java.util.regex.Pattern regex2 = java.util.regex.Pattern
				.compile("value=\"([0-9a-z]+\\\\?)\" name=\"wpEditToken\"");
		java.util.regex.Matcher matcher2 = regex2.matcher(response);
		if (matcher2.find())
			editToken = matcher2.group(1);

		java.util.regex.Pattern regex3 = java.util.regex.Pattern
				.compile("name='wpEditToken' value=\"([0-9a-z]+\\\\?)\"");
		java.util.regex.Matcher matcher3 = regex3.matcher(response);
		if (matcher3.find())
			editToken = matcher3.group(1);

		java.util.regex.Pattern regex4 = java.util.regex.Pattern
				.compile("name=\"wpEditToken\" type=\"hidden\" value=\"([0-9a-z]+..)\"");
		java.util.regex.Matcher matcher4 = regex4.matcher(response);
		if (matcher4.find())
			editToken = matcher4.group(1);

		log.info("wpEditToken: " + editToken);		

		PostMethod post = new PostMethod(URL);
		post.getParams().setParameter(HttpMethodParams.RETRY_HANDLER,
				new DefaultHttpMethodRetryHandler(3, false));
		post.getParams().setIntParameter(HttpMethodParams.SO_TIMEOUT, 10000);

		int status = 1;

		

		try {
			NameValuePair[] data = { new NameValuePair("wpEdittime", editTime),
					new NameValuePair("wpEditToken", editToken),
					new NameValuePair("wpReason", reason),
					new NameValuePair("wpDeleteReasonList", "other"),
					new NameValuePair("wpConfirmB", "Delete page") };

			post.setRequestBody(data);

			status = client.executeMethod(post);
		} catch (HttpException e) {
			// uhoh... the HttpClient object is probably in a bad state.
			e.printStackTrace();
			disposeClient();
		} catch (IOException e) {
			// who cares
			e.printStackTrace();
			disposeClient();
		} finally {
			// debug!
			try {
				response = post.getResponseBodyAsString();
				log.debug(response);
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			post.releaseConnection();
		}

		return (status == 200 || status == 302);
	}

	public boolean uploadFile(String filename, String description)
			throws IOException {
		return uploadFile(new File(filename), description);
	}

	public boolean uploadFile(File f, String description) throws IOException {
		renewClient();

		String URL = baseURL + "?title=Special:Upload";

		PostMethod upload = new PostMethod(URL);
		Part[] parts = { new StringPart("wpUploadDescription", description),
				new StringPart("wpUploadAffirm", "1"),
				new StringPart("wpIgnoreWarning", "1"),
				new StringPart("wpUpload", "Upload file"),
				new FilePart("wpUploadFile", f.getName(), f) };
		upload.setRequestEntity(new MultipartRequestEntity(parts, upload
				.getParams()));

		int status = 1;
		try {
			status = client.executeMethod(upload);
		} catch (HttpException e) {
			e.printStackTrace();
			disposeClient();
		} catch (IOException e) {
			e.printStackTrace();
			disposeClient();
		} finally {
			upload.releaseConnection();
		}

		return (status == 302 || status == 200);
	}

	// deleteFile doesn't work! The mediawiki software says that something is
	// wrong with the login session.
	public boolean deleteFile(String filename, String reason)
			throws HttpException, IOException {
		String URL1 = baseURL + "?title=Image:" + filename + "&action=delete";

		// first, load the edit page, to get an 'edittime' and 'edittoken'
		// String editTime = "";
		String editToken = "";
		GetMethod get = new GetMethod(URL1);
		// enable automatic retrying
		get.getParams().setParameter(HttpMethodParams.RETRY_HANDLER,
				new DefaultHttpMethodRetryHandler(3, false));
		client.executeMethod(get);

		// now sift through the response looking for the editime and edittoken
		String response = get.getResponseBodyAsString();
		get.releaseConnection();

		/*
		 * 'wpEdittime' was needed in an earlier, pre 1.4, version of MediaWiki
		 * 
		 * java.util.regex.Pattern regex1 = java.util.regex.Pattern.compile(
		 * "value=\"([0-9]+)\" name=\"wpEdittime\""); java.util.regex.Matcher
		 * matcher1 = regex1.matcher(response); if(matcher1.find()) editTime =
		 * matcher1.group(1);
		 */

		java.util.regex.Pattern regex2 = java.util.regex.Pattern
				.compile("value=\"([0-9a-z]+)\" name=\"wpEditToken\"");
		java.util.regex.Matcher matcher2 = regex2.matcher(response);
		if (matcher2.find())
			editToken = matcher2.group(1);

		String URL2 = baseURL + "?title=Image:" + filename + "&action=delete"
				+ "&image=" + filename;
		PostMethod post = new PostMethod(URL2);

		NameValuePair[] data = {
				new NameValuePair("wpConfirm", "1"),
				// new NameValuePair("wpEdittime", editTime),
				new NameValuePair("wpReason", reason),
				new NameValuePair("wpConfirmB", "Confirm"),
				new NameValuePair("wpEditToken", editToken) };

		post.setRequestBody(data);

		int status = 1;
		try {
			status = client.executeMethod(post);
		} catch (HttpException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		} finally {
			post.releaseConnection();
		}

		return (status == 302);
	}

	/*
	 * public static void main(String[] args) { String baseURL =
	 * "http://omath.org/w/index.php";
	 * 
	 * MediawikiConnection conn = new MediawikiConnection(baseURL); Set
	 * pageNames = new HashSet(); pageNames.add("Main Page");
	 * pageNames.add("Roadmap"); Map pageTexts = conn.getPageTexts(pageNames);
	 * Iterator i = pageTexts.keySet().iterator(); String title;
	 * while(i.hasNext()) { title = (String)(i.next());
	 * System.out.println(title); System.out.println(pageTexts.get(title)); } }
	 */

}

/*
 * </pre> [[Category:Source Code]]
 */

