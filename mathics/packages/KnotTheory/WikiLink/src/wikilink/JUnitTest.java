package wikilink;


import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.jdom.JDOMException;
import org.junit.Before;
import org.junit.Test;

public class JUnitTest {

  @Before
  public void setUp() throws Exception {
  }
  @Test
  public void testtqft() throws IOException, JDOMException {
    MediawikiConnection connection = new MediawikiConnection("http://tqft.net/w/index.php", "ScottTestRobot", "foobar");
    String date = new Date().toString();
    assertTrue(connection.setPageText("Sandbox/wikilink1", "First edit, at " + date));
    assertTrue(connection.setPageText("Sandbox/wikilink2", "Second edit, also at " + date));
    assertEquals("First edit, at " + date, connection.getPageText("Sandbox/wikilink1"));
    Map<String, String> pages = connection.getPageTexts("Sandbox/wikilink1", "Sandbox/wikilink2");
    assertEquals(2, pages.size());
    assertEquals("First edit, at " + date, pages.get("Sandbox/wikilink1"));
    assertEquals("Second edit, also at " + date, pages.get("Sandbox/wikilink2"));
    
    date = new Date().toString();
    Map<String, String> texts = new HashMap<String, String>();
    texts.put("Sandbox/wikilink3", "Third edit, at " + date);
    texts.put("Sandbox/wikilink4", "Fourth edit, also at " + date);
    assertEquals(0, connection.setPageTexts(texts).size());
    assertEquals("Third edit, at " + date, connection.getPageText("Sandbox/wikilink3"));
    pages = connection.getPageTexts("Sandbox/wikilink3", "Sandbox/wikilink4");
    assertEquals(2, pages.size());
    assertEquals("Third edit, at " + date, pages.get("Sandbox/wikilink3"));
    assertEquals("Fourth edit, also at " + date, pages.get("Sandbox/wikilink4"));
  }

  @Test
  public void testKAtlas() throws IOException, JDOMException {
    MediawikiConnection connection = new MediawikiConnection("http://katlas.math.toronto.edu/w/index.php", "ScottTestRobot", "foobar");
    String date = new Date().toString();
    assertTrue(connection.setPageText("Sandbox/wikilink1", "First edit, at " + date));
    assertTrue(connection.setPageText("Sandbox/wikilink2", "Second edit, also at " + date));
    assertEquals("First edit, at " + date, connection.getPageText("Sandbox/wikilink1"));
    Map<String, String> pages = connection.getPageTexts("Sandbox/wikilink1", "Sandbox/wikilink2");
    assertEquals(2, pages.size());
    assertEquals("First edit, at " + date, pages.get("Sandbox/wikilink1"));
    assertEquals("Second edit, also at " + date, pages.get("Sandbox/wikilink2"));
    
    date = new Date().toString();
    Map<String, String> texts = new HashMap<String, String>();
    texts.put("Sandbox/wikilink3", "Third edit, at " + date);
    texts.put("Sandbox/wikilink4", "Fourth edit, also at " + date);
    assertEquals(0, connection.setPageTexts(texts).size());
    assertEquals("Third edit, at " + date, connection.getPageText("Sandbox/wikilink3"));
    pages = connection.getPageTexts("Sandbox/wikilink3", "Sandbox/wikilink4");
    assertEquals(2, pages.size());
    assertEquals("Third edit, at " + date, pages.get("Sandbox/wikilink3"));
    assertEquals("Fourth edit, also at " + date, pages.get("Sandbox/wikilink4"));
  }
  
}
