package org.katlas.JavaKh;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.text.DecimalFormat;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.PosixParser;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.katlas.JavaKh.algebra.Ring;
import org.katlas.JavaKh.algebra.rings.ModP;
import org.katlas.JavaKh.algebra.rings.Rings;

public class JavaKh {
	private static final Log log = LogFactory.getLog(JavaKh.class);
	
    public static boolean using_h = false;
    
    public static boolean inMemory = true;
    
    @SuppressWarnings("unchecked")
	public static void main(String args[]) throws java.io.IOException {
    	
    	boolean reorderCrossings = true;
    	boolean caching = false;
    	
    /* Process command-line arguments */
    	
		try {
			CommandLineParser parser = new PosixParser();

			Options options = new Options();
			options.addOption("h", "help", false, "show this help screen");
			options.addOption("i", "info", false,
					"turn on lower level debugging statements [INFO].");
			options.addOption("d", "debug", false,
					"turn on lowest level debugging statements [DEBUG].");
			options.addOption("U", "universal", false, "use the universal theory over the integers");
			options.addOption("Z", "integer", false, "work over the integers");
			options.addOption("Q", "rational", false, "work over the rationals");
			options.addOption("m", "mod", true, "work over a field of characteristic p");
			options.addOption("O", "ordered", false, "don't change the ordering of the crossings");
			options.addOption("C", "caching", false, "cache intermediate steps to the cache/ directory");
			options.addOption("D", "disk", false, "store large lists on disk, rather than in memory (slow!)");
			options.addOption("N", "nocobordisms", false, "disable the cobordism cache");
			options.addOption("P", "parallel", false, "simplify complexes using parallel threads (experimental)");
			options.addOption("G", "garbage", false, "perform intense garbage collection");
			
			CommandLine line = parser.parse(options, args);
			// String[] clean_args = line.getArgs();

			Logger rootLogger = Logger.getRootLogger();
			if (line.hasOption("d"))
				rootLogger.setLevel(Level.DEBUG);
			else if (line.hasOption("i"))
				rootLogger.setLevel(Level.INFO);
			else
				rootLogger.setLevel(Level.WARN);

			if(line.hasOption("U")) {
				using_h = true;
			} 
			
			if(line.hasOption("Z")) {
				Rings.setRing("Int");
			} else if(line.hasOption("Q")) {
				Rings.setRing("Rational");
			} else if(line.hasOption("m")) {
				int p = Integer.parseInt(line.getOptionValue("m"));
				if (p == 0)
					Rings.setRing("Rational");
				else {
					ModP.setP(p);
					Rings.setRing("ModP");
				}
			} else {
				// default
				Rings.setRing("Rational");
			}
			
			if(line.hasOption("O")) reorderCrossings = false;
			if(line.hasOption("C")) caching = true;
			if(line.hasOption("D")) {
				inMemory = false;
			}
			if(line.hasOption("N")) CannedCobordismImpl.disableCache();
			if(line.hasOption("P")) Komplex.parallel = true;
			if(line.hasOption("G")) Komplex.intenseGarbage = true;
			
			if (line.hasOption("h")) {
				HelpFormatter formatter = new HelpFormatter();
				formatter.printHelp(
						"Usage: java JavaKh [OPTIONS]", options);
				System.exit(1);
			}

		} catch (Exception e) {
			log.fatal("Error found initializing", e);
			System.exit(1);
		}

		long startTime = System.currentTimeMillis();
		
	//Komplex.checkReidemeister();
	BufferedReader br=new BufferedReader(new InputStreamReader(System.in));
	while (true) {
	    int knot[][] = Komplex.getPD(br);
	    if (knot == null)
		break;
	    Komplex k = Komplex.generateFast(knot, Komplex.getSigns(knot), reorderCrossings, caching, inMemory);
	    assert k.check(true);
	    
	    log.info("Elapsed time: " + new DecimalFormat("###,###,###,###").format(System.currentTimeMillis() - startTime));
	    startTime = System.currentTimeMillis();
	    
	    System.out.println("\"" + k.Kh() + "\"");
	    //k.debugPrint();
	}
	br.close();
    }

}
