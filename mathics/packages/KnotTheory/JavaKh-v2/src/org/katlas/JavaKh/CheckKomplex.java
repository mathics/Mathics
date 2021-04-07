package org.katlas.JavaKh;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.ObjectInputStream;

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

public class CheckKomplex<R extends Ring<R>> {
  private static final Log log = LogFactory.getLog(CheckKomplex.class);

  @SuppressWarnings("unchecked")
  public static void main(String[] args) throws FileNotFoundException, IOException, ClassNotFoundException {

    File komplexFile = null;

    /* Process command-line arguments */

    try {
      CommandLineParser parser = new PosixParser();

      Options options = new Options();
      options.addOption("h", "help", false, "show this help screen");
      options.addOption("i", "info", false, "turn on lower level debugging statements [INFO].");
      options.addOption("d", "debug", false, "turn on lowest level debugging statements [DEBUG].");
      options.addOption("U", "universal", false, "use the universal theory over the integers");
      options.addOption("Z", "integer", false, "work over the integers");
      options.addOption("Q", "rational", false, "work over the rationals");
      options.addOption("m", "mod", true, "work over a field of characteristic p");

      CommandLine line = parser.parse(options, args);
      String[] clean_args = line.getArgs();

      Logger rootLogger = Logger.getRootLogger();
      if (line.hasOption("d"))
        rootLogger.setLevel(Level.DEBUG);
      else if (line.hasOption("i"))
        rootLogger.setLevel(Level.INFO);
      else
        rootLogger.setLevel(Level.WARN);

      if (line.hasOption("U")) {
        JavaKh.using_h = true;
      }

      if (line.hasOption("Z")) {
        Rings.setRing("Int");
      } else if (line.hasOption("Q")) {
        Rings.setRing("Rational");
      } else if (line.hasOption("m")) {
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

      if (clean_args.length != 1 || line.hasOption("h")) {
        HelpFormatter formatter = new HelpFormatter();
        formatter.printHelp("Usage: java CheckComplex [OPTIONS] <filename>", options);
        System.exit(1);
      }

      komplexFile = new File(clean_args[0]);

    } catch (Exception e) {
      log.fatal("Error found initializing", e);
      System.exit(1);
    }

    CheckKomplex check = new CheckKomplex();

    if (check.checkComplex(komplexFile)) {
      System.out.println("Komplex looks good; d^2 = 0.");
      System.exit(0);
    } else {
      System.out.println("Komplex looks broken; d^2 != 0.");
      System.exit(1);
    }

  }

  private boolean checkComplex(File file) throws FileNotFoundException, IOException, ClassNotFoundException {
    ObjectInputStream ois = new ObjectInputStream(new FileInputStream(file));
    return checkComplex(ois);
  }

  @SuppressWarnings("unchecked")
  private boolean checkComplex(ObjectInputStream s) throws IOException, ClassNotFoundException {
    Komplex.CHECK_DURING_DESERIALIZATION = true;
    try {
      @SuppressWarnings("unused")
      Komplex<R> result = (Komplex<R>) s.readObject();
    } catch (AssertionError e) {
      return false;
    }
    return true;
  }
}
