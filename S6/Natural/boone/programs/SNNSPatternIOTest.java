/*
 * SNNSPatternIOTest.java
 *
 * Copyright (C) August Mayer, 2001-2004. All rights reserved.
 * Please consult the Boone LICENSE file for additional rights granted to you.
 *
 * Created on 27. August 2003, 17:05
 */

package samples.programs;

import boone.PatternSet;
import boone.io.SNNSPatternFilter;

import java.io.File;

/**
 * A test reading and writing SNNS pattern files. Note that only a simple SNNS format is supported (without dimensions).
 *
 * @author August Mayer
 * @version $Id: SNNSPatternIOTest.java 2296 2018-04-16 12:29:15Z helmut $
 * TODO. Test with variable pattern sets.
 */
public class SNNSPatternIOTest {

	protected static PatternSet[] dataSets = new PatternSet[5];

	/**
	 * programs reading SNNS pattern files.
	 * method: read a number of files, print some statistics about the files
	 */
	public static void testRead() {

		testReadFile("samples/data/snns_test1.pat", 0);
		testReadFile("samples/data/snns_test2.pat", 1);
		testReadFile("samples/data/snns_test3.pat", 2);
		testReadFile("samples/data/snns_test4.pat", 3);
		testReadFile("samples/data/snns_test5.pat", 4);
	}


	/** programs reading, for a single file */
	public static void testReadFile(String fileName, int toIndex) {

		System.out.println("\nRead patterns from file " + fileName + " -");

		File file = new File(fileName);
		PatternSet set;
		System.out.println("Reading file ...");

		try {
			set = PatternSet.load(file, new SNNSPatternFilter());
		} catch (Exception e) {
			e.printStackTrace(System.out);
			return;
		}
		System.out.println("Pattern parameters:");
		printPatternSetInfo(set);

		// store the pattern set
		dataSets[toIndex] = set;
	}


	/** print some info about the pattern set
	 *
	 * @param set	a pattern set
	 */
	public static void printPatternSetInfo(PatternSet set) {

		System.out.println("  Number of input patterns: " + set.getInputs().size());
		System.out.println("  Size of input pattern: " + set.getInputPatternSize());
		System.out.println("  Number of output patterns: " + set.getTargets().size());
		System.out.println("  Size of output pattern: " + set.getTargetPatternSize());
	}


	public static void testWrite() {

		testWriteFile("samples/data/snns_test1_written.pat", dataSets[0]);
		testWriteFile("samples/data/snns_test2_written.pat", dataSets[1]);
		testWriteFile("samples/data/snns_test3_written.pat", dataSets[2]);
		testWriteFile("samples/data/snns_test4_written.pat", dataSets[3]);
		testWriteFile("samples/data/snns_test5_written.pat", dataSets[4]);
	}


	public static void testWriteFile(String fileName, PatternSet set) {

		System.out.println("Write samples to file " + fileName + " -");

		File file = new File(fileName);
		set.setFilter(new SNNSPatternFilter());

		System.out.println("Writing file ...");
		try {
			set.save(file);
		} catch (Exception e) {
			e.printStackTrace(System.out);
			return;
		}
		System.out.println("  Done.\n");
	}


	public static void main(String[] args) {

		System.out.println("Test program for: SNNSPatternFilter");
		System.out.println("Function........: read and write SNNS pattern files");
		System.out.println("Working directory: " + new File(".").getAbsolutePath() + "\n");

		System.out.println("--------------------------------------------------------------------");
		System.out.println("Test block: Reading SNNS pattern files");
		testRead();

		System.out.println("\n------------------------------------------------------------------");
		System.out.println("Test block: Writing SNNS pattern files");
		testWrite();
	}
}
