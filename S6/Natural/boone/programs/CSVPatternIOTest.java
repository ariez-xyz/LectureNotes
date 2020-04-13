/*
 * Main.java
 *
 * Copyright (C) August Mayer, 2001-2004. All rights reserved.
 * Please consult the Boone LICENSE file for additional rights granted to you.
 *
 * Created on 25. November 2002, 14:47
 */

package samples.programs;

import boone.PatternSet;
import boone.io.CSVPatternFilter;
import boone.io.IOFilter;
import boone.util.Conversion;

import java.io.File;
import java.io.IOException;

/**
 * Test saving/loading CSV patterns.
 *
 * @author Helmut A. Mayer
 * @version $Id: CSVPatternIOTest.java 2294 2016-04-18 11:38:16Z helmut $
 */
public class CSVPatternIOTest {

	public static void main(String[] args) {

		System.out.println("Creating XOR patterns...");

		double[][] inPatterns = new double[][]{{0, 0}, {0, 1}, {1, 0}, {1, 1}};
		double[][] outPatterns = new double[][]{{0}, {1}, {1}, {0}};
		PatternSet xor = new PatternSet();
		for (int i = 0; i < inPatterns.length; i++) {
			xor.getInputs().add(Conversion.asList(inPatterns[i]));
			xor.getTargets().add(Conversion.asList(outPatterns[i]));
		}
		IOFilter filter = new CSVPatternFilter(2, 1);
		xor.setFilter(filter);
		File file = new File("samples/data/xor.csv");

		System.out.println("Saving pattern set to 'samples/data/xor.csv'...");

		try {
			xor.save(file);
		} catch (IOException e) {
			e.printStackTrace();
		}

		System.out.println("Loading pattern set from 'samples/data/xor.csv'...");

		try {
			xor = PatternSet.load(file, filter);
		} catch (IOException e) {
			e.printStackTrace();
			return;
		}
		System.out.println("Loaded set:");
		System.out.println(xor);
		System.out.println("Done.");
	}

}
