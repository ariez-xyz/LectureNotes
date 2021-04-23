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
import boone.io.IOFilter;
import boone.io.Proben1PatternFilter;

import java.io.File;
import java.io.IOException;

/**
 * Test of loading and saving Proben1 pattern files.
 *
 * @author Helmut A. Mayer
 * @version $Id: Proben1PatternIOTest.java 2296 2018-04-16 12:29:15Z helmut $
 */
public class Proben1PatternIOTest {

	/** Loads 'heart1.dt' and saves it to 'check.dt'. */
	public static void main(String[] args) {

		System.out.println("Loading Proben1 file 'samples/data/heart1.dt'...");

		PatternSet proben1;
		IOFilter filter = new Proben1PatternFilter();
		File file = new File("samples/data/heart1.dt");

		try {
			proben1 = PatternSet.load(file, filter);
		} catch (IOException e) {
			e.printStackTrace();
			return;
		}

		System.out.println("Saving pattern set to Proben1 file 'samples/data/check.dt'...");

		file = new File("samples/data/check.dt");

		try {
			proben1.save(file);
		} catch (IOException e) {
			e.printStackTrace();
		}
		System.out.println("Done.");
	}

}
