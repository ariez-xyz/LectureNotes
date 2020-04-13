/*
 * PatternIOTest.java
 *
 * Copyright (C) August Mayer, 2001-2004. All rights reserved.
 * Please consult the Boone LICENSE file for additional rights granted to you.
 *
 * Created on 09. Dezember 2002, 19:00
 */

package samples.programs;

import boone.PatternSet;
import boone.io.BooneFilter;
import boone.util.Conversion;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Map;

/**
 * A test saving/loading a pattern set.
 *
 * @author August Mayer
 * @version $Id: PatternIOTest.java 2296 2018-04-16 12:29:15Z helmut $
 */
public class PatternIOTest {

	static String writeName = "samples/data/write.xpat";

	static String writeName2 = "samples/data/write2.xpat";


	public static void main(String[] args) {

		PatternSet outData = new PatternSet();
		outData.getInputs().add(Conversion.asList(new double[]{0, 1, 2}));
		outData.getTargets().add(null);
		outData.getInputs().add(Conversion.asList(new double[]{3, 4}));
		outData.getTargets().add(Conversion.asList(new double[]{5, 6, 7}));
		outData.getInputs().add(Conversion.asList(new double[]{8, 9, 10, 11}));
		outData.getTargets().add(new ArrayList<Double>());
		outData.getProperties().put("testProp", "something");
		outData.getProperties().put("emptyProp", "");

		System.out.println("Writing " + writeName + " ...");

		try {
			outData.save(new File(writeName));
		} catch (IOException e) {
			e.printStackTrace();
		}

		System.out.println("Reading " + writeName + " ...");
		PatternSet data;
		try {
			data = PatternSet.load(new File(writeName), new BooneFilter("set", true));
		} catch (IOException e) {
			e.printStackTrace();
			return;
		}

		for (Map.Entry<Object, Object> entry : data.getProperties().entrySet())
			System.out.println("Property \"" + entry.getKey() + "\" is \"" + entry.getValue() + "\".");

		System.out.println("Writing again, to " + writeName2 + " ...");

		try {
			data.save(new File(writeName2));
		} catch (IOException e) {
			e.printStackTrace();
		}

		System.out.println("Done.");
	}

}
