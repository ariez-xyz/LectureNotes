/*
 * NetIOTest.java
 *
 * Copyright (C) August Mayer, 2001-2004. All rights reserved.
 * Please consult the Boone LICENSE file for additional rights granted to you.
 *
 * Created on 09. Januar 2003, 15:56
 */

package samples.programs;

import boone.NetFactory;
import boone.NeuralNet;
import boone.io.BooneFilter;
import boone.spike.SpikingNeuralNet;

import java.io.File;
import java.io.IOException;

/**
 * A test loading and saving networks.
 *
 * @author August Mayer
 * @version $Id: NetIOTest.java 2292 2016-01-15 16:52:18Z helmut $
 */
public class SpikeIOTest {

	public static void main(String[] argv) {

		NeuralNet net;

		try {
			net = SpikingNeuralNet.load(new File("samples/data/spike.xnet"), new BooneFilter("net", false));
		} catch (IOException e) {
			e.printStackTrace();
			return;
		}

		System.out.println("Properties: " + net.props);   						// list the properties
		System.out.println("Done. Printing the net:\n" + net);

		net.getFilter().setCompressed(true);
		try {
			net.save(new File("samples/data/spike1.xnet"));
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

}
