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

import java.io.File;
import java.io.IOException;

/**
 * A test loading and saving networks.
 *
 * @author August Mayer
 * @version $Id: NetIOTest.java 2292 2016-01-15 16:52:18Z helmut $
 */
public class NetIOTest {

	public static void main(String[] argv) {

		System.out.println("*** Testing FeedForward network creation & storing");
		System.out.println("Creating network...");

		NeuralNet net = NetFactory.createFeedForward(new int[]{2, 2, 2}, false, null, null, null, null);

		net.props.put("storedBy", "NetIOTest");				// add some properties
		if (net.isFeedForward())
			net.props.put("networkType", "FeedForward");

		System.out.println("Done. Saving the network to test1.xnet...");
		System.out.println("Done. Printing the net:\n" + net);

		try {
			net.save(new File("test1.xnet"));
		} catch (IOException e) {
			e.printStackTrace();
		}

		System.out.println("Done. Loading the network...");

		try {
			net = NeuralNet.load(new File("test1.xnet"), new BooneFilter("net", true));
		} catch (IOException e) {
			e.printStackTrace();
			return;
		}

		System.out.println("Properties: " + net.props);   						// list the properties
		System.out.println("Done. Printing the net:\n" + net);
		System.out.println("Done. Saving the net again to test2.xnet ...");

		try {
			net.save(new File("test2.xnet"));
		} catch (IOException e) {
			e.printStackTrace();
		}
		System.out.println("All Done. Please check if test1.xnet and test2.xnet differ.");
	}

}
