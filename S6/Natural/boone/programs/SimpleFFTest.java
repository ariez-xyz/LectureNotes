/*
 * Test.java
 *
 * Copyright (C) August Mayer, 2001-2004. All rights reserved.
 * Please consult the Boone LICENSE file for additional rights granted to you.
 *
 * Created on 11. November 2002, 11:35
 */

package samples.programs;

import boone.Link;
import boone.NetFactory;
import boone.NeuralNet;
import boone.Neuron;

/**
 * Tests creating and running a simple feed-forward network.
 *
 * @author August Mayer
 * @version $Id: SimpleFFTest.java 2292 2016-01-15 16:52:18Z helmut $
 */
public class SimpleFFTest {

	/** programs creating and running a simple Feed-Forward network. */
	public static void main(String[] args) {

		System.out.println("Creating feed forward network...");
		NeuralNet net = NetFactory.createFeedForward(
				new int[]{10, 4, 2},     // 10 input neurons, 4 hidden and 2 output neurons.
				false,          // not fully connected
				null,           // no special activation function, use the default sigmoid
				null,           // no special trainer, use the default rprop trainer.
				new Neuron(),   // to demonstrate neuron templates, may also just use null.
				new Link());   // to demonstrate link templates, can also just stay null.

		System.out.println("Printing the network:");
		System.out.println(net.toString());

		System.out.println("Running the network...");
		net.setInput(new double[]{.1, .15, .2, .25, .3, .35, .4, .45, .5, .55});
		net.innervate();

		double[] out = net.getOutput(null);
		System.out.println("Network output: " + out[0] + ", " + out[1]);

		System.out.println("Removing some neurons...");
		Neuron n = net.getInputNeuron(0);
		net.removeNeuron(n);

		System.out.println("Printing the network again:");
		System.out.println(net.toString());

		System.out.println("Done.");
	}
}
