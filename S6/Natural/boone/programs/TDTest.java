/*
 * Main.java
 *
 * Copyright (C) August Mayer, 2001-2004. All rights reserved.
 * Please consult the Boone LICENSE file for additional rights granted to you.
 *
 * Created on 25. November 2002, 14:47
 */

package samples.programs;

import boone.NetFactory;
import boone.NeuralNet;
import boone.map.Function;
import boone.training.BackpropTrainer;
import boone.util.Common;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * A test program.
 *
 * @author Helmut A. Mayer
 * @version $Id: XORTest.java 2292 2016-01-15 16:52:18Z helmut $
 */
public class TDTest {

	/**
	 *	Creates a network, puts random values into it and prints the output.
	 *
	 * @param args	command line arguments
	 */
	public static void main(String[] args) {

		List<Double> pattern = new ArrayList<>();
		NeuralNet net = NetFactory.createFeedForward(new int[]{4, 4, 1}, false, new Function.Identity(), new BackpropTrainer(), null, null);

		for (int i = 0; i < 4; i++)
			pattern.add(0.1 * i + 0.09);

		for (int i = 0; i < 10; i++) {
			net.randomize();
			System.out.println("Input: " + pattern);
			net.setInput(pattern);
			net.innervate();
			System.out.println("Output: " + net.getOutputNeuron(0).getOutput());	// preferred getOutput() for one neuron
//			System.out.println("Output: " + Arrays.toString(net.getOutput(null)));
		}
	}

}
