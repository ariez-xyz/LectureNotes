/*
 * HopfieldStrucTest.java
 *
 * Copyright (C) August Mayer, 2001-2004. All rights reserved.
 * Please consult the Boone LICENSE file for additional rights granted to you.
 *
 * Created on 04. M�rz 2004, 09:56
 */

package samples.programs;

import boone.Link;
import boone.NetFactory;
import boone.NeuralNet;
import boone.Neuron;

/**
 * Test class for investigating a Hopfield network structure.
 *
 * @author August Mayer
 * @author Helmut A. Mayer
 */
public class HopfieldStructureTest {

	public static void main(String[] args) {

		HopfieldStructureTest hft = new HopfieldStructureTest();
		hft.runTest();
	}


	/** Prints out the network structure.
	 *
	 * @param net		the Hopfield net
	 */
	public void printNet(NeuralNet net) {

		for (int i = 0; i < net.getNeuronCount(); i++) {
			Neuron neuron = net.getNeuron(i);
			System.out.println("Neuron " + neuron.getID());

			for (int j = 0; j < neuron.getOutputLinkCount(); j++) {
				Link link = neuron.getOutputLink(j);
				System.out.println("  " + link.getName()
						+ " to Neuron " + link.getOtherNeuron(neuron).getID()
						+ " value " + link.getWeight());
			}
		}
	}


	/** Runs the test. */
	public void runTest() {

		System.out.println("Creating network.");
		NeuralNet net = NetFactory.createHopfield(4, null, null, null, null);

		if (!net.isFeedForward())
			System.out.println("Recurrent net detected.");
		printNet(net);
	}

}
