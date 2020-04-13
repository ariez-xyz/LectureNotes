/*
 * NoisyLink.java
 *
 * Copyright (C) August Mayer, 2001-2004. All rights reserved.
 * Please consult the Boone LICENSE file for additional rights granted to you.
 */

package samples.programs;

import boone.*;
import boone.util.Common;
import boone.util.Conversion;

/**
 * A link distorting the transmitted signal with some noise. The network remains well-defined, as the weights are not changed.
 * This should be primarily an example for extending a Link
 *
 * @author Helmut A. Mayer
 * @author August Mayer
 * @version 0.1
 * @since April 5, 2004
 *
 * CVS Tag
 * $Id: NoisyLinkTest.java 2292 2016-01-15 16:52:18Z helmut $
 */

public class NoisyLinkTest {

	public static class NoisyLink extends Link {

		/** Constructs the NoisyLink with default values. */
		public NoisyLink() {

			randomize(-0.1, 0.1);            // random weight
		}


		/**
		 * Transmits the signal being distorted according to the specific method noise is afflicted.
		 *
		 * @param n a neuron
		 */
		public void propagate(Neuron n) {

			double signal = getSource().getOutput() * weight;
			getSink().addLinkInput(signal + weight * 0.1 * Common.getRandom().nextGaussian());
		}

	}


	/** * Tester uses basically the code from XORTest. */
	public static void main(String[] args) {

		NeuralNet net = NetFactory.createFeedForward(new int[]{2, 2, 1}, false, null, null, null, new NoisyLink());

		// trains the network
		double[][] inPatterns = new double[][]{{0, 0}, {0, 1}, {1, 0}, {1, 1}};
		double[][] outPatterns = new double[][]{{0}, {1}, {1}, {0}};
		PatternSet patternSet = new PatternSet();
		for (int i = 0; i < inPatterns.length; i++) {
			patternSet.getInputs().add(Conversion.asList(inPatterns[i]));
			patternSet.getTargets().add(Conversion.asList(outPatterns[i]));
		}

		int stepCount = 50;
		Trainer trainer = net.getTrainer();
		trainer.setTrainingData(patternSet);
		trainer.setTestData(patternSet);
		trainer.setEpochs(10);
		trainer.setStepMode(true);
		System.out.println("\n*** Training " + stepCount * trainer.getEpochs() + " epochs...");
		System.out.println("Error: ");
		for (int i = 0; i < stepCount; i++) {
			trainer.train();
			System.out.println(i * trainer.getEpochs() + ". - " + trainer.test());
		}

		System.out.println("\n*** Testing the network...\n");
		for (int i = 0; i < patternSet.size(); i++) {
			System.out.println("Error " + i + " = "
					+ net.getTrainer().test(patternSet.getInputs().get(i), patternSet.getTargets().get(i)));
		}

		System.out.println("\n*** Finished.");
	}

}