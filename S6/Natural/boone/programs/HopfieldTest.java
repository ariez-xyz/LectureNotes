/*
 * HopfieldTest.java
 *
 * Copyright (C) August Mayer, 2001-2004. All rights reserved.
 * Please consult the Boone LICENSE file for additional rights granted to you.
 *
 * Created on March 20, 2003, 10:22
 */

package samples.programs;

import boone.NetFactory;
import boone.NeuralNet;
import boone.PatternSet;
import boone.Trainer;
import boone.util.Conversion;

/**
 * A small demonstration of the Hopfield network package. It trains the training patterns to be fixed points of the net.
 *
 * @author August Mayer
 * @author Helmut A. Mayer
 * @version $Id: HopfieldTest.java 2294 2016-04-18 11:38:16Z helmut $
 */
public class HopfieldTest {

	/** The training patterns. */
	public double[][] trainingPatterns = {
					{1, 0, 0, 0, 1, 0, 0, 0, 1},
					{1, 1, 1, 1, 0, 1, 1, 1, 1},
					{0, 0, 0, 0, 0, 0, 1, 1, 1}
	};

	/** Two erroneous test patterns. */
	public double[] testPattern1 = {0, 1, 0, 0, 1, 1, 1, 1, 1};
	public double[] testPattern2 = {1, 1, 1, 0, 0, 1, 1, 1, 1};


	/** Applies a pattern to the net and displays the input pattern and the resulting output pattern.
	 *
	 * @param net			a Hopfield net
	 * @param pattern		a input pattern
	 * @param name          the name of the pattern
	 */
	public void runPattern(NeuralNet net, double[] pattern, String name) {

		println("Testing " + name);
		net.setInput(pattern);
		net.innervate();
		double[] result = net.getOutput(null);

		print("    Input:  ");
		for (int i = 0; i < pattern.length; i++)
			print((i > 0 ? ", " : "") + pattern[i]);

		print("\n    Output: ");
		for (int i = 0; i < result.length; i++)
			print((i > 0 ? ", " : "") + result[i]);

		println("");
	}


	/** Trains the network. Training is stopped, if the training (=test) set applied to the net does not
	 * generate an error in three consecutive epochs. Then the detailed results for the training patterns and the
	 * two erroneous patterns are given.
	 */
	public void testHopfield() {

		println("Creating network.");
		NeuralNet net = NetFactory.createHopfield(9, null, null, null, null);

		println("Training.");
		PatternSet trainingSet = new PatternSet();

		for (double[] trainingPattern : trainingPatterns) {							// build training set
			trainingSet.getInputs().add(Conversion.asList(trainingPattern));
			trainingSet.getTargets().add(Conversion.asList(trainingPattern));
		}
		Trainer trainer = net.getTrainer();
		trainer.setTrainingData(trainingSet);
		trainer.setTestData(trainingSet);
		trainer.setLearnRate(0.1);
		trainer.setEpochs(1);
		trainer.setStepMode(true);					// trains in steps of 1 epoch

		int i;
		int maxRepeat = 1000;
		int perfect = 0;

		for (i = 0; i < maxRepeat; i++) {
			System.out.print("Training repetition # " + i);
			trainer.train();
			double error = trainer.test();
			System.out.println(" ... error " + error);

			if (error == 0.0) {
				if (++perfect == 3)
					break;
			} else
				perfect = 0;
		}
		System.out.println("Used " + i + " training repetitions (of " + maxRepeat + " max)");

		for (i = 0; i < trainingPatterns.length; i++)                            	// apply training patterns
			runPattern(net, trainingPatterns[i], "training pattern #" + (i + 1));

		runPattern(net, testPattern1, "erroneous pattern #1");						// apply test patterns
		runPattern(net, testPattern2, "erroneous pattern #2");
	}


	public void println(String s) {

		System.out.println(s);
	}


	public void print(String s) {

		System.out.print(s);
	}


	public static void main(String[] args) {

		HopfieldTest hft = new HopfieldTest();
		hft.testHopfield();
	}

}
