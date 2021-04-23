package samples.programs;

import boone.NetFactory;
import boone.spike.SpikeSet;
import boone.spike.SpikingNeuralNet;
import boone.spike.SpikingNeuron;

import java.util.ArrayList;
import java.util.List;

/**
 * Test/Demo showing the basic usage of a {@code SpikingNeuralNet} and spike-related classes.
 * <p>
 *
 * @author Johannes Mory
 */
public class SpikingNetTest {

	public static void main(String[] args) {

		////////////////////////
		// Test Standard Mode //
		////////////////////////

		System.out.printf("STANDARD MODE TEST%n%n");
		// Create feed-forward network with standard spiking neurons, spiking links, activation functions and randomized link delays
		SpikingNeuralNet net = NetFactory.createFeedForward(
				new int[]{3, 3, 3},        // 3 input, 3 hidden and 3 output neurons
				true,                    // fully connected
				null,                    // spike form template
				null,                    // postsynaptic potential form
				null,                    // trainer
				null,                    // neuron template
				null);                    // link template

		// Create spike set to control network input and record network output
		SpikeSet spikeSet = new SpikeSet();
		// iterate over all input neurons and create for each input neuron a list of
		// random timestamps (unit milliseconds) that tells the input neuron when to emit a new spike
		for (int i = 0; i < net.getInputNeuronCount(); i++) {
			SpikingNeuron inputNeuron = (SpikingNeuron)net.getInputNeuron(i);
			// the list telling the input neuron, handled in the current iteration, the times at which it should emit a spike
			List<Double> input = new ArrayList<>();
			for (int j = 0; j < 10; j++)
				input.add(Math.random() * 200);
//			for (int j = 0; j < 500; j++)
//				input.add(Math.random() * 10000);
			// link the created list of spike timestamps to the input neuron handled in the current iteration
			spikeSet.setInputSpikes(inputNeuron, input);
			System.out.printf("Input neuron with id %d will generate FeedForward spikes at times: %s%n", inputNeuron.getID(), asString(input));
		}
		// iterate over all output neurons and turn for each output neuron the recording of spike timestamps on
		for (int i = 0; i < net.getOutputNeuronCount(); i++) {
			SpikingNeuron outputNeuron = (SpikingNeuron)net.getOutputNeuron(i);
			outputNeuron.recordStart(spikeSet);
		}
		// we have to tell the network that it should read its input from our spike set
		net.setInput(spikeSet);
		// tell the network to start processing the input we just set
		net.innervate();
		// wait until no more new events/spikes are generated, i.e. until the event buffer is empty
		while (net.hasMoreSpikes()) {
			try {
				Thread.sleep(100);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
		// explicitly stop the current simulation run
		net.stop();
		System.out.println();
		// print recorded spike times of all output neurons
		for (int i = 0; i < net.getOutputNeuronCount(); i++) {
			SpikingNeuron outputNeuron = (SpikingNeuron)net.getOutputNeuron(i);
			List<Double> output = spikeSet.getOutputSpikes(outputNeuron);
			System.out.printf("Recorded spikes from output neuron with id %d: %s%n", outputNeuron.getID(), asString(output));
		}

		////////////////////////
		// Test Real-Time Mode //
		////////////////////////

		System.out.printf("%n%nREAL-TIME MODE TEST 2%n%n");
		// Turn on real time mode
		net.setRealTimeMode(true);
		// Clear recently defined network input and recorded network output
		spikeSet.getInputs().clear();
		spikeSet.getOutputs().clear();
		// iterate over all output neurons and turn for each output neuron the recording of spike timestamps on
		for (int i = 0; i < net.getOutputNeuronCount(); i++)
			((SpikingNeuron)net.getOutputNeuron(i)).recordStart(spikeSet);
		// Start new simulation run in real-time mode - implicitly stops the current simulation run
		net.innervate();

		long start = System.currentTimeMillis();
		// Run for 5 seconds
		System.out.printf("%nFeeding the network random input data over a time span of 5 Seconds%n");
		while ((System.currentTimeMillis() - start) < 5000) {
			try {
				Thread.sleep((long)(Math.random() * 25));
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
			// select a random input neuron
			SpikingNeuron randomNeuron = (SpikingNeuron)net.getInputNeuron((int)(Math.random() * 3));
			// tell selected input neuron to fire a spike
			randomNeuron.fire(System.currentTimeMillis() - start);
		}
		// wait until no more new events/spikes are generated, i.e. until the event buffer is empty
		while (net.hasMoreSpikes()) {
			try {
				Thread.sleep(100);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
		// Don't forget to stop the network
		net.stop();

		System.out.println();
		// print recorded spike times of all output neurons
		for (int i = 0; i < net.getOutputNeuronCount(); i++) {
			SpikingNeuron outputNeuron = (SpikingNeuron)net.getOutputNeuron(i);
			outputNeuron.recordStop();
			List<Double> output = spikeSet.getOutputSpikes(outputNeuron);
			System.out.printf("Recorded spikes from output neuron with id %d: %s%n", outputNeuron.getID(), asString(output));
		}
	}


	public static String asString(List<Double> list) {

		StringBuilder listString = new StringBuilder();
		listString.append("{");
		for (int i = 0; i < list.size(); i++) {
			listString.append(i == 0 ? "" : ", ");
			listString.append(String.format("%.2f", list.get(i)));
		}
		listString.append("}");
		return listString.toString();
	}

}
