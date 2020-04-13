/*
 * SNNSNetIOTest.java
 *
 * Created on June 16, 2005, 14:40
 */

package samples.programs;

import boone.NetFactory;
import boone.NeuralNet;
import boone.io.IOFilter;
import boone.io.SNNSNetFilter;

import java.io.File;
import java.io.IOException;

/**
 * Test loading and saving SNNS networks.
 *
 * @author August Mayer
 * @author Helmut A. Mayer
 * @version $Id: SNNSNetIOTest.java 2292 2016-01-15 16:52:18Z helmut $
 * @since June 16, 2005
 */
public class SNNSNetIOTest {

	public static void main(String[] argv) {

		System.out.println("*** Testing FeedForward network creation & storing");

		System.out.println("Creating network...");
		NeuralNet net = NetFactory.createFeedForward(new int[]{10, 4, 2}, false, null, null, null, null);

		System.out.println("Done. Saving the network to 'samples/data/snns.net'...");

		IOFilter filter = new SNNSNetFilter();
		net.setFilter(filter);
		try {
			net.save(new File("samples/data/snns.net"));
		} catch (IOException e) {
			e.printStackTrace();
			return;
		}

		System.out.println("Done. Loading the network from file 'samples/data/snns.net'...");

		try {
			net = NeuralNet.load(new File("samples/data/snns.net"), filter);
		} catch (IOException e) {
			e.printStackTrace();
			return;
		}

		System.out.println("Done. Printing the net:\n" + net);

		System.out.println("Done. Saving the net again to 'samples/data/snns2.net'...");

		try {
			net.save(new File("samples/data/snns2.net"));
		} catch (IOException e) {
			e.printStackTrace();
		}

		System.out.println("All Done. Please check if 'snns.net' and 'snns2.net' differ.");
	}

}
