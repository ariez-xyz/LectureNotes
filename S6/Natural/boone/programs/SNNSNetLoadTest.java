/*
 * SNNSNetIOTest.java
 *
 * Created on 16. Juni 2005, 14:40
 */

package samples.programs;

import boone.NeuralNet;
import boone.io.SNNSNetFilter;

import java.io.File;
import java.io.IOException;

/**
 * Test loading an SNNS network.
 *
 * @author August Mayer
 * @version $Id: SNNSNetLoadTest.java 2292 2016-01-15 16:52:18Z helmut $
 * @since June 16, 2005, 14:40
 */
public class SNNSNetLoadTest {

	public static void main(String[] args) {

		String snnsFileName = "samples/data/letters.net";
		if (args.length > 1)
			snnsFileName = args[1];

		System.out.println("Loading the file " + snnsFileName + " ...");

		NeuralNet net;

		try {
			net = NeuralNet.load(new File(snnsFileName), new SNNSNetFilter());
		} catch (IOException e) {
			e.printStackTrace();
			return;
		}

		System.out.println("Properties: " + net.props);   		// list the properties

		System.out.println("Done. Printing the net:\n" + net);
	}

}
