package samples.programs;

import boone.NetFactory;
import boone.NeuralNet;
import boone.PatternSet;
import boone.io.BooneFilter;
import boone.map.HexagonTopology;
import boone.training.LVQTrainer;
import boone.training.SOMTrainer;
import boone.util.UMatrix;

import java.io.File;
import java.io.IOException;
import java.util.logging.Logger;

/**
 * Example for using Boone's SOM extension which demonstrates usage and some reasonable parameters.<br/>
 * The program's patterns are the word frequencies of 120 novels written by 8 different Russian authors.
 *
 * @author ghowa
 * @author Helmut A. Mayer
 *
 */
public class SOMTest {

	private static final Logger log = Logger.getLogger(SOMTest.class.getName());

	/**
	 * Loads patterns from file, creates and trains SOM, creates visualization.
	 *
	 * @param args the command line arguments
	 */
	public static void main(String[] args) {

		log.info("Load patterns and create net.");

		//fromXML patterns from file
		PatternSet patterns;
		try {
			patterns = PatternSet.load(new File("samples/data/russian.xpat"), new BooneFilter("set", true));
		} catch (IOException e) {
			e.printStackTrace();
			log.severe("Couldn't find sample pattern file");
			return;
		}

		//create new hexagonal SOM, initialize weights using PCA of the patterns
		SOMTrainer trainer = new SOMTrainer(new HexagonTopology(12, 9));			// hexagonal SOM with 12x9 neurons
		NeuralNet som = NetFactory.createSOM(trainer, patterns);
//		som.randomize(0.0, 1.0);

		trainer.setTrainingData(patterns);
		trainer.setTestData(patterns);
		trainer.setEpochs(0);
		trainer.train();
//		UMatrix umatrix = new UMatrix(patterns, (HexagonTopology)trainer.getTopology(), 205, new File("init.png"));
//		umatrix.visualize();

		//set training parameters
		trainer.setLearnRate(100.0);
		trainer.setStartRadius(6.0);
		trainer.setEpochs(300);
		log.info("Start SOM training...this may take a while...");
		trainer.train();
		log.info("SOM accuracy: " + (1.0 - trainer.test()));

		// UMatrix after SOM training
//		UMatrix umatrix = new UMatrix(patterns, (HexagonTopology)trainer.getTopology(), 205, new File("som.png"));
//		umatrix.visualize();

		log.info("Start LVQ.");

		trainer = new LVQTrainer(new HexagonTopology(12, 9));			// hexagonal SOM with 12x9 neurons
		trainer.setLearnRate(0.5);
		trainer.setTrainingData(patterns);
		trainer.setTestData(patterns);
		trainer.setEpochs(1000);
		som.setTrainer(trainer);
		trainer.train();

		//visualize net
		UMatrix umatrix = new UMatrix(patterns, (HexagonTopology)trainer.getTopology(), 205, new File("samples/data/result.png"));
		umatrix.visualize();
		log.info("UMatrix wrote to 'samples/data/result.png'.");

		log.info("LVQ accuracy: " + (1.0 - trainer.test()));
	}
}
