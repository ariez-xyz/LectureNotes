// specific imports
import bptree.BPNode;
import bptree.BPTree;

// java imports
import java.util.List;
import java.util.ArrayList;
import java.util.Scanner;
import java.io.File;

/**
 * <p>
 * <strong>IMPORTANT:</strong> Do not modify this class. When you submit your
 * code, all changes to this file are discarded automatically. Hence, it may
 * happen that you program is not working on the submission system.
 * </p>
 * <p>
 * Provides a main function that reads an input file containing the following
 * format:
 * </p>
 * <code><pre>
 *    node-degree-as-integer
 *    key1-as-integer
 *    key2-as-intege
 *    ...
 *    keyN-as-integer
 * </pre></code>
 * <p>
 * The read keys are then append to a list of keys as they are read. This list
 * of keys is then used to call the loadBulk function of the B+ tree.
 * </p>
 */
public class Main {
  public static void main(String[] args) {
    // deal with incorrect usage
    if (args.length < 1) {
      System.err.println("USAGE: java Main <inputfile>");
      System.exit(1);
    }

    // get filename and open scanner for specified file
    final String fileName = args[0];
    final Scanner scanner = openInputFile(fileName);

    BPTree<Integer> bptree = null;

    try {
      // extract m aka the node degree of the B+ tree
      // we read from the scanner until we have found an integer that can be
      // used as node degree, and break afterwards
      while (scanner.hasNext()) {
        try {
          final int nodeDegree = Integer.valueOf(scanner.nextLine()); // m

          bptree = new BPTree<Integer>(nodeDegree);

          break;
        } catch(NumberFormatException nfe) {
          continue;
        }
      }

      // the remaining input lines are treated as keys

      // gather all keys of inputfile
      List<Integer> keys = new ArrayList<Integer>();

      while (scanner.hasNext()) {
        int key = 0;

        try {
          // get next line from scanner and convert it to integer
          key = Integer.valueOf(scanner.nextLine());
        } catch(NumberFormatException nfe) {
          // if the read line cannot be converted to integer, we simply continue
          // with the next line
          continue;
        }

        // append key to list
        keys.add(key);
      }

      // load keys into B+ tree
      List<BPNode<Integer>> nodeList = bptree.loadBulk(keys);

      for (final BPNode<Integer> node: nodeList) {
        System.out.println(node);
      }

      if (bptree.root() != null) {
        System.out.println("r" + bptree.root().id());
      }
    } catch(Exception e) {
      e.printStackTrace();
      terminateOnError("Caught exception \"" + e.toString() + "\"");
    }
  }

  /**
   * Opens and returns a scanner for a specified filename.
   *
   * @param fileName the name of the file to be opened
   *
   * @return the scanner instance the specified filename is associated with
   */
  private static Scanner openInputFile(String fileName) {
		if (fileName != null)
			try {
				return new Scanner(new File(fileName));
			} catch (Exception e) {
        terminateOnError("Could not open \"" + fileName + "\" for reading");
			}

		return new Scanner(System.in);
	}

  /**
   * Prints a specified message as error message (i.e., with "[ERROR] " prefix)
   * to stderr and terminates the program with exit status != 0.
   *
   * @param errorMessage the error message to be printed
   */
  private static void terminateOnError(String errorMessage) {
    System.err.println("[ERROR] " + errorMessage);
    System.exit(1);
  }
}
