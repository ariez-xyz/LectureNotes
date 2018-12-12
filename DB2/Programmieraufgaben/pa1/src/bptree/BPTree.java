package bptree;

// java imports

import java.util.List;
import java.util.ArrayList;
import java.util.Collections;

/**
 * A generic implementation of a B+ tree that is supposed to support bulk
 * loading. It uses the <tt>BPNode</tt> class to stored the keys and children.
 *
 * @param <Key> the type of the keys stored in this B+ tree. This type has to
 *              implement the <tt>Comparable</tt> interface.
 * @see BPNode
 */
public class BPTree<Key extends Comparable<Key>> {
    /**
     * Constructs an empty B+ tree of a specified node degree, i.e., m.
     *
     * @param degree the node degree of this B+ tree node (i.e., m)
     */
    public BPTree(final int degree) {
        this.degree = degree;
    }

    // public methods

    /**
     * Retrieves the root node of this B+ tree.
     *
     * @return the root node of this B+ tree
     */
    public BPNode<Key> root() {
        return root;
    }

    /**
     * Retrieves the degree of this B+ tree.
     *
     * @return the degree of this B+ tree
     */
    public int degree() {
        return degree;
    }

    /**
     * Construct a B+ tree bottom-up.
     *
     * @param keys a list of keys that are bulk loaded into the B+ tree
     * @return a sorted list of all created nodes (sorted with respect to the node
     * identifier)
     */
    public List<BPNode<Key>> loadBulk(final List<Key> keys) {
        // IMPORTANT: no need to modify this method

        // list of all nodes created in the process
        List<BPNode<Key>> nodeList = new ArrayList<BPNode<Key>>();

        // deal with trivial cases
        if ((keys == null) || keys.isEmpty()) {
            return nodeList;
        }

        // sort keys
        Collections.sort(keys);

        // build the leaf level based on the given keys
        List<BPNode<Key>> leaves = buildLeafLevel(keys);

        // append leaves
        nodeList.addAll(leaves);

        // assign the list of leaf nodes as the current "top level" (since it is the
        // only level at this point in time)
        List<BPNode<Key>> topLevelNodes = leaves;

        // construct additional inner levels until there is only one node in the
        // current "top level" of the B+ tree, i.e., the root node has been built
        // and we can stop constructing additional tree levels
        while (topLevelNodes.size() > 1) {
            // build next higher level based on the currently assigned "top level" of
            // the B+ tree and assign the resulting level as the new "top level"
            // this is done as long as we have more than one (inner) node in the
            // current "top level"
            topLevelNodes = buildInnerLevel(topLevelNodes);

            // append newly constructed level
            nodeList.addAll(topLevelNodes);
        }

        // just to make sure that the "top level" has at least one element
        if (topLevelNodes.size() > 0) {
            // assign the first element of the final "top level" as root node
            // if the B+ tree is built correctly, the first element of topLevelNodes
            // is the -only- node in the list
            root = topLevelNodes.get(0);
        }

        // return sorted list of nodes (sorted with respect to the node identifier)
        return nodeList;
    }

    /**
     * Builds a valid leaf level of this B+ tree with the minimal number of leaf
     * nodes for the specified list of keys. This method returns a sorted list of
     * the leaf nodes created in the process (sorted with respect to the keys in
     * the nodes). The children in a leaf node are supposed to be <tt>null</tt>.
     *
     * @param keys a sorted list of keys that are bulk loaded into the B+ tree
     * @return a sorted list of all leaf nodes created in the process, or
     * <tt>null</tt> if there are no keys to load
     */
    private List<BPNode<Key>> buildLeafLevel(final List<Key> keys) {
        // deal with trivial cases
        if ((keys == null) || keys.isEmpty()) {
            return null;
        }

        List<BPNode<Key>> leaves = new ArrayList<BPNode<Key>>();
        int remainingEls = keys.size();
        int position = 0;

        // create as many "full" nodes as possible
        while (remainingEls > (degree - 1)) {
            BPNode<Key> n = new BPNode<>(degree);
            for (int i = 0; i < degree - 1; i++) {
                n.addKey(keys.get(position++));
                n.addChild(null);
                remainingEls--;
            }
            leaves.add(n);
        }

        // if enough elements are left to fill last node
        if (remainingEls >= Math.ceil((degree - 1) / 2f)) {
            BPNode<Key> n = new BPNode<>(degree);
            while (position < keys.size()) {
                n.addChild(null);
                n.addKey(keys.get(position++));
            }
            leaves.add(n);
        // else transfer elements from second last to last
        } else {
            BPNode<Key> n = new BPNode<>(degree);
            // should throw exception here if list of keys holds less than ceil((deg-1)/2) keys TODO add third case
            BPNode<Key> last = leaves.get(leaves.size() - 1);
            while (remainingEls < Math.ceil((degree - 1) / 2f)){
                n.addChild(0, last.removeLastChild());
                n.addKey(0, last.removeLastKey());
                remainingEls++;
            }
            while (position < keys.size()) {
                n.addChild(null);
                n.addKey(keys.get(position++));
            }
            leaves.add(n);
        }

        // link leaves
        for (int i = 0; i < leaves.size() - 1; i++)
            leaves.get(i).addChild(leaves.get(i + 1));
        leaves.get(leaves.size() - 1).addChild(null);

        return leaves;
    }

    /**
     * Builds a valid inner level of this B+ tree with the minimal number of inner
     * nodes for the specified list of child nodes. This method returns a sorted
     * list of the inner nodes created in the process (sorted with respect to the
     * keys in the nodes). The children in an inner node are supposed to point
     * to the respective child nodes.
     *
     * @param children a sorted list of child nodes for which the next inner level
     *                 is built
     * @return a sorted list of all inner nodes created in the process, or
     * <tt>null</tt> if there are no children to connect
     */
    private List<BPNode<Key>> buildInnerLevel(final List<BPNode<Key>> children) {
        // deal with trivial cases
        if ((children == null) || children.isEmpty()) {
            return null;
        }
        List<BPNode<Key>> currentLevel = new ArrayList<BPNode<Key>>();
        int remainingEls = children.size();
        int position = 0;

        while (remainingEls > degree) {
            BPNode<Key> n = new BPNode<>(degree);
            n.addChild(children.get(position++));
            remainingEls--;
            for (int i = 0; i < degree - 1; i++) {
                BPNode<Key> newChild = children.get(position++);
                n.addKey(smallestKey(newChild));
                n.addChild(newChild);
                remainingEls--;
            }
            currentLevel.add(n);
        }

        // if enough children are left to fill last node
        if (remainingEls >= Math.ceil(degree / 2f)) {
            BPNode<Key> n = new BPNode<>(degree);
            n.addChild(children.get(position++));
            while (position < children.size()) {
                BPNode<Key> newChild = children.get(position++);
                n.addKey(smallestKey(newChild));
                n.addChild(newChild);
            }
            currentLevel.add(n);
        // else if this is not the only node in this level transfer children from second last to last
        } else if (!currentLevel.isEmpty()){
            BPNode<Key> n = new BPNode<>(degree);
            BPNode<Key> last = currentLevel.get(currentLevel.size() - 1);
            n.addChild(0, last.removeLastChild());
            last.removeLastKey();
            remainingEls++;
            while (remainingEls < Math.ceil(degree / 2f)){
                BPNode<Key> newChild = last.removeLastChild();
                last.removeLastKey();
                n.addKey(0, smallestKey(newChild));
                n.addChild(0, newChild);
                remainingEls++;
            }
            while (position < children.size()) {
                BPNode<Key> newChild = children.get(position++);
                n.addChild(newChild);
                n.addKey(smallestKey(newChild));
            }
            currentLevel.add(n);
        // else create root node
        } else {
            BPNode<Key> n = new BPNode<>(degree);
            n.addChild(children.remove(0));
            BPNode<Key> newChild;
            while (!children.isEmpty()) {
                newChild = children.remove(0);
                n.addKey(smallestKey(newChild));
                n.addChild(newChild);
            }
            currentLevel.add(n);
        }

        return currentLevel;
    }

    /**
     * Retrieves the smallest key in the specified branch of this B+ tree, i.e.,
     * the leftmost key of the leftmost branch with respect to the specified node.
     *
     * @param node the node that represents the subroot of the branch for which
     *             the smallest key should be found
     * @return the smallest key of the branch that has <tt>node</tt> as (sub)root
     */
    private Key smallestKey(final BPNode<Key> node) {
        // IMPORTANT: no need to modify this method

        BPNode<Key> currentNode = node;

        // iterate until the leaf level is reached and following the leftmost
        // child on each traversed inner level (to reach the leftmost leaf of the
        // B+ tree branch that is rooted at node)
        while (!currentNode.isLeaf()) {
            currentNode = currentNode.child(0);
        }

        // return the first key of the leftmost leaf of the specified branch
        return currentNode.key(0);
    }

    // private

    // the degree of this B+ tree
    private final int degree;
    // the root node of this B+ tree
    private BPNode<Key> root = null;
}
