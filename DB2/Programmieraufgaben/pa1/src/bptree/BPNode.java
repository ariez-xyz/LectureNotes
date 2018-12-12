package bptree;

// java imports
import java.util.List;
import java.util.ArrayList;

/**
 * <p>
 * <strong>IMPORTANT:</strong> Do not modify this class. When you submit your
 * code, all changes to this file are discarded automatically. Hence, it may
 * happen that you program is not working on the submission system.
 * </p>
 * <p>
 * A simple implementation of a B+ tree node that has an identifier, a node
 * degree, a list of keys (of a specified type), and a list of children to other
 * B+ tree nodes.
 * </p>
 *
 * @param <Key> the type of the keys stored in this B+ tree node. This type has
 *  to implement the <tt>Comparable</tt> interface.
 */
public class BPNode<Key extends Comparable<Key>> {
  /**
   * Constructs an empty B+ tree node of a specified degree.
   *
   * @param degree the node degree of this B+ tree node (i.e., m)
   */
  public BPNode(final int degree) {
    // consecutive node ids
    id = ++globalNodeId;
    this.degree = degree;

    // empty key and children lists
    keys = new ArrayList<Key>();
    children = new ArrayList<BPNode<Key>>();
  }

  /**
   * Retrieves the unique identifier of this B+ tree node.
   *
   * @return the unique identifier of this B+ tree node
   */
  public int id() {
    return id;
  }

  /**
   * Retrieves the degree of this B+ tree node.
   *
   * @return the degree of this B+ tree node
   */
  public int degree() {
    return degree;
  }

  /**
   * Retrieves the list of keys stored in this B+ tree node.
   *
   * @return the list of keys stored in this B+ tree node
   */
  public List<Key> keys() {
    return keys;
  }

  /**
   * Retrieves the list of children stored in this B+ tree node.
   *
   * @return the list of children stored in this B+ tree node
   */
  public List<BPNode<Key>> children() {
    return children;
  }

  /**
   * Retrieves the type of a B+ tree node, i.e., is it a leaf node or not.
   *
   * @return <tt>true</tt> if this B+ tree node is a leaf, <tt>false</tt>
   *  otherwise
   */
  public boolean isLeaf() {
    // we have to deal with this case somehow
    if (children.isEmpty()) {
      return true;
    }

    return (children.get(0) == null);
  }

  /**
   * Retrieves a reference to the child at a specified index (0-based) of this
   * B+ tree node. Be aware that this method does not perform any range checks
   * for the given index.
   *
   * @param index the index of the child to retrieve (0-based)
   *
   * @return the reference to the child at the specified index of this B+ tree
   *  node
   *
   * @throws IndexOutOfBoundsException
   */
  public BPNode<Key> child(final int index) {
    return children.get(index);
  }

  /**
   * Retrieves a reference to the first child aof this B+ tree node if it
   * exists.
   *
   * @return the reference to the first child of this B+ tree node if it exists,
   *  <tt>null</tt> otherwise
   */
  public BPNode<Key> firstChild() {
    try {
      return child(0);
    } catch (IndexOutOfBoundsException ioobe) {
      return null;
    }
  }

  /**
   * Retrieves the key at a specified index (0-based) of this B+ tree node. Be
   * aware that this method does not perform any range checks for the given
   * index.
   *
   * @param index the index of the key to retrieve (0-based)
   *
   * @return the key at the specified index of this B+ tree node
   *
   * @throws IndexOutOfBoundsException if the index is out of range
   *  (<tt>index < 0 || index > size()</tt>)
   */
  public Key key(final int index) {
    return keys.get(index);
  }

  /**
   * Adds the specified key at a specified index (0-based) of this B+ tree node.
   * Be aware that this method does not perform any range checks for the given
   * index.
   *
   * @param index the index the specified key is added at (0-based)
   *
   * @throws IndexOutOfBoundsException if the index is out of range
   *  (<tt>index < 0 || index > size()</tt>)
   */
  public void addKey(final int index, final Key key) {
    keys.add(index, key);
  }

  /**
   * Appends the specified key to this B+ tree node. This is equivalent to the
   * call <tt>addKey(keys().size(), key)</tt>.
   *
   * @throws IndexOutOfBoundsException if the index is out of range
   *  (<tt>index < 0 || index > size()</tt>)
   */
  public void addKey(final Key key) {
    addKey(keys.size(), key);
  }

  /**
   * Adds the specified child at a specified index (0-based) of this B+ tree
   * node. Be aware that this method does not perform any range checks for the
   * given index.
   *
   * @param index the index the specified child is added at (0-based)
   *
   * @throws IndexOutOfBoundsException if the index is out of range
   *  (<tt>index < 0 || index > size()</tt>)
   */
  public void addChild(final int index, final BPNode<Key> child) {
    children.add(index, child);
  }

  /**
   * Appends the specified child to this B+ tree node. This is equivalent to the
   * call <tt>addChild(children().size(), child)</tt>.
   *
   * @throws IndexOutOfBoundsException if the index is out of range
   *  (<tt>index < 0 || index > size()</tt>)
   */
  public void addChild(final BPNode<Key> child) {
    addChild(children.size(), child);
  }

  /**
   * Removes the key at a specified index (0-based) of this B+ tree node. Be
   * aware that this method does not perform any range checks for the given
   * index.
   *
   * @param index the index of the key to be removed (0-based)
   *
   * @return the removed key
   *
   * @throws IndexOutOfBoundsException if the index is out of range
   *  (<tt>index < 0 || index > size()</tt>)
   */
  public Key removeKey(final int index) {
    return keys.remove(index);
  }

  /**
   * Removes the last key of this B+ tree node. This is equivalent to the call
   * <tt>removeKey(keys().size() - 1)</tt>.
   *
   * @return the removed key if it existed, <tt>null</tt> otherwise
   *
   * @throws IndexOutOfBoundsException if the index is out of range
   *  (<tt>index < 0 || index > size()</tt>)
   */
  public Key removeLastKey() {
    try {
      return removeKey(keys.size() - 1);
    } catch (IndexOutOfBoundsException ioobe) {
      return null;
    }
  }

  /**
   * Removes the child at a specified index (0-based) of this B+ tree node. Be
   * aware that this method does not perform any range checks for the given
   * index.
   *
   * @param index the index of the child to be removed (0-based)
   *
   * @return the removed child
   *
   * @throws IndexOutOfBoundsException if the index is out of range
   *  (<tt>index < 0 || index > size()</tt>)
   */
  public BPNode<Key> removeChild(final int index) {
    return children.remove(index);
  }

  /**
   * Removes the last child of this B+ tree node. This is equivalent to the
   * call <tt>removeChild(children().size() - 1)</tt>.
   *
   * @return the removed child if it existed, <tt>null</tt> otherwise
   *
   * @throws IndexOutOfBoundsException if the index is out of range
   *  (<tt>index < 0 || index > size()</tt>)
   */
  public BPNode<Key> removeLastChild() {
    try {
      return removeChild(children.size() - 1);
    } catch (IndexOutOfBoundsException ioobe) {
      return null;
    }
  }

  /**
   * <p>
   * Overridden <tt>toString</tt> method to print this B+ tree node.
   * </p>
   *
   * @return a string representing this B+ tree node
   *
   * @see Object
   */
  @Override
  public String toString() {
    String str = new String();

    // append node identifier
    str += id() + ":";

    // append keys and children
    for (int i = 0; i < keys.size(); ++i) {
      BPNode<Key> currentNode = null;
      try {
        currentNode = children.get(i);
      } catch(IndexOutOfBoundsException ioobe) {
        System.err.println("[WARNING] BPNode has too few child pointers (" + children.size() +
          ") for its number of keys (" + keys.size() + ")");
        return "";
      }
      str += (currentNode == null ? "0" : currentNode.id()) + ":";
      str += keys.get(i) + ":";
    }

    // append last child
    BPNode<Key> lastNode = null;
    try {
      lastNode = children.get(children.size() - 1);
    } catch(IndexOutOfBoundsException ioobe) {
      System.err.println("[WARNING] BPNode has no children.");
      return "";
    }
    str += (lastNode == null ? "0" : lastNode.id());

    return str;
  }

  // private non-static fields

  // unique identifier of this B+ tree node
  private final int id;
  // node degree of this B+ tree node
  private final int degree;
  // list of keys stored in this B+ tree node
  private List<Key> keys = null;
  // list of children stored in this B+ tree node
  private List<BPNode<Key>> children = null;

  // private static fields

  // global identifier that is increment whenever a new B+ tree node is created
  private static int globalNodeId = 0;
}
