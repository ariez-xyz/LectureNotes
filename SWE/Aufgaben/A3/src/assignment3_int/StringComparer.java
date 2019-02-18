package assignment3_int;

public interface StringComparer {
	
   /** returns the distance between two strings, or throws some RuntimeException if anything goes wrong */
   public int compare(String s, String t);
}