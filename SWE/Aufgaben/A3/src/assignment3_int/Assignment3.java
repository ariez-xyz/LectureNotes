package assignment3_int;

public interface Assignment3 {
	
   /** returns the distance between two strings, or throws some RuntimeException if anything goes wrong */
   public int getDistance(String s, String t);
   
   /** sets the strategy for calculating the distance between two strings */
   public void setStringComparison(StringComparer c);
}