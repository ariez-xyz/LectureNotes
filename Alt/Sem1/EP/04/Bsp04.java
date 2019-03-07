public class Bsp04 {

  public static void main(String[] args) {
    String text = SavitchIn.readLineWord();
    int keyInt = SavitchIn.readLineInt();

    System.out.println("Code: " + rotate(text, keyInt % 26));

    text = SavitchIn.readLineWord();
    keyInt = SavitchIn.readLineInt();

    System.out.println("Text: " + rotate(text, -keyInt % 26));

    //Teil 2

    text = SavitchIn.readLineWord();
    String key = SavitchIn.readLineWord();

    System.out.println("Code: " + rotate(text, key, true));

    text = SavitchIn.readLineWord();
    key = SavitchIn.readLineWord();

    System.out.println("Text: " + rotate(text, key, false));
  }

  /** Returns an encoded/decoded string, where a key value is added
  * to each character of a text. The key value may be negative. The alphabet
  * is cyclically traversed, i.e., 'a' comes after 'z'.
  *
  * @param text           a string with characters in 'a'-'z'
  * @param key             a value in [-25, 25]

  * @return                               the encoded/decoded string
  *
  */
  public static String rotate(String text, int key) {
    String s = "";
    for (int i = 0; i < text.length(); i++) {
      int code = ((text.charAt(i) - 97) + key) % 26;
      if(code < 0) code += 26;
      code += 97;
      s += (char)code;
    }
    return s;
  }

  /** Returns an encoded/decoded string, where key values are added
  * to the characters of a text. The key values are derived
  * from a password, where each character ['a', 'z'] is interpreted
  * as a value in [0, 25]. The i-th key value is added to or subtracted
  * from the i-th text character. The text and the password are
  * traversed cyclically.
  *
  * @param text            a string with characters in 'a'-'z'
  * @param pass   a string with characters in 'a'-'z'
  * @param encode true, if encoding (adding), otherwise decoding (subtracting)

  * @return                               the encoded/decoded string
  *
  */
  public static String rotate(String text, String pass, boolean encode) {
    String s = "";
    for (int i = 0; i < text.length(); i++) {
      int code;
      if(encode)
        code = ((text.charAt(i) - 97) + (pass.charAt(i % pass.length()) - 97)) % 26;
      else
        code = ((text.charAt(i) - 97) - (pass.charAt(i % pass.length()) - 97)) % 26;
      if(code < 0) code += 26;
      code += 97;
      s += (char)code;
    }
    return s;
  }
}
