//David Pape 01634454

public class Bsp05 {

  public static void main(String[] args) {
    String word = createWord(SavitchIn.readLineInt());

    System.out.println("Zufallswort: " + word);

    int position = SavitchIn.readLineInt();
    char replacement = SavitchIn.readLineNonwhiteChar();

    System.out.println("neues Wort: " + replaceAt(word, position, replacement));

    String secretWord = createWord(SavitchIn.readLineInt());
    String guessedWord = guessWord(secretWord, 10);

    System.out.println("gesuchtes Wort: " + secretWord);
    System.out.println("erratenes Wort: " + guessedWord);

    if (secretWord.equals(guessedWord)) {
      System.out.println("erraten");
    } else {
      System.out.println("nicht erraten");
    }
  }

  public static String replaceAt (String text, int pos, char newChar) {
    String newStr = "";

    for (int i = 0; i < text.length(); i++) {
      if (i == pos) {
        newStr += newChar;
      } else {
        newStr += text.charAt(i);
      }
    }

    return newStr;
  }

  public static String guessWord(String wordToGuess, int maxTrials) {
    String guess = "";
    for (int i = 0; i < wordToGuess.length(); i++) {
      guess += ".";
    }

    for (int i = 1; i <= maxTrials; i++) {
      if (guess.equals(wordToGuess)) {
        return guess;
      }

      char c = SavitchIn.readLineNonwhiteChar();
      String newGuess = "";

      for (int j = 0; j < wordToGuess.length(); j++) {
        if (wordToGuess.charAt(j) == c) {
          newGuess += c;
        } else {
          newGuess += guess.charAt(j);
        }
      }

      guess = newGuess;
      System.out.println(guess);
    }

    return guess;
  }

  public static String createWord (int length) {
    if (length > 0) {
      char[] alphabet = {'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',
                         'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't',
                         'u', 'v', 'w', 'x', 'y', 'z'};
      StringBuilder sb = new StringBuilder(length);
      for (int i = 0; i < length; i++)
          sb.append(alphabet[PRNG.randomInt(alphabet.length)]);
      return sb.toString();
    } else {
      return "";
    }
  }
}
