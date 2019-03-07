// David Pape 01634454

public class Bsp06 {

  public static int[] greatestRatioPos (int[] gearsA, int[] gearsB) {
    int min = 0;
    int max = 0;

    for(int i = 0; i < gearsA.length; i++)
      if (gearsA[i] > gearsA[max])
        max = i;

    for(int i = 0; i < gearsB.length; i++)
      if (gearsB[i] < gearsB[min])
        min = i;

    int[] a = {max, min};
    return a;
  }

  public static int[] nearestRatioPos (int[] gearsA, int[] gearsB, double ratio) {
    int[] closest = {0, 0};

    for(int i = 0; i < gearsA.length; i++)
      for(int j = 0; j < gearsB.length; j++)
        if (Math.abs(ratio - ((double)gearsA[i] / (double)gearsB[j])) < Math.abs(ratio - ((double)gearsA[closest[0]] / (double)gearsB[closest[1]]))) {
          closest[0] = i;
          closest[1] = j;
        }

    return closest;
  }

  public static double ratioError (int[] gearsA, int[] gearsB, double[] ratios) {
    double sum = 0;

    for(int k = 0; k < ratios.length; k++) {
      int[] closest = {0, 0};

      for(int i = 0; i < gearsA.length; i++)
        for(int j = 0; j < gearsB.length; j++)
          if (Math.abs(ratios[k] - ((double)gearsA[i] / (double)gearsB[j])) < Math.abs(ratios[k] - ((double)gearsA[closest[0]] / (double)gearsB[closest[1]]))) {
            closest[0] = i;
            closest[1] = j;
          }

      sum += Math.abs(ratios[k] - ((double)gearsA[closest[0]] / (double)gearsB[closest[1]]));
    }

    return sum;
  }

  public static void improveGears (int[] gearsA, int[] gearsB, double[] ratios) {
    double bestRatio = ratioError(gearsA, gearsB, ratios);
    int optimizationIndex = 0;
    boolean isGearsB = false;

    for (int i = 0; i < gearsA.length; i++) {
      gearsA[i] += 1;
      if(ratioError(gearsA, gearsB, ratios) < bestRatio) {
        bestRatio = ratioError(gearsA, gearsB, ratios);
        optimizationIndex = i;
      }
      gearsA[i] -= 1;
    }

    for (int i = 0; i < gearsB.length; i++) {
      gearsB[i] += 1;
      if(ratioError(gearsA, gearsB, ratios) < bestRatio) {
        bestRatio = ratioError(gearsA, gearsB, ratios);
        optimizationIndex = i;
        isGearsB = true;
      }
      gearsB[i] -= 1;
    }

    if(isGearsB)
      gearsB[optimizationIndex] += 1;
    else
      gearsA[optimizationIndex] += 1;
  }
}
