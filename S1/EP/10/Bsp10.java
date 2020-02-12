//David Pape 01634454

import java.util.Arrays;

public class Bsp10 {
    public static void main(String[] args) {
        Shape[] ps = new Shape [8];
        ps[0] = new Triangle ();  ps[0].scale (10, 18);  ps[0].shift (15, 5);
        ps[1] = new Parallelogram ();  ps[1].scale (1, 2);  ps[1].shift (15, 3);
        ps[2] = new Star (5, 0.4);  ps[2].scale (4, 4);  ps [2].shift (15, 25);
        ps[3] = new Ring (0.5);  ps[3].scale (2, 2);  ps[3].shift (7, 9);
        ps[4] = new Ring (0.5);  ps[4].scale (2, 2);  ps[4].shift (21, 12);
        ps[5] = new Ring (0.5);  ps[5].scale (2, 2);  ps[5].shift (12, 18);
        ps[6] = new Parallelogram ();  ps[6].scale (2, 1.5);  ps[6].shift (6, 2.0);
        ps[7] = new Parallelogram ();  ps[7].scale (2, 1.5);
        ps[7].rotate (-0.8);  ps[7].shift (2, 2.5);
        draw (ps, 30);
    }

    public static void draw (Shape s, int textSize) {
        String[] str = new String[textSize];
        Arrays.fill(str, "");

        for(int x1 = 0; x1 < textSize; x1++) {
            for(int x0 = 0; x0 < textSize * 2; x0++) {
                boolean lower = s.inside(0.5 * x0,x1 - 0.25);
                boolean upper = s.inside(0.5 * x0,x1 + 0.25);

                if (upper && lower) str[x1] += '+';
                else if (upper) str[x1] += "'";
                else if (lower) str[x1] += ",";
                else str[x1] += " ";
            }
        }

        for(int i = str.length - 1; i >= 0; i--)
            System.out.println(str[i]);
    }

    public static void draw (Shape[] s, int textSize) {
        int[][] upperArray = new int[textSize][textSize * 2];
        int[][] lowerArray = new int[textSize][textSize * 2];

        for(Shape shape : s)
            for(int x1 = 0; x1 < textSize; x1++) {
                for(int x0 = 0; x0 < textSize * 2; x0++) {
                    if(shape.inside(0.5 * x0,x1 - 0.25))
                        lowerArray[x1][x0]++;
                    if(shape.inside(0.5 * x0,x1 + 0.25))
                        upperArray[x1][x0]++;
                }
            }

        for(int i = upperArray.length - 1; i >= 0; i--) {
            String str = "";
            for (int e = 0; e < upperArray[0].length; e++) {
                 boolean lower = lowerArray[i][e] > 0;
                 boolean upper = upperArray[i][e] > 0;

                 if (upper && lower) str += '+';
                 else if (upper) str += "'";
                 else if (lower) str += ",";
                 else str += " ";
            }
            System.out.println(str);
        }
    }
}
