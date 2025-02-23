
import java.io.*;
import java.util.*;

public class Solution {

  static int nextSecret(int s) {
    int mod = 1 << 24;
    int x = s * 64;
    s ^= x;
    s &= mod - 1;
    x = s >>> 5;
    s ^= x;
    s &= mod - 1;
    x = s * 2048;
    s ^= x;
    s &= mod - 1;
    return s;
  }

  static int encodeChange4(int c1, int c2, int c3, int c4) {
    return (c1 + 9) + (c2 + 9) * 19 + (c3 + 9) * 19 * 19 + (c4 + 9) * 19 * 19 * 19;
  }

  public static void main(String[] args) throws IOException {
    int numSteps = 2000;
    int patternCount = 19 * 19 * 19 * 19;

    List<Integer> initials = new ArrayList<>();
    try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
      String line;
      while ((line = br.readLine()) != null) {
        line = line.trim();
        if (!line.isEmpty()) {
          initials.add(Integer.parseInt(line));
        }
      }
    }

    int[] globalSum = new int[patternCount];
    Arrays.fill(globalSum, 0);

    for (int initVal : initials) {
      int[] prices = new int[numSteps + 1];
      int s = initVal;
      for (int i = 0; i <= numSteps; i++) {
        prices[i] = s % 10;
        s = nextSecret(s);
      }

      int[] localPrice = new int[patternCount];
      Arrays.fill(localPrice, -1);

      for (int i = 0; i < numSteps - 3; i++) {
        int c1 = prices[i + 1] - prices[i];
        int c2 = prices[i + 2] - prices[i + 1];
        int c3 = prices[i + 3] - prices[i + 2];
        int c4 = prices[i + 4] - prices[i + 3];

        if (c1 >= -9 && c1 <= 9 && c2 >= -9 && c2 <= 9 && c3 >= -9 && c3 <= 9 && c4 >= -9 &&
            c4 <= 9) {
          int idx = encodeChange4(c1, c2, c3, c4);
          if (localPrice[idx] < 0) {
            localPrice[idx] = prices[i + 4];
          }
        }
      }

      for (int idx = 0; idx < patternCount; idx++) {
        if (localPrice[idx] >= 0) {
          globalSum[idx] += localPrice[idx];
        }
      }
    }

    int maxVal = 0;
    for (int val : globalSum) {
      maxVal = Math.max(maxVal, val);
    }
    System.out.println(maxVal);
  }
}
