
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class Solution {

  private static boolean evenDigits(String s) {
    return s.length() % 2 == 0;
  }

  private static String trimLeadingZeros(String s) {
    s = s.replaceFirst("^0+(?!$)", "");
    return s.isEmpty() ? "0" : s;
  }

  public static void main(String[] args) throws IOException {
    List<String> stones = new ArrayList<>();
    try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
      String line = br.readLine().trim();
      for (String s : line.split("\\s+")) {
        stones.add(s);
      }
    }

    for (int i = 0; i < 25; i++) {
      List<String> nextStones = new ArrayList<>();
      for (String s : stones) {
        if (s.equals("0")) {
          nextStones.add("1");
        } else if (evenDigits(s)) {
          int mid = s.length() / 2;
          String left = trimLeadingZeros(s.substring(0, mid));
          String right = trimLeadingZeros(s.substring(mid));
          nextStones.add(left);
          nextStones.add(right);
        } else {
          nextStones.add(String.valueOf(Long.parseLong(s) * 2024));
        }
      }
      stones = nextStones;
    }
    System.out.println(stones.size());
  }
}
