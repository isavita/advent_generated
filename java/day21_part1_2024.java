
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class Solution {
  static int[] findPosition(String[] mat, char ch) {
    for (int i = 0; i < mat.length; i++) {
      for (int j = 0; j < mat[i].length(); j++) {
        if (mat[i].charAt(j) == ch) {
          return new int[] {i, j};
        }
      }
    }
    return new int[] {-1, -1};
  }

  static boolean ok(String[] mat, int[] st, String seq) {
    int currI = st[0];
    int currJ = st[1];
    for (char ch : seq.toCharArray()) {
      if (mat[currI].charAt(currJ) == ' ') {
        return false;
      }
      if (ch == '^') {
        currI -= 1;
      } else if (ch == 'v') {
        currI += 1;
      } else if (ch == '<') {
        currJ -= 1;
      } else if (ch == '>') {
        currJ += 1;
      }
      if (currI < 0 || currI >= mat.length || currJ < 0 || currJ >= mat[0].length()) {
        return false;
      }
    }
    return true;
  }

  static String generateMoves(int[] position, char objective, String[] pad) {
    int[] objPos = findPosition(pad, objective);
    int objI = objPos[0];
    int objJ = objPos[1];
    int posI = position[0];
    int posJ = position[1];
    StringBuilder ret = new StringBuilder();
    if (posJ > objJ) {
      ret.append("<".repeat(posJ - objJ));
    }
    if (posI > objI) {
      ret.append("^".repeat(posI - objI));
    }
    if (posI < objI) {
      ret.append("v".repeat(objI - posI));
    }
    if (posJ < objJ) {
      ret.append(">".repeat(objJ - posJ));
    }
    if (!ok(pad, position, ret.toString())) {
      ret = new StringBuilder();
      if (posJ < objJ) {
        ret.append(">".repeat(objJ - posJ));
      }
      if (posI > objI) {
        ret.append("^".repeat(posI - objI));
      }
      if (posI < objI) {
        ret.append("v".repeat(objI - posI));
      }
      if (posJ > objJ) {
        ret.append("<".repeat(posJ - objJ));
      }
    }
    return ret.toString();
  }

  static long solve(
      String code,
      int robots,
      String[] keyPad,
      String[] robotPad,
      int maxRobots,
      Map<String, Long> memo) {
    if (robots <= 0) {
      return code.length();
    }
    String state = code + "," + robots;
    if (memo.containsKey(state)) {
      return memo.get(state);
    }
    long ret = 0;
    int posI = 3, posJ = 2;
    if (robots != maxRobots) {
      posI = 0;
    }
    for (char ch : code.toCharArray()) {
      String moves;
      int[] newPos;
      if (robots == maxRobots) {
        moves = generateMoves(new int[] {posI, posJ}, ch, keyPad);
        newPos = findPosition(keyPad, ch);
      } else {
        moves = generateMoves(new int[] {posI, posJ}, ch, robotPad);
        newPos = findPosition(robotPad, ch);
      }
      posI = newPos[0];
      posJ = newPos[1];
      ret += solve(moves + "A", robots - 1, keyPad, robotPad, maxRobots, memo);
    }
    memo.put(state, ret);
    return ret;
  }

  public static void main(String[] args) throws IOException {
    int maxRobots = 3;
    String[] keyPad = {"789", "456", "123", " 0A"};
    String[] robotPad = {" ^A", "<v>"};
    long ret = 0;
    BufferedReader f = new BufferedReader(new FileReader("input.txt"));
    String line;
    while ((line = f.readLine()) != null) {
      line = line.trim();
      if (line.isEmpty()) {
        continue;
      }
      int numericPart = 0;
      for (char char_ : line.toCharArray()) {
        if (Character.isDigit(char_)) {
          numericPart = numericPart * 10 + (char_ - '0');
        }
      }
      Map<String, Long> memo = new HashMap<>();
      long sv = solve(line, maxRobots, keyPad, robotPad, maxRobots, memo);
      ret += sv * numericPart;
    }
    f.close();
    System.out.println(ret);
  }
}
