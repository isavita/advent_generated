
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Solution {

  static class Key {
    String code;
    int robots;
    int maxRobots;

    public Key(String code, int robots, int maxRobots) {
      this.code = code;
      this.robots = robots;
      this.maxRobots = maxRobots;
    }

    @Override
    public boolean equals(Object o) {
      if (this == o) return true;
      if (o == null || getClass() != o.getClass()) return false;
      Key key = (Key) o;
      return robots == key.robots
          && maxRobots == key.maxRobots
          && code.equals(key.code);
    }

    @Override
    public int hashCode() {
      return java.util.Objects.hash(code, robots, maxRobots);
    }
  }

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
      if (!(0 <= currI && currI < mat.length && 0 <= currJ && currJ < mat[currI].length())
          || mat[currI].charAt(currJ) == ' ') {
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
    }
    return true;
  }

  static String generateMoves(int[] position, char objective, String[] pad) {
    int[] objPos = findPosition(pad, objective);
    int posI = position[0];
    int posJ = position[1];
    int objPosI = objPos[0];
    int objPosJ = objPos[1];

    StringBuilder result = new StringBuilder();
    if (posJ > objPosJ) {
      result.append("<".repeat(posJ - objPosJ));
    }
    if (posI > objPosI) {
      result.append("^".repeat(posI - objPosI));
    }
    if (posI < objPosI) {
      result.append("v".repeat(objPosI - posI));
    }
    if (posJ < objPosJ) {
      result.append(">".repeat(objPosJ - posJ));
    }

    if (!ok(pad, position, result.toString())) {
      result = new StringBuilder();
      if (posJ < objPosJ) {
        result.append(">".repeat(objPosJ - posJ));
      }
      if (posI > objPosI) {
        result.append("^".repeat(posI - objPosI));
      }
      if (posI < objPosI) {
        result.append("v".repeat(objPosI - posI));
      }
      if (posJ > objPosJ) {
        result.append("<".repeat(posJ - objPosJ));
      }
    }

    return result.toString();
  }

  static long solve(
      String code,
      int robots,
      String[] keyPad,
      String[] robotPad,
      int maxRobots,
      Map<Key, Long> memo) {
    Key key = new Key(code, robots, maxRobots);
    if (memo.containsKey(key)) {
      return memo.get(key);
    }

    if (robots <= 0) {
      return code.length();
    }

    long ret = 0;
    int posI = 3, posJ = 2;
    if (robots != maxRobots) {
      posI = 0;
    }

    for (char ch : code.toCharArray()) {
      String moves;
      if (robots == maxRobots) {
        moves = generateMoves(new int[] {posI, posJ}, ch, keyPad);
        int[] pos = findPosition(keyPad, ch);
        posI = pos[0];
        posJ = pos[1];
      } else {
        moves = generateMoves(new int[] {posI, posJ}, ch, robotPad);
        int[] pos = findPosition(robotPad, ch);
        posI = pos[0];
        posJ = pos[1];
      }
      ret += solve(moves + "A", robots - 1, keyPad, robotPad, maxRobots, memo);
    }

    memo.put(key, ret);
    return ret;
  }

  public static void main(String[] args) throws IOException {
    List<String> content = Files.readAllLines(Paths.get("input.txt"));

    int maxRobots = 26;
    String[] keyPad = {"789", "456", "123", " 0A"};
    String[] robotPad = {" ^A", "<v>"};

    long ret = 0;
    for (String code : content) {
      code = code.trim();
      if (code.isEmpty()) {
        continue;
      }

      long numericPart = 0;
      for (char char_ : code.toCharArray()) {
        if ('0' <= char_ && char_ <= '9') {
          numericPart = numericPart * 10 + (char_ - '0');
        }
      }

      ret += solve(code, maxRobots, keyPad, robotPad, maxRobots, new HashMap<>()) * numericPart;
    }

    System.out.println(ret);
  }
}
