
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Solution {

  static int parseVal(String s) {
    s = s.strip();
    s = s.replaceAll("^(X\\+|Y\\+|X=|Y=)", "");
    return Integer.parseInt(s);
  }

  static int parseValPrize(String s) {
    s = s.strip();
    s = s.replaceAll("^(X=|Y=)", "");
    return Integer.parseInt(s);
  }

  static int[] parseLine(String s) {
    String[] parts = s.strip().split(",");
    int x = parseVal(parts[0]);
    int y = parseVal(parts[1]);
    return new int[] {x, y};
  }

  static int[] parsePrize(String s) {
    String[] parts = s.strip().split(",");
    int x = parseValPrize(parts[0]);
    int y = parseValPrize(parts[1]);
    return new int[] {x, y};
  }

  static Map<String, Integer> parseMachine(List<String> lines) {
    Map<String, Integer> m = new HashMap<>();
    for (String l : lines) {
      l = l.replace("Button A:", "A:");
      l = l.replace("Button B:", "B:");
      l = l.replace("Prize:", "P:");
      if (l.startsWith("A:")) {
        int[] parsed = parseLine(l.substring(2));
        m.put("ax", parsed[0]);
        m.put("ay", parsed[1]);
      } else if (l.startsWith("B:")) {
        int[] parsed = parseLine(l.substring(2));
        m.put("bx", parsed[0]);
        m.put("by", parsed[1]);
      } else if (l.startsWith("P:")) {
        int[] parsed = parsePrize(l.substring(2));
        m.put("px", parsed[0]);
        m.put("py", parsed[1]);
      }
    }
    return m;
  }

  static int solveMachine(Map<String, Integer> m) {
    int minCost = -1;
    for (int aCount = 0; aCount <= 100; aCount++) {
      for (int bCount = 0; bCount <= 100; bCount++) {
        int x = m.get("ax") * aCount + m.get("bx") * bCount;
        int y = m.get("ay") * aCount + m.get("by") * bCount;
        if (x == m.get("px") && y == m.get("py")) {
          int cost = aCount * 3 + bCount;
          if (minCost == -1 || cost < minCost) {
            minCost = cost;
          }
        }
      }
    }
    return minCost;
  }

  public static void main(String[] args) throws IOException {
    List<String> lines = Files.readAllLines(Paths.get("input.txt"));
    List<Map<String, Integer>> machines = new ArrayList<>();
    List<String> currentMachine = new ArrayList<>();
    for (String line : lines) {
      line = line.strip();
      if (line.isEmpty()) {
        if (!currentMachine.isEmpty()) {
          machines.add(parseMachine(currentMachine));
          currentMachine = new ArrayList<>();
        }
      } else {
        currentMachine.add(line);
      }
    }
    if (!currentMachine.isEmpty()) {
      machines.add(parseMachine(currentMachine));
    }

    List<Integer> results = new ArrayList<>();
    for (Map<String, Integer> m : machines) {
      int result = solveMachine(m);
      if (result != -1) {
        results.add(result);
      }
    }

    if (results.isEmpty()) {
      System.out.println("0 0");
    } else {
      System.out.println(results.size() + " " + results.stream().mapToInt(Integer::intValue).sum());
    }
  }
}
