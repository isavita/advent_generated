
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;

public class Solution {

  public static String readAll(String path) throws IOException {
    return Files.readString(Paths.get(path)).strip();
  }

  public static long num(int[] w) {
    long n = 0;
    for (int digit : w) {
      n = n * 10 + digit;
    }
    return n;
  }

  public static void main(String[] args) throws IOException {
    List<Integer> k = new ArrayList<>();
    List<Integer> l = new ArrayList<>();
    List<Integer> m = new ArrayList<>();
    String[] lines = readAll("input.txt").split("\n");
    for (int i = 0; i < lines.length; i++) {
      int v;
      if (i % 18 == 4) {
        v = Integer.parseInt(lines[i].split(" ")[2]);
        l.add(v);
      } else if (i % 18 == 5) {
        v = Integer.parseInt(lines[i].split(" ")[2]);
        k.add(v);
      } else if (i % 18 == 15) {
        v = Integer.parseInt(lines[i].split(" ")[2]);
        m.add(v);
      }
    }

    Map<Integer, int[]> constraints = new HashMap<>();
    Stack<Integer> stack = new Stack<>();
    for (int i = 0; i < l.size(); i++) {
      if (l.get(i) == 1) {
        stack.push(i);
      } else if (l.get(i) == 26) {
        int pop = stack.pop();
        constraints.put(pop, new int[] {i, m.get(pop) + k.get(i)});
      }
    }

    int[] minVal = new int[14];
    for (int i = 0; i < 14; i++) {
      if (!constraints.containsKey(i)) {
        continue;
      }
      int vmin = 1;
      while (vmin + constraints.get(i)[1] < 1) {
        vmin++;
      }
      minVal[i] = vmin;
      minVal[constraints.get(i)[0]] = vmin + constraints.get(i)[1];
    }
    System.out.println(num(minVal));
  }
}
