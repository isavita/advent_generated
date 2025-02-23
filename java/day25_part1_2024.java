
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

public class Solution {

  public static void main(String[] args) throws IOException {
    List<String> raw = Files.readAllLines(Paths.get("input.txt"));
    raw.removeIf(String::isBlank);

    if (raw.size() % 7 != 0) {
      System.out.println(0);
      return;
    }

    List<int[]> locks = new ArrayList<>();
    List<int[]> keys = new ArrayList<>();

    for (int i = 0; i < raw.size(); i += 7) {
      List<String> block = raw.subList(i, i + 7);
      boolean valid = true;
      for (String ln : block) {
        if (ln.length() < 5) {
          valid = false;
          break;
        }
      }
      if (!valid) continue;

      if (block.get(0).chars().allMatch(c -> c == '#')) {
        locks.add(parseLock(block));
      } else {
        keys.add(parseKey(block));
      }
    }

    int count = 0;
    for (int[] lock : locks) {
      for (int[] key : keys) {
        if (fits(lock, key)) {
          count++;
        }
      }
    }
    System.out.println(count);
  }

  static int[] parseLock(List<String> b) {
    int[] h = new int[5];
    for (int c = 0; c < 5; c++) {
      for (int r = 1; r < 7; r++) {
        if (b.get(r).charAt(c) == '#') {
          h[c]++;
        } else {
          break;
        }
      }
    }
    return h;
  }

  static int[] parseKey(List<String> b) {
    int[] h = new int[5];
    for (int c = 0; c < 5; c++) {
      for (int r = 5; r >= 0; r--) {
        if (b.get(r).charAt(c) == '#') {
          h[c]++;
        } else {
          break;
        }
      }
    }
    return h;
  }

  static boolean fits(int[] lock, int[] key) {
    for (int i = 0; i < 5; i++) {
      if (lock[i] + key[i] > 5) {
        return false;
      }
    }
    return true;
  }
}
