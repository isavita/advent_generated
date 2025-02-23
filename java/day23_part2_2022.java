
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ElfSimulation {

  static class Elf {
    int x, y;
    boolean moving;
    int nextX, nextY;

    Elf(int x, int y) {
      this.x = x;
      this.y = y;
    }
  }

  static boolean aroundAllEmpty(Elf e, Map<Integer, Elf> map, int[][] dirs) {
    for (int[] dir : dirs) {
      if (map.containsKey(hash(e.x + dir[0], e.y + dir[1]))) {
        return false;
      }
    }
    return true;
  }

  static boolean elfInDirection(Elf e, int wannaGo, Map<Integer, Elf> map, int[][] dirs) {
    for (int j = -1; j <= 1; j++) {
      int dirIndex = (wannaGo + j + 8) % 8;
      int[] dir = dirs[dirIndex];
      if (map.containsKey(hash(e.x + dir[0], e.y + dir[1]))) {
        return true;
      }
    }
    return false;
  }

  static int hash(int x, int y) {
    return x * 10000 + y;
  }

  static boolean run(
      List<Elf> elves, Map<Integer, Elf> map, int[] order, int currDir, int[][] dirs) {
    Map<Integer, Integer> proposes = new HashMap<>();
    for (Elf elf : elves) {
      if (aroundAllEmpty(elf, map, dirs)) {
        continue;
      }
      for (int i = 0; i < 4; i++) {
        int dir_ = order[(currDir + i) % 4];
        if (elfInDirection(elf, dir_, map, dirs)) {
          continue;
        }
        int[] dxy = dirs[dir_];
        int destX = elf.x + dxy[0];
        int destY = elf.y + dxy[1];
        int destHash = hash(destX, destY);
        proposes.put(destHash, proposes.getOrDefault(destHash, 0) + 1);
        elf.nextX = destX;
        elf.nextY = destY;
        elf.moving = true;
        break;
      }
    }

    boolean someoneMoved = false;
    for (Elf elf : elves) {
      if (!elf.moving) {
        continue;
      }
      int nextHash = hash(elf.nextX, elf.nextY);
      if (proposes.get(nextHash) > 1) {
        elf.moving = false;
        continue;
      }
      someoneMoved = true;
      map.remove(hash(elf.x, elf.y));
      elf.x = elf.nextX;
      elf.y = elf.nextY;
      map.put(hash(elf.x, elf.y), elf);
      elf.moving = false;
    }

    return someoneMoved;
  }

  public static void main(String[] args) throws IOException {
    int[][] dirs = {{-1, -1}, {-1, 0}, {-1, 1}, {0, 1}, {1, 1}, {1, 0}, {1, -1}, {0, -1}};
    int[] order = {1, 5, 7, 3};
    int currDir = 0;

    List<Elf> elves = new ArrayList<>();
    Map<Integer, Elf> map = new HashMap<>();

    List<String> lines = Files.readAllLines(Paths.get("input.txt"));
    for (int row = 0; row < lines.size(); row++) {
      String line = lines.get(row);
      for (int col = 0; col < line.length(); col++) {
        if (line.charAt(col) == '#') {
          Elf elf = new Elf(row, col);
          elves.add(elf);
          map.put(hash(row, col), elf);
        }
      }
    }

    int i = 0;
    while (true) {
      if (!run(elves, map, order, currDir, dirs)) {
        System.out.println(i + 1);
        break;
      }
      currDir = (currDir + 1) % 4;
      i++;
    }
  }
}
