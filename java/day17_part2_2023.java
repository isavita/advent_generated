
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.PriorityQueue;

public class HeatLoss {

  static class State {

    int x, y, heatLoss, steps;
    char direction;

    State(int heatLoss, int x, int y, char direction, int steps) {
      this.heatLoss = heatLoss;
      this.x = x;
      this.y = y;
      this.direction = direction;
      this.steps = steps;
    }
  }

  private static int[][] readInput(String filePath) throws IOException {
    BufferedReader reader = new BufferedReader(new FileReader(filePath));
    return reader.lines().map(line -> line.chars().map(c -> c - '0').toArray()).toArray(int[][]::new);
  }

  private static int dijkstra(int[][] grid, int part) {
    int height = grid.length;
    int width = grid[0].length;
    int[] end = {width - 1, height - 1};

    Map<Character, int[]> directions = new HashMap<>();
    directions.put('N', new int[] {0, -1});
    directions.put('S', new int[] {0, 1});
    directions.put('E', new int[] {1, 0});
    directions.put('W', new int[] {-1, 0});

    Map<Character, Map<Character, Character>> directionTurns = new HashMap<>();
    directionTurns.put(
        'N',
        new HashMap<>() {
          {
            put('L', 'W');
            put('R', 'E');
          }
        });
    directionTurns.put(
        'S',
        new HashMap<>() {
          {
            put('L', 'E');
            put('R', 'W');
          }
        });
    directionTurns.put(
        'E',
        new HashMap<>() {
          {
            put('L', 'N');
            put('R', 'S');
          }
        });
    directionTurns.put(
        'W',
        new HashMap<>() {
          {
            put('L', 'S');
            put('R', 'N');
          }
        });

    PriorityQueue<State> heap = new PriorityQueue<>((a, b) -> a.heatLoss - b.heatLoss);
    heap.add(new State(0, 0, 0, 'E', 0));

    Map<String, Integer> visited = new HashMap<>();

    while (!heap.isEmpty()) {
      State current = heap.poll();

      if (current.x == end[0] && current.y == end[1]) {
        return current.heatLoss;
      }

      String stateKey = current.x + "," + current.y + "," + current.direction + "," + current.steps;
      if (visited.containsKey(stateKey) && visited.get(stateKey) <= current.heatLoss) {
        continue;
      }

      visited.put(stateKey, current.heatLoss);

      int maxSteps = (part == 1) ? 3 : 10;
      int minStepsBeforeTurn = (part == 1) ? 1 : 4;

      if (current.steps < maxSteps) {
        int[] dir = directions.get(current.direction);
        int nx = current.x + dir[0];
        int ny = current.y + dir[1];

        if (nx >= 0 && nx < width && ny >= 0 && ny < height) {
          heap.add(
              new State(
                  current.heatLoss + grid[ny][nx], nx, ny, current.direction, current.steps + 1));
        }
      }

      if (current.steps >= minStepsBeforeTurn) {
        for (char turn : new char[] {'L', 'R'}) {
          char newDirection = directionTurns.get(current.direction).get(turn);
          int[] dir = directions.get(newDirection);
          int nx = current.x + dir[0];
          int ny = current.y + dir[1];

          if (nx >= 0 && nx < width && ny >= 0 && ny < height) {
            heap.add(new State(current.heatLoss + grid[ny][nx], nx, ny, newDirection, 1));
          }
        }
      }
    }

    return -1;
  }

  public static void main(String[] args) throws IOException {
    int[][] grid = readInput("input.txt");
    System.out.println("Part One Answer: " + dijkstra(grid, 1));
    System.out.println("Part Two Answer: " + dijkstra(grid, 2));
  }
}
