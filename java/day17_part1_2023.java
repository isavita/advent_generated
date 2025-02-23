
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;

public class Solution {

  static class Coord {

    int x, y;

    public Coord(int x, int y) {
      this.x = x;
      this.y = y;
    }

    public Coord add(Coord other) {
      return new Coord(this.x + other.x, this.y + other.y);
    }

    public Coord subtract(Coord other) {
      return new Coord(this.x - other.x, this.y - other.y);
    }

    @Override
    public boolean equals(Object obj) {
      if (this == obj) {
        return true;
      }
      if (obj == null || getClass() != obj.getClass()) {
        return false;
      }
      Coord other = (Coord) obj;
      return x == other.x && y == other.y;
    }

    @Override
    public int hashCode() {
      return 31 * x + y;
    }
  }

  static class Grid {

    int width, height;
    Map<Coord, Integer> data;

    public Grid(List<String> input) {
      this.height = input.size();
      this.width = input.get(0).length();
      this.data = new HashMap<>();
      for (int y = 0; y < height; y++) {
        String line = input.get(y);
        for (int x = 0; x < width; x++) {
          this.data.put(new Coord(x, y), line.charAt(x) - '0');
        }
      }
    }

    public List<Coord> neighbors4(Coord coord) {
      Coord[] directions = {
        new Coord(0, -1), new Coord(-1, 0), new Coord(0, 1), new Coord(1, 0)
      };
      List<Coord> neighbors = new ArrayList<>();
      for (Coord direction : directions) {
        Coord neighbor = coord.add(direction);
        if (neighbor.x >= 0 && neighbor.x < width && neighbor.y >= 0 && neighbor.y < height) {
          neighbors.add(neighbor);
        }
      }
      return neighbors;
    }

    public int aStarConstrained(Coord start, Coord goal, int minStraight, int maxStraight) {
      PriorityQueue<Node> frontier = new PriorityQueue<>();
      Node startNode = new Node(start, new Coord(0, 0), 0, 0);
      frontier.add(startNode);

      Map<NodeKey, Integer> costSoFar = new HashMap<>();
      costSoFar.put(startNode.getKey(), 0);

      while (!frontier.isEmpty()) {
        Node current = frontier.poll();

        if (current.coord.equals(goal)) {
          return current.cost;
        }

        for (Coord neighbor : neighbors4(current.coord)) {
          Coord newDir = neighbor.subtract(current.coord);
          int newNumStraight =
              newDir.equals(current.dir) ? current.numStraight + 1 : 1;

          int newCost = current.cost + data.get(neighbor);
          NodeKey neighborKey = new NodeKey(neighbor, newDir, newNumStraight);

          if ((current.numStraight >= minStraight || newDir.equals(current.dir)
                  || current.coord.equals(start))
              && newNumStraight <= maxStraight
              && !(newDir.x == -current.dir.x && newDir.y == -current.dir.y)) {
            if (!costSoFar.containsKey(neighborKey)
                || newCost < costSoFar.get(neighborKey)) {
              costSoFar.put(neighborKey, newCost);
              int priority = newCost + Math.abs(neighbor.x - goal.x) + Math.abs(neighbor.y - goal.y);
              frontier.add(new Node(neighbor, newDir, newNumStraight, newCost, priority));
            }
          }
        }
      }
      return -1;
    }
  }

  static class NodeKey {

    Coord coord;
    Coord dir;
    int numStraight;

    public NodeKey(Coord coord, Coord dir, int numStraight) {
      this.coord = coord;
      this.dir = dir;
      this.numStraight = numStraight;
    }

    @Override
    public boolean equals(Object obj) {
      if (this == obj) {
        return true;
      }
      if (obj == null || getClass() != obj.getClass()) {
        return false;
      }
      NodeKey other = (NodeKey) obj;
      return coord.equals(other.coord)
          && dir.equals(other.dir)
          && numStraight == other.numStraight;
    }

    @Override
    public int hashCode() {
      int result = coord.hashCode();
      result = 31 * result + dir.hashCode();
      result = 31 * result + numStraight;
      return result;
    }
  }

  static class Node implements Comparable<Node> {

    Coord coord;
    Coord dir;
    int numStraight;
    int cost;
    int priority;

    public Node(Coord coord, Coord dir, int numStraight, int cost) {
      this.coord = coord;
      this.dir = dir;
      this.numStraight = numStraight;
      this.cost = cost;
    }

    public Node(Coord coord, Coord dir, int numStraight, int cost, int priority) {
      this(coord, dir, numStraight, cost);
      this.priority = priority;
    }

    public NodeKey getKey() {
      return new NodeKey(coord, dir, numStraight);
    }

    @Override
    public int compareTo(Node other) {
      return Integer.compare(this.priority, other.priority);
    }
  }

  public static int solve(List<String> input) {
    Grid grid = new Grid(input);
    Coord start = new Coord(0, 0);
    Coord goal = new Coord(grid.width - 1, grid.height - 1);
    return grid.aStarConstrained(start, goal, 0, 3);
  }

  public static void main(String[] args) throws IOException {
    List<String> input = new ArrayList<>();
    try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
      String line;
      while ((line = br.readLine()) != null) {
        input.add(line);
      }
    }
    System.out.println(solve(input));
  }
}
