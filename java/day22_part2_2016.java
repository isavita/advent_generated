
import java.io.*;
import java.util.*;
import java.util.regex.*;

public class Solution {
  static class Point {
    int x, y;

    Point(int x, int y) {
      this.x = x;
      this.y = y;
    }

    @Override
    public boolean equals(Object o) {
      if (this == o) return true;
      if (o == null || getClass() != o.getClass()) return false;
      Point point = (Point) o;
      return x == point.x && y == point.y;
    }

    @Override
    public int hashCode() {
      return Objects.hash(x, y);
    }
  }

  static class Node {
    int used, avail;

    Node(int used, int avail) {
      this.used = used;
      this.avail = avail;
    }
  }

  static Point[] Neighbors4 = {
    new Point(0, 1), new Point(0, -1), new Point(1, 0), new Point(-1, 0)
  };
  static Pattern re_pattern = Pattern.compile("-x(\\d+)-y(\\d+)");

  public static void main(String[] args) throws IOException {
    Map<Point, Node> nodes = new HashMap<>();
    BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
    String line;
    reader.readLine();
    reader.readLine();
    while ((line = reader.readLine()) != null) {
      String[] fields = line.split("\\s+");
      Matcher matcher = re_pattern.matcher(fields[0]);
      matcher.find();
      Point p = new Point(Integer.parseInt(matcher.group(1)), Integer.parseInt(matcher.group(2)));
      Node n = new Node(Integer.parseInt(fields[2].substring(0, fields[2].length() - 1)), Integer.parseInt(fields[3].substring(0, fields[3].length() - 1)));
      nodes.put(p, n);
    }
    reader.close();
    System.out.println(minmoves(nodes));
  }

  static int minmoves(Map<Point, Node> nodes) {
    int w = dim(nodes).x;
    Point goal = new Point(w, 0);
    Point hole = find_hole(nodes);
    int moves_sum = 0;
    while (!goal.equals(new Point(0, 0))) {
      Point next_pos = new Point(goal.x - 1, 0);
      moves_sum += moves(nodes, goal, hole, next_pos);
      hole = next_pos;
      moves_sum += moves(nodes, goal, goal, hole);
      goal = hole;
      hole = new Point(goal.x + 1, 0);
    }
    return moves_sum;
  }

  static Point find_hole(Map<Point, Node> nodes) {
    for (Map.Entry<Point, Node> entry : nodes.entrySet()) {
      if (entry.getValue().used == 0) {
        return entry.getKey();
      }
    }
    throw new IllegalArgumentException("no hole");
  }

  static int moves(Map<Point, Node> nodes, Point goal, Point from_pos, Point to_pos) {
    Point dimensions = dim(nodes);
    int w = dimensions.x;
    int h = dimensions.y;

    Map<Point, Integer> depth = new HashMap<>();
    depth.put(from_pos, 0);
    PriorityQueue<AbstractMap.SimpleEntry<Integer, Point>> pq = new PriorityQueue<>(Comparator.comparingInt(AbstractMap.SimpleEntry::getKey));

    pq.add(new AbstractMap.SimpleEntry<>(0, from_pos));

    while (!pq.isEmpty()) {
      AbstractMap.SimpleEntry<Integer, Point> current = pq.poll();
      int curr_depth = current.getKey();
      Point p = current.getValue();
      if (p.equals(to_pos)) {
        return curr_depth;
      }
      int nextDepth = depth.get(p) + 1;
      for (Point n : Neighbors4) {
        Point next_pos = new Point(p.x + n.x, p.y + n.y);
        if (next_pos.x < 0 || next_pos.y < 0 || next_pos.x > w || next_pos.y > h || nodes.get(next_pos).used > 400 || next_pos.equals(goal)) {
          continue;
        }

        if (!depth.containsKey(next_pos) || nextDepth < depth.get(next_pos)) {
          depth.put(next_pos, nextDepth);
          pq.add(new AbstractMap.SimpleEntry<>(nextDepth, next_pos));
        }
      }
    }
    throw new IllegalArgumentException("no possible path");
  }

  static Point dim(Map<Point, Node> nodes) {
    int w = 0, h = 0;
    for (Point p : nodes.keySet()) {
      if (p.x > w) {
        w = p.x;
      }
      if (p.y > h) {
        h = p.y;
      }
    }
    return new Point(w, h);
  }
}
