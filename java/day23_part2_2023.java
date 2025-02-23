
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;

public class HikingTrails {

  static class Coord {
    int x, y;

    Coord(int x, int y) {
      this.x = x;
      this.y = y;
    }

    Coord add(Coord other) {
      return new Coord(this.x + other.x, this.y + other.y);
    }

    @Override
    public boolean equals(Object o) {
      if (this == o) return true;
      if (o == null || getClass() != o.getClass()) return false;
      Coord coord = (Coord) o;
      return x == coord.x && y == coord.y;
    }

    @Override
    public int hashCode() {
      return Objects.hash(x, y);
    }
  }

  static class Grid {
    int width, height;
    Map<Coord, Character> data;

    Grid(int width, int height) {
      this.width = width;
      this.height = height;
      this.data = new HashMap<>();
    }
  }

  static final Coord North = new Coord(0, -1);
  static final Coord South = new Coord(0, 1);
  static final Coord West = new Coord(-1, 0);
  static final Coord East = new Coord(1, 0);

  static final char Empty = '.';
  static final char Wall = '#';

  static class Edge {
    Coord start, end;
    int weight;

    Edge(Coord start, Coord end, int weight) {
      this.start = start;
      this.end = end;
      this.weight = weight;
    }
  }

  static class Graph {
    Set<Coord> vertices = new HashSet<>();
    Map<Coord, Set<Edge>> edges = new HashMap<>();
  }

  static boolean isInBounds(Grid grid, Coord coord) {
    return 0 <= coord.x && coord.x < grid.width && 0 <= coord.y && coord.y < grid.height;
  }

  static Grid parseInput(List<String> input) {
    Grid grid = new Grid(input.get(0).length(), input.size());
    for (int y = 0; y < input.size(); y++) {
      String line = input.get(y);
      for (int x = 0; x < line.length(); x++) {
        char charAt = line.charAt(x);
        if (charAt != Empty) {
          grid.data.put(new Coord(x, y), charAt);
        }
      }
    }
    return grid;
  }

  static boolean isValidNeighbor(Grid grid, Coord coord, Coord dir) {
    if (!isInBounds(grid, coord)) {
      return false;
    }
    return grid.data.getOrDefault(coord, Empty) != Wall;
  }

  static List<Coord> neighbors4(Grid grid, Coord coord) {
    Coord[] directions = {North, South, West, East};
    List<Coord> validNeighbors = new ArrayList<>();
    for (Coord dir : directions) {
      Coord neighbor = coord.add(dir);
      if (isValidNeighbor(grid, neighbor, dir)) {
        validNeighbors.add(neighbor);
      }
    }
    return validNeighbors;
  }

  static Graph getGraph(Grid grid, Coord start, Coord end) {
    Graph graph = new Graph();
    graph.vertices.add(start);
    graph.vertices.add(end);

    for (int y = 0; y < grid.height; y++) {
      for (int x = 0; x < grid.width; x++) {
        Coord coord = new Coord(x, y);
        if (!grid.data.containsKey(coord)) {
          if (neighbors4(grid, coord).size() > 2) {
            graph.vertices.add(coord);
          }
        }
      }
    }

    for (Coord v : graph.vertices) {
      graph.edges.put(v, getEdgesBfs(grid, v, graph.vertices));
    }

    return graph;
  }

  static Set<Edge> getEdgesBfs(Grid grid, Coord start, Set<Coord> vertices) {
    Queue<Coord> frontier = new LinkedList<>();
    frontier.add(start);
    Set<Coord> reached = new HashSet<>();
    reached.add(start);
    Map<Coord, Integer> distances = new HashMap<>();
    distances.put(start, 0);
    Set<Edge> edges = new HashSet<>();

    while (!frontier.isEmpty()) {
      Coord current = frontier.poll();

      if (vertices.contains(current) && !current.equals(start)) {
        edges.add(new Edge(start, current, distances.get(current)));
        continue;
      }

      for (Coord next : neighbors4(grid, current)) {
        if (!reached.contains(next)) {
          frontier.add(next);
          reached.add(next);
          distances.put(next, distances.get(current) + 1);
        }
      }
    }

    return edges;
  }

  static int getMaxDistanceDfs(Graph graph, Coord current, Coord end, Set<Coord> seen) {
    if (current.equals(end)) {
      return 0;
    }

    int maxi = -1;
    seen.add(current);
    for (Edge edge : graph.edges.getOrDefault(current, new HashSet<>())) {
      if (!seen.contains(edge.end)) {
        int dist = getMaxDistanceDfs(graph, edge.end, end, seen);
        if (dist != -1) {
          maxi = Math.max(maxi, dist + edge.weight);
        }
      }
    }
    seen.remove(current);
    return maxi;
  }

  static int solve(List<String> input) {
    Grid grid = parseInput(input);
    Coord start = new Coord(1, 0);
    Coord end = new Coord(grid.width - 2, grid.height - 1);
    Graph graph = getGraph(grid, start, end);
    return getMaxDistanceDfs(graph, start, end, new HashSet<>());
  }

  public static void main(String[] args) throws IOException {
    List<String> input = Files.readAllLines(Paths.get("input.txt"));
    System.out.println(solve(input));
  }
}
