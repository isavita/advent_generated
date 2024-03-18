import 'dart:io';

class Coord {
  int x;
  int y;

  Coord(this.x, this.y);

  Coord add(Coord c) {
    return Coord(x + c.x, y + c.y);
  }

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is Coord && runtimeType == other.runtimeType && x == other.x && y == other.y;

  @override
  int get hashCode => x.hashCode ^ y.hashCode;
}

class Grid {
  int width;
  int height;
  Map<Coord, String> data;

  Grid(this.width, this.height, this.data);
}

final Coord north = Coord(0, -1);
final Coord south = Coord(0, 1);
final Coord west = Coord(-1, 0);
final Coord east = Coord(1, 0);

const String empty = '.';
const String wall = '#';
const String northSlopes = '^';
const String southSlopes = 'v';
const String westSlopes = '<';
const String eastSlopes = '>';

final Map<String, Coord> slopeToDir = {
  northSlopes: north,
  southSlopes: south,
  westSlopes: west,
  eastSlopes: east,
};

class Edge {
  Coord start;
  Coord end;
  int weight;

  Edge(this.start, this.end, this.weight);
}

class Graph {
  Set<Coord> vertices;
  Map<Coord, Set<Edge>> edges;

  Graph(this.vertices, this.edges);
}

bool isInBounds(Grid grid, Coord coord) {
  return coord.x >= 0 && coord.x < grid.width && coord.y >= 0 && coord.y < grid.height;
}

Grid parseInput(List<String> input) {
  Map<Coord, String> data = {};

  for (int y = 0; y < input.length; y++) {
    String line = input[y];
    for (int x = 0; x < line.length; x++) {
      String char = line[x];
      if (char != empty) {
        data[Coord(x, y)] = char;
      }
    }
  }

  return Grid(input[0].length, input.length, data);
}

bool isValidNeighbor(Grid grid, Coord coord, Coord dir) {
  if (!isInBounds(grid, coord)) {
    return false;
  }
  if (grid.data[coord] == wall) {
    return false;
  }
  return true;
}

bool isValidNeighborWithSlopes(Grid grid, Coord coord, Coord dir) {
  if (!isInBounds(grid, coord)) {
    return false;
  }
  if (!grid.data.containsKey(coord)) {
    return true;
  }
  if (grid.data[coord] == wall) {
    return false;
  }
  return slopeToDir[grid.data[coord]] == dir;
}

List<Coord> neighbors4(Grid grid, Coord coord, bool Function(Grid, Coord, Coord) isValidNeighborFunc) {
  List<Coord> directions = [north, south, west, east];
  List<Coord> validNeighbors = [];

  for (Coord dir in directions) {
    Coord neighbor = coord.add(dir);
    if (isValidNeighborFunc(grid, neighbor, dir)) {
      validNeighbors.add(neighbor);
    }
  }

  return validNeighbors;
}

Graph getGraph(Grid grid, Coord start, Coord end, bool Function(Grid, Coord, Coord) isValidNeighborFunc) {
  Set<Coord> vertices = {start, end};
  Map<Coord, Set<Edge>> edges = {};

  for (int y = 0; y < grid.height; y++) {
    for (int x = 0; x < grid.width; x++) {
      Coord coord = Coord(x, y);
      if (!grid.data.containsKey(coord)) {
        if (neighbors4(grid, coord, isValidNeighbor).length > 2) {
          vertices.add(coord);
        }
      }
    }
  }

  for (Coord start in vertices) {
    Set<Edge> edgeSet = getEdgesBFS(grid, start, vertices, isValidNeighborFunc);
    edges[start] = edgeSet;
  }

  return Graph(vertices, edges);
}

Set<Edge> getEdgesBFS(Grid grid, Coord start, Set<Coord> vertices, bool Function(Grid, Coord, Coord) isValidNeighborFunc) {
  List<Coord> frontier = [start];
  Set<Coord> reached = {start};
  Map<Coord, int> distances = {start: 0};
  Set<Edge> edges = {};

  while (frontier.isNotEmpty) {
    Coord current = frontier.removeAt(0);

    if (vertices.contains(current) && current != start) {
      Edge edge = Edge(start, current, distances[current]!);
      edges.add(edge);
      continue;
    }

    for (Coord next in neighbors4(grid, current, isValidNeighborFunc)) {
      if (!reached.contains(next)) {
        frontier.add(next);
        reached.add(next);
        distances[next] = (distances[current] ?? 0) + 1;
      }
    }
  }

  return edges;
}

int getMaxDistanceDFS(Grid grid, Graph graph, Coord current, Coord end, Set<Coord> seen) {
  if (current == end) {
    return 0;
  }

  int maxi = 0;
  seen.add(current);
  for (Edge edge in graph.edges[current] ?? {}) {
    if (!seen.contains(edge.end)) {
      int dist = getMaxDistanceDFS(grid, graph, edge.end, end, seen);
      maxi = dist + edge.weight > maxi ? dist + edge.weight : maxi;
    }
  }
  seen.remove(current);

  return maxi;
}

int solve(List<String> input) {
  Grid grid = parseInput(input);

  Coord start = Coord(1, 0);
  Coord end = Coord(grid.width - 2, grid.height - 1);

  Graph graph = getGraph(grid, start, end, isValidNeighborWithSlopes);

  return getMaxDistanceDFS(grid, graph, start, end, {});
}

List<String> readFile(String fileName) {
  File file = File(fileName);
  List<String> lines = file.readAsLinesSync();
  return lines;
}

void main() {
  List<String> input = readFile("input.txt");
  print(solve(input));
}