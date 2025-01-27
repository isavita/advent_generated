
import 'dart:io';
import 'dart:math';

class Coord {
  int x, y;
  Coord(this.x, this.y);
  Coord add(Coord other) => Coord(x + other.x, y + other.y);
  @override
  bool operator ==(Object other) =>
      other is Coord && x == other.x && y == other.y;
  @override
  int get hashCode => x.hashCode ^ y.hashCode;
}

class Grid {
  int width, height;
  Map<Coord, String> data;
  Grid(this.width, this.height, this.data);
}

final north = Coord(0, -1);
final south = Coord(0, 1);
final west = Coord(-1, 0);
final east = Coord(1, 0);

const empty = '.';
const wall = '#';
const northSlopes = '^';
const southSlopes = 'v';
const westSlopes = '<';
const eastSlopes = '>';

final slopeToDir = {
  northSlopes: north,
  southSlopes: south,
  westSlopes: west,
  eastSlopes: east,
};

class Edge {
  Coord start, end;
  int weight;
  Edge(this.start, this.end, this.weight);
}

class Graph {
  Set<Coord> vertices;
  Map<Coord, Set<Edge>> edges;
  Graph(this.vertices, this.edges);
}

bool isInBounds(Grid grid, Coord coord) =>
    0 <= coord.x && coord.x < grid.width && 0 <= coord.y && coord.y < grid.height;

Grid parseInput(List<String> input) {
  final width = input[0].length;
  final height = input.length;
  final data = <Coord, String>{};
  for (var y = 0; y < height; y++) {
    for (var x = 0; x < width; x++) {
      final char = input[y][x];
      if (char != empty) {
        data[Coord(x, y)] = char;
      }
    }
  }
  return Grid(width, height, data);
}

bool isValidNeighbor(Grid grid, Coord coord, Coord dir) =>
    isInBounds(grid, coord) && grid.data[coord] != wall;

List<Coord> neighbors4(Grid grid, Coord coord) {
  final directions = [north, south, west, east];
  final validNeighbors = <Coord>[];
  for (final dir in directions) {
    final neighbor = coord.add(dir);
    if (isValidNeighbor(grid, neighbor, dir)) {
      validNeighbors.add(neighbor);
    }
  }
  return validNeighbors;
}

Graph getGraph(Grid grid, Coord start, Coord end) {
  final vertices = {start, end};
  final edges = <Coord, Set<Edge>>{};
  for (var y = 0; y < grid.height; y++) {
    for (var x = 0; x < grid.width; x++) {
      final coord = Coord(x, y);
      if (!grid.data.containsKey(coord) && neighbors4(grid, coord).length > 2) {
        vertices.add(coord);
      }
    }
  }
  for (final startVertex in vertices) {
    edges[startVertex] = getEdgesBFS(grid, startVertex, vertices);
  }
  return Graph(vertices, edges);
}

Set<Edge> getEdgesBFS(Grid grid, Coord start, Set<Coord> vertices) {
  final frontier = [start];
  final reached = {start};
  final distances = {start: 0};
  final edges = <Edge>{};
  while (frontier.isNotEmpty) {
    final current = frontier.removeAt(0);
    if (vertices.contains(current) && current != start) {
      edges.add(Edge(start, current, distances[current]!));
      continue;
    }
    for (final next in neighbors4(grid, current)) {
      if (!reached.contains(next)) {
        frontier.add(next);
        reached.add(next);
        distances[next] = distances[current]! + 1;
      }
    }
  }
  return edges;
}

int getMaxDistanceDFS(Graph graph, Coord current, Coord end, Set<Coord> seen) {
  if (current == end) {
    return 0;
  }
  var maxi = 0;
  seen.add(current);
  for (final edge in graph.edges[current]!) {
    if (!seen.contains(edge.end)) {
      final dist = getMaxDistanceDFS(graph, edge.end, end, seen);
      if (dist >= 0) {
        maxi = max(maxi, dist + edge.weight);
      }
    }
  }
  seen.remove(current);
  return maxi > 0 ? maxi : -1;
}

int solve(List<String> input) {
  final grid = parseInput(input);
  final start = Coord(1, 0);
  final end = Coord(grid.width - 2, grid.height - 1);
  final graph = getGraph(grid, start, end);
  return getMaxDistanceDFS(graph, start, end, {});
}

List<String> readFile(String fileName) {
  final file = File(fileName);
  return file.readAsLinesSync();
}

void main() {
  final input = readFile('input.txt');
  print(solve(input));
}
