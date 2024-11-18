
import 'dart:io';
import 'dart:collection';

class Coord {
  final int x;
  final int y;

  const Coord(this.x, this.y);

  Coord operator +(Coord other) => Coord(x + other.x, y + other.y);
  Coord operator *(int scalar) => Coord(x * scalar, y * scalar);

  @override
  bool operator ==(Object other) => 
    other is Coord && x == other.x && y == other.y;

  @override
  int get hashCode => Object.hash(x, y);
}

class Grid {
  final int width;
  final int height;
  final Map<Coord, String> data;

  Grid(this.width, this.height, this.data);

  bool isInBounds(Coord coord) => 
    coord.x >= 0 && coord.x < width && coord.y >= 0 && coord.y < height;
}

class Solution {
  static final north = const Coord(0, -1);
  static final west = const Coord(-1, 0);
  static final south = const Coord(0, 1);
  static final east = const Coord(1, 0);

  static Grid parseInput(List<String> input) {
    Map<Coord, String> data = {};
    int width = input[0].length;
    int height = input.length;

    for (int y = 0; y < height; y++) {
      for (int x = 0; x < width; x++) {
        if (input[y][x] != '.') {
          data[Coord(x, y)] = input[y][x];
        }
      }
    }

    return Grid(width, height, data);
  }

  static Coord findStart(Grid grid) {
    return grid.data.keys.firstWhere(
      (coord) => grid.data[coord] == 'S', 
      orElse: () => throw Exception('No start found')
    );
  }

  static List<Coord> getNeighbors(Grid grid, Coord coord) {
    List<Coord> neighbors = [
      coord + north,
      coord + south,
      coord + east,
      coord + west
    ];

    return neighbors.where((neighbor) => 
      grid.isInBounds(neighbor) && 
      grid.data[neighbor] != '#'
    ).toList();
  }

  static Map<Coord, int> breadthFirstSearch(Grid grid, Coord start) {
    Queue<Coord> frontier = Queue()..add(start);
    Set<Coord> reached = {start};
    Map<Coord, int> distances = {start: 0};

    while (frontier.isNotEmpty) {
      Coord current = frontier.removeFirst();

      for (Coord next in getNeighbors(grid, current)) {
        if (!reached.contains(next)) {
          frontier.add(next);
          reached.add(next);
          distances[next] = distances[current]! + 1;
        }
      }
    }

    return distances;
  }

  static int solve(List<String> input, int numSteps) {
    Grid grid = parseInput(input);
    Coord start = findStart(grid);
    Map<Coord, int> distances = breadthFirstSearch(grid, start);

    return distances.values.where((dist) => 
      dist <= numSteps && dist % 2 == 0
    ).length;
  }
}

void main() {
  List<String> input = File('input.txt').readAsLinesSync();
  print(Solution.solve(input, 64));
}
