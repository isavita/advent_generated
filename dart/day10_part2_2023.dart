
import 'dart:io';
import 'dart:math';

class Coord {
  int x, y;
  Coord(this.x, this.y);

  Coord add(Coord other) => Coord(x + other.x, y + other.y);
  Coord subtract(Coord other) => Coord(x - other.x, y - other.y);
  Coord opposite() => Coord(-x, -y);

  @override
  bool operator ==(Object other) => 
    other is Coord && x == other.x && y == other.y;

  @override
  int get hashCode => Object.hash(x, y);
}

enum Tile {
  empty('.'),
  start('S'),
  vertical('|'),
  horizontal('-'),
  topLeftCorner('J'),
  topRightCorner('L'),
  bottomLeftCorner('7'),
  bottomRightCorner('F'),
  enclosed('X');

  final String value;
  const Tile(this.value);
}

class Grid {
  int width, height;
  Map<Coord, Tile> data = {};

  Grid(this.width, this.height);

  static Grid fromInput(List<String> input) {
    var grid = Grid(input[0].length, input.length);
    for (var y = 0; y < input.length; y++) {
      for (var x = 0; x < input[y].length; x++) {
        var tile = Tile.values.firstWhere(
          (t) => t.value == input[y][x], 
          orElse: () => Tile.empty
        );
        if (tile != Tile.empty) {
          grid.data[Coord(x, y)] = tile;
        }
      }
    }
    return grid;
  }
}

class PipeMap {
  static final Map<Tile, Set<Coord>> tileToPipe = {
    Tile.vertical: {Coord(0, -1), Coord(0, 1)},
    Tile.horizontal: {Coord(-1, 0), Coord(1, 0)},
    Tile.topLeftCorner: {Coord(0, -1), Coord(-1, 0)},
    Tile.topRightCorner: {Coord(0, -1), Coord(1, 0)},
    Tile.bottomLeftCorner: {Coord(0, 1), Coord(-1, 0)},
    Tile.bottomRightCorner: {Coord(0, 1), Coord(1, 0)},
  };

  static Set<Coord> getPipeFromTile(Tile tile) => 
    tileToPipe[tile] ?? {};

  static Tile getTileFromPipe(Set<Coord> pipe) {
    return tileToPipe.entries.firstWhere(
      (entry) => _arePipesEqual(entry.value, pipe),
      orElse: () => MapEntry(Tile.empty, {})
    ).key;
  }

  static bool _arePipesEqual(Set<Coord> pipe1, Set<Coord> pipe2) {
    return pipe1.length == pipe2.length && 
           pipe1.every((dir) => pipe2.contains(dir));
  }
}

class PathFinder {
  static Coord findStart(Grid grid) {
    return grid.data.keys.firstWhere(
      (coord) => grid.data[coord] == Tile.start
    );
  }

  static Set<Coord> getPipeFromNeighbors(Coord c, Grid grid) {
    var pipe = <Coord>{};
    var neighbors = [
      Coord(0, -1), Coord(1, 0), Coord(0, 1), Coord(-1, 0)
    ];

    for (var dir in neighbors) {
      var neighborCoord = c.add(dir);
      var neighborPipe = PipeMap.getPipeFromTile(grid.data[neighborCoord] ?? Tile.empty);
      if (neighborPipe.contains(dir.opposite())) {
        pipe.add(dir);
      }
    }

    return pipe;
  }

  static List<Coord> findPath(Grid grid) {
    var start = findStart(grid);
    var path = [start];
    var startPipe = getPipeFromNeighbors(start, grid);

    var previousDir = startPipe.first;
    var current = start.add(previousDir);

    while (current != start) {
      path.add(current);
      var currentPipe = PipeMap.getPipeFromTile(grid.data[current] ?? Tile.empty);
      
      for (var dir in currentPipe) {
        if (dir != previousDir.opposite()) {
          previousDir = dir;
          current = current.add(dir);
          break;
        }
      }
    }

    return path;
  }
}

class InsideChecker {
  static bool isInside(Coord c, Grid pathGrid) {
    if (pathGrid.data.containsKey(c)) return false;

    var numPipeOnLeft = 0;
    var startPipe = Tile.empty;

    for (var x = 0; x < c.x; x++) {
      var coord = Coord(x, c.y);
      var v = pathGrid.data[coord];

      switch (v) {
        case Tile.vertical:
          numPipeOnLeft++;
          break;
        case Tile.topRightCorner:
          startPipe = Tile.topRightCorner;
          break;
        case Tile.bottomRightCorner:
          startPipe = Tile.bottomRightCorner;
          break;
        case Tile.topLeftCorner:
          if (startPipe == Tile.bottomRightCorner) {
            startPipe = Tile.empty;
            numPipeOnLeft++;
          } else if (startPipe == Tile.topRightCorner) {
            startPipe = Tile.empty;
          }
          break;
        case Tile.bottomLeftCorner:
          if (startPipe == Tile.topRightCorner) {
            startPipe = Tile.empty;
            numPipeOnLeft++;
          } else if (startPipe == Tile.bottomRightCorner) {
            startPipe = Tile.empty;
          }
          break;
        default:
          break;
      }
    }

    return numPipeOnLeft % 2 == 1;
  }
}

int solve(List<String> input) {
  var grid = Grid.fromInput(input);
  var path = PathFinder.findPath(grid);
  
  var pathGrid = Grid(grid.width, grid.height);
  for (var coord in path) {
    pathGrid.data[coord] = grid.data[coord] ?? Tile.empty;
  }
  
  var start = path.first;
  pathGrid.data[start] = PipeMap.getTileFromPipe(
    PathFinder.getPipeFromNeighbors(start, grid)
  );

  var cnt = 0;
  for (var y = 0; y < grid.height; y++) {
    for (var x = 0; x < grid.width; x++) {
      var c = Coord(x, y);
      if (InsideChecker.isInside(c, pathGrid)) {
        cnt++;
      }
    }
  }

  return cnt;
}

void main() {
  var input = File('input.txt').readAsLinesSync();
  print(solve(input));
}
