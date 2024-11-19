
import 'dart:io';

class Coord {
  int x;
  int y;

  Coord(this.x, this.y);

  Coord add(Coord other) => Coord(x + other.x, y + other.y);
  Coord subtract(Coord other) => Coord(x - other.x, y - other.y);
  Coord opposite() => Coord(-x, -y);

  @override
  bool operator ==(Object other) =>
      other is Coord && other.x == x && other.y == y;

  @override
  int get hashCode => x.hashCode ^ y.hashCode;
}

typedef Tile = String;
typedef Pipe = Set<Coord>;

class Grid {
  int width;
  int height;
  Map<Coord, Tile> data;

  Grid(this.width, this.height, this.data);

  static Grid build(List<String> input) {
    final data = <Coord, Tile>{};
    for (int y = 0; y < input.length; y++) {
      for (int x = 0; x < input[y].length; x++) {
        final tile = input[y][x];
        if (tile != '.') {
          data[Coord(x, y)] = tile;
        }
      }
    }
    return Grid(input[0].length, input.length, data);
  }

  Coord findStart() {
    for (final entry in data.entries) {
      if (entry.value == 'S') {
        return entry.key;
      }
    }
    throw Exception('Start not found');
  }

  Pipe getPipeFromNeighbors(Coord coord) {
    final pipe = <Coord>{};
    final neighbors = {
      Coord(0, -1): coord.add(Coord(0, -1)),
      Coord(1, 0): coord.add(Coord(1, 0)),
      Coord(0, 1): coord.add(Coord(0, 1)),
      Coord(-1, 0): coord.add(Coord(-1, 0)),
    };

    for (final entry in neighbors.entries) {
      final dir = entry.key;
      final neighborCoord = entry.value;
      final neighborTile = data[neighborCoord];
      if (neighborTile != null) {
        final neighborPipe = getPipeFromTile(neighborTile);
        if (neighborPipe.contains(dir.opposite())) {
          pipe.add(dir);
        }
      }
    }

    return pipe;
  }

  List<Coord> pathFinding(Coord start) {
    final path = <Coord>[start];
    final startPipe = getPipeFromNeighbors(start);
    Coord previousDir = startPipe.first;
    Coord current = start.add(previousDir);

    while (current != start) {
      path.add(current);
      final currentPipe = getPipeFromTile(data[current]!);
      for (final dir in currentPipe) {
        if (dir != previousDir.opposite()) {
          previousDir = dir;
          current = current.add(dir);
          break;
        }
      }
    }

    return path;
  }

  static Pipe getPipeFromTile(Tile tile) {
    switch (tile) {
      case '|':
        return {Coord(0, -1), Coord(0, 1)};
      case '-':
        return {Coord(-1, 0), Coord(1, 0)};
      case 'J':
        return {Coord(0, -1), Coord(-1, 0)};
      case 'L':
        return {Coord(0, -1), Coord(1, 0)};
      case '7':
        return {Coord(0, 1), Coord(-1, 0)};
      case 'F':
        return {Coord(0, 1), Coord(1, 0)};
      default:
        return {};
    }
  }
}

void main() async {
  final input = await File('input.txt').readAsLines();
  final grid = Grid.build(input);
  final start = grid.findStart();
  final path = grid.pathFinding(start);
  final maxLength = path.length ~/ 2;
  print(maxLength);
}
