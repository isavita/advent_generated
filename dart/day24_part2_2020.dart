
import 'dart:io';
import 'dart:collection';

class Coordinate {
  final int q;
  final int r;

  const Coordinate(this.q, this.r);

  @override
  bool operator ==(Object other) =>
      other is Coordinate && q == other.q && r == other.r;

  @override
  int get hashCode => q.hashCode ^ r.hashCode;
}

void main() {
  final directions = {
    'e': Coordinate(1, 0),
    'se': Coordinate(0, 1),
    'sw': Coordinate(-1, 1),
    'w': Coordinate(-1, 0),
    'nw': Coordinate(0, -1),
    'ne': Coordinate(1, -1),
  };

  final blackTiles = HashSet<Coordinate>();
  final file = File('input.txt');
  final lines = file.readAsLinesSync();

  for (final line in lines) {
    var coord = Coordinate(0, 0);
    for (var i = 0; i < line.length; i++) {
      String dir;
      if (line[i] == 'e' || line[i] == 'w') {
        dir = line[i];
      } else {
        dir = line.substring(i, i + 2);
        i++;
      }
      final move = directions[dir]!;
      coord = Coordinate(coord.q + move.q, coord.r + move.r);
    }
    
    if (blackTiles.contains(coord)) {
      blackTiles.remove(coord);
    } else {
      blackTiles.add(coord);
    }
  }

  for (var day = 0; day < 100; day++) {
    final tilesToCheck = HashSet<Coordinate>();
    for (final tile in blackTiles) {
      tilesToCheck.add(tile);
      tilesToCheck.addAll(_getNeighbors(tile));
    }

    final newBlackTiles = HashSet<Coordinate>();
    for (final tile in tilesToCheck) {
      final blackNeighborCount = _getNeighbors(tile)
          .where((neighbor) => blackTiles.contains(neighbor))
          .length;

      if ((blackTiles.contains(tile) && (blackNeighborCount == 1 || blackNeighborCount == 2)) ||
          (!blackTiles.contains(tile) && blackNeighborCount == 2)) {
        newBlackTiles.add(tile);
      }
    }

    blackTiles.clear();
    blackTiles.addAll(newBlackTiles);
  }

  print(blackTiles.length);
}

Iterable<Coordinate> _getNeighbors(Coordinate tile) sync* {
  final directions = [
    Coordinate(1, 0),
    Coordinate(0, 1),
    Coordinate(-1, 1),
    Coordinate(-1, 0),
    Coordinate(0, -1),
    Coordinate(1, -1),
  ];

  for (final dir in directions) {
    yield Coordinate(tile.q + dir.q, tile.r + dir.r);
  }
}
