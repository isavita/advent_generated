
import 'dart:io';

class Coordinate {
  int x, y, z;
  Coordinate(this.x, this.y, this.z);

  @override
  bool operator ==(Object other) =>
      other is Coordinate && x == other.x && y == other.y && z == other.z;

  @override
  int get hashCode => Object.hash(x, y, z);
}

void main() async {
  var input = await File('input.txt').readAsString();
  var initialState = input.trim().split('\n');
  var activeCubes = <Coordinate>{};

  for (var y = 0; y < initialState.length; y++) {
    for (var x = 0; x < initialState[y].length; x++) {
      if (initialState[y][x] == '#') {
        activeCubes.add(Coordinate(x, y, 0));
      }
    }
  }

  for (var cycle = 0; cycle < 6; cycle++) {
    activeCubes = simulateCycle(activeCubes);
  }

  print(activeCubes.length);
}

Set<Coordinate> simulateCycle(Set<Coordinate> activeCubes) {
  var newActiveCubes = <Coordinate>{};
  var neighborCounts = <Coordinate, int>{};

  for (var coord in activeCubes) {
    for (var dz = -1; dz <= 1; dz++) {
      for (var dy = -1; dy <= 1; dy++) {
        for (var dx = -1; dx <= 1; dx++) {
          if (dz == 0 && dy == 0 && dx == 0) continue;
          var neighbor = Coordinate(coord.x + dx, coord.y + dy, coord.z + dz);
          neighborCounts[neighbor] = (neighborCounts[neighbor] ?? 0) + 1;
        }
      }
    }
  }

  for (var entry in neighborCounts.entries) {
    var coord = entry.key;
    var count = entry.value;
    if (count == 3 || (count == 2 && activeCubes.contains(coord))) {
      newActiveCubes.add(coord);
    }
  }

  return newActiveCubes;
}
