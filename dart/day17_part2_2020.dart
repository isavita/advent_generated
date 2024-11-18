
import 'dart:io';
import 'dart:collection';

class Coordinate4D {
  final int x, y, z, w;
  
  Coordinate4D(this.x, this.y, this.z, this.w);
  
  @override
  bool operator ==(Object other) =>
      other is Coordinate4D && x == other.x && y == other.y && z == other.z && w == other.w;
  
  @override
  int get hashCode => Object.hash(x, y, z, w);
}

void main() {
  var input = File('input.txt').readAsStringSync().trim().split('\n');
  var activeCubes = HashSet<Coordinate4D>();

  for (var y = 0; y < input.length; y++) {
    for (var x = 0; x < input[y].length; x++) {
      if (input[y][x] == '#') {
        activeCubes.add(Coordinate4D(x, y, 0, 0));
      }
    }
  }

  for (var _ = 0; _ < 6; _++) {
    activeCubes = simulateCycle4D(activeCubes);
  }

  print(activeCubes.length);
}

HashSet<Coordinate4D> simulateCycle4D(HashSet<Coordinate4D> activeCubes) {
  var neighborCounts = HashMap<Coordinate4D, int>();
  
  for (var coord in activeCubes) {
    for (var dw = -1; dw <= 1; dw++) {
      for (var dz = -1; dz <= 1; dz++) {
        for (var dy = -1; dy <= 1; dy++) {
          for (var dx = -1; dx <= 1; dx++) {
            if (dw == 0 && dz == 0 && dy == 0 && dx == 0) continue;
            
            var neighbor = Coordinate4D(
              coord.x + dx, 
              coord.y + dy, 
              coord.z + dz, 
              coord.w + dw
            );
            
            neighborCounts[neighbor] = (neighborCounts[neighbor] ?? 0) + 1;
          }
        }
      }
    }
  }

  var newActiveCubes = HashSet<Coordinate4D>();
  
  neighborCounts.forEach((coord, count) {
    if (count == 3 || (count == 2 && activeCubes.contains(coord))) {
      newActiveCubes.add(coord);
    }
  });

  return newActiveCubes;
}
