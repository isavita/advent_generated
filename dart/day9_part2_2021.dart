import 'dart:io';
import 'dart:math';

void main() {
  final heightmap = readHeightmap();
  final basinSizes = <int>[];
  final visited = <String>{};

  for (var y = 0; y < heightmap.length; y++) {
    for (var x = 0; x < heightmap[y].length; x++) {
      if (isLowPoint(heightmap, x, y)) {
        final size = exploreBasin(heightmap, x, y, visited);
        basinSizes.add(size);
      }
    }
  }

  basinSizes.sort((a, b) => b.compareTo(a));
  final result = basinSizes[0] * basinSizes[1] * basinSizes[2];
  print(result);
}

List<List<int>> readHeightmap() {
  final lines = File('input.txt').readAsLinesSync();
  return lines.map((line) => line.codeUnits.map((c) => c - '0'.codeUnits.first).toList()).toList();
}

bool isLowPoint(List<List<int>> heightmap, int x, int y) {
  final height = heightmap[y][x];
  if (x > 0 && heightmap[y][x - 1] <= height) return false;
  if (x < heightmap[y].length - 1 && heightmap[y][x + 1] <= height) return false;
  if (y > 0 && heightmap[y - 1][x] <= height) return false;
  if (y < heightmap.length - 1 && heightmap[y + 1][x] <= height) return false;
  return true;
}

int exploreBasin(List<List<int>> heightmap, int x, int y, Set<String> visited) {
  final key = '$x,$y';
  if (visited.contains(key) || heightmap[y][x] == 9) return 0;
  visited.add(key);
  var size = 1;

  final directions = [[-1, 0], [1, 0], [0, -1], [0, 1]];
  for (final dir in directions) {
    final newX = x + dir[0];
    final newY = y + dir[1];
    if (newX >= 0 && newX < heightmap[0].length && newY >= 0 && newY < heightmap.length) {
      size += exploreBasin(heightmap, newX, newY, visited);
    }
  }
  return size;
}