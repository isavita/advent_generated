import 'dart:io';

void main() {
  var file = File('input.txt');
  var lines = file.readAsLinesSync();

  var heightmap = <List<int>>[];
  for (var line in lines) {
    var row = <int>[];
    for (var char in line.runes) {
      var height = int.parse(String.fromCharCode(char));
      row.add(height);
    }
    heightmap.add(row);
  }

  var totalRiskLevel = 0;
  for (var y = 0; y < heightmap.length; y++) {
    for (var x = 0; x < heightmap[y].length; x++) {
      if (isLowPoint(heightmap, x, y)) {
        totalRiskLevel += 1 + heightmap[y][x];
      }
    }
  }

  print(totalRiskLevel);
}

bool isLowPoint(List<List<int>> heightmap, int x, int y) {
  var height = heightmap[y][x];
  if (x > 0 && heightmap[y][x - 1] <= height) {
    return false;
  }
  if (x < heightmap[y].length - 1 && heightmap[y][x + 1] <= height) {
    return false;
  }
  if (y > 0 && heightmap[y - 1][x] <= height) {
    return false;
  }
  if (y < heightmap.length - 1 && heightmap[y + 1][x] <= height) {
    return false;
  }
  return true;
}