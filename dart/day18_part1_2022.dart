import 'dart:io';

class Point {
  int x, y, z;

  Point(this.x, this.y, this.z);

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is Point && runtimeType == other.runtimeType && other.x == x && other.y == y && other.z == z;

  @override
  int get hashCode => x.hashCode ^ y.hashCode ^ z.hashCode;
}

void main() {
  var file = File('input.txt');
  var lines = file.readAsLinesSync();

  var cubes = <Point, bool>{};

  for (var line in lines) {
    var coords = line.split(',');
    var x = int.parse(coords[0]);
    var y = int.parse(coords[1]);
    var z = int.parse(coords[2]);
    cubes[Point(x, y, z)] = true;
  }

  var surfaceArea = 0;
  for (var cube in cubes.keys) {
    surfaceArea += calculateExposedSides(cube, cubes);
  }

  print(surfaceArea);
}

int calculateExposedSides(Point p, Map<Point, bool> cubes) {
  var directions = [
    Point(1, 0, 0),
    Point(-1, 0, 0),
    Point(0, 1, 0),
    Point(0, -1, 0),
    Point(0, 0, 1),
    Point(0, 0, -1)
  ];

  var exposedSides = 6;
  for (var dir in directions) {
    var adjacent = Point(p.x + dir.x, p.y + dir.y, p.z + dir.z);
    if (cubes.containsKey(adjacent)) {
      exposedSides--;
    }
  }
  return exposedSides;
}