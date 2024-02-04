
import 'dart:io';

class Point {
  int x, y;

  Point(this.x, this.y);

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is Point && runtimeType == other.runtimeType && x == other.x && y == other.y;

  @override
  int get hashCode => x.hashCode ^ y.hashCode;
}

void main() {
  var file = File('input.txt');
  var lines = file.readAsLinesSync();
  var wire1 = getPoints(lines[0]);
  var wire2 = getPoints(lines[1]);

  var intersections = <Point>{};
  for (var p in wire1.keys) {
    if (wire2.containsKey(p)) {
      intersections.add(p);
    }
  }

  var minDistance = 1 << 31;
  for (var p in intersections) {
    var distance = p.x.abs() + p.y.abs();
    if (distance < minDistance) {
      minDistance = distance;
    }
  }

  print(minDistance);
}

Map<Point, bool> getPoints(String path) {
  var points = <Point, bool>{};
  var current = Point(0, 0);
  for (var move in path.split(',')) {
    var dir = move[0];
    var steps = int.parse(move.substring(1));
    for (var i = 0; i < steps; i++) {
      switch (dir) {
        case 'U':
          current.y++;
          break;
        case 'D':
          current.y--;
          break;
        case 'L':
          current.x--;
          break;
        case 'R':
          current.x++;
          break;
      }
      points[Point(current.x, current.y)] = true;
    }
  }
  return points;
}
