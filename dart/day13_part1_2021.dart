
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

void main() async {
  final input = await File('input.txt').readAsLines();
  final points = <Point>{};
  final folds = <String>[];
  var readingPoints = true;

  for (var line in input) {
    if (line.isEmpty) {
      readingPoints = false;
      continue;
    }
    if (readingPoints) {
      final coords = line.split(',');
      points.add(Point(int.parse(coords[0]), int.parse(coords[1])));
    } else {
      folds.add(line);
    }
  }

  final fold = folds[0].split(' ')[2].split('=');
  final axis = fold[0];
  final value = int.parse(fold[1]);
  final newPoints = <Point>{};

  for (var point in points) {
    if (axis == 'x' && point.x > value) {
      point = Point(2 * value - point.x, point.y);
    } else if (axis == 'y' && point.y > value) {
      point = Point(point.x, 2 * value - point.y);
    }
    newPoints.add(point);
  }

  print(newPoints.length);
}
