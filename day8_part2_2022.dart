import 'dart:io';

void main() {
  var Neighbors4 = [
    [0, 1],
    [0, -1],
    [1, 0],
    [-1, 0]
  ];

  var grid = {};
  var lines = File('input.txt').readAsLinesSync();
  var y = 0;

  for (var line in lines) {
    for (var x = 0; x < line.length; x++) {
      grid[Point(x, y)] = int.parse(line[x]);
    }
    y++;
  }

  var maxScore = 0;
  for (var p in grid.keys) {
    var score = 1;
    for (var n in Neighbors4) {
      var next = p;
      var view = 0;
      while (true) {
        next = Point(next.x + n[0], next.y + n[1]);
        if (grid.containsKey(next)) {
          view++;
          if (grid[next] >= grid[p]) {
            score *= view;
            break;
          }
        } else {
          score *= view;
          break;
        }
      }
    }

    if (score > maxScore) {
      maxScore = score;
    }
  }
  print(maxScore);
}

class Point {
  final int x;
  final int y;

  Point(this.x, this.y);

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is Point && runtimeType == other.runtimeType && x == other.x && y == other.y;

  @override
  int get hashCode => x.hashCode ^ y.hashCode;
}