import 'dart:io';
import 'dart:math';

void main() {
  final grid = <Point<int>, bool>{};
  final scanner = File('input.txt').readAsLinesSync().iterator;
  while (scanner.moveNext()) {
    final points = scanner.current.split(' -> ').map((p) {
      final xy = p.split(',').map(int.parse).toList();
      return Point(xy[0], xy[1]);
    }).toList();
    for (var i = 0; i < points.length - 1; i++) {
      final p1 = points[i];
      final p2 = points[i + 1];
      if (p1.x == p2.x) {
        for (var y = min(p1.y, p2.y); y <= max(p1.y, p2.y); y++) {
          grid[Point(p1.x, y)] = true;
        }
      } else {
        for (var x = min(p1.x, p2.x); x <= max(p1.x, p2.x); x++) {
          grid[Point(x, p1.y)] = true;
        }
      }
    }
  }

  print(fill(grid));
}

int fill(Map<Point<int>, bool> grid) {
  final floor = bounds(grid).bottom + 1;
  var sands = 0, firstFloorTouch = 0;
  var full = false;
  while (!full) {
    var sand = Point<int>(500, 0);
    var settled = false;
    while (!settled) {
      if (sand.y == floor) {
        if (firstFloorTouch == 0) {
          firstFloorTouch = sands;
        }
        grid[sand] = true;
        break;
      }
      final next = [
        Point<int>(sand.x, sand.y + 1),
        Point<int>(sand.x - 1, sand.y + 1),
        Point<int>(sand.x + 1, sand.y + 1),
      ].firstWhere((p) => !grid.containsKey(p), orElse: () => sand);
      if (next == sand) {
        grid[sand] = true;
        settled = true;
      } else {
        sand = next;
      }
    }
    sands++;
    full = grid.containsKey(Point<int>(500, 0));
  }
  return firstFloorTouch;
}

Rectangle<int> bounds(Map<Point<int>, bool> grid) {
  final points = grid.keys.toList();
  return Rectangle(
    points.map((p) => p.x).reduce(min),
    points.map((p) => p.y).reduce(min),
    points.map((p) => p.x).reduce(max) - points.map((p) => p.x).reduce(min) + 1,
    points.map((p) => p.y).reduce(max) - points.map((p) => p.y).reduce(min) + 1,
  );
}