
import 'dart:io';

void main() {
  final lines = File('input.txt').readAsLinesSync();
  final grid = parseInput(lines);
  final minY = grid.keys.map((p) => p.y).reduce((a, b) => a < b ? a : b);
  final maxY = grid.keys.map((p) => p.y).reduce((a, b) => a > b ? a : b);

  fill(grid, Point(500, 0), minY, maxY);

  int reachable = 0;
  int retained = 0;
  for (final p in grid.keys) {
    if (p.y >= minY && p.y <= maxY) {
      if (grid[p] == '~' || grid[p] == '|') {
        reachable++;
      }
      if (grid[p] == '~') {
        retained++;
      }
    }
  }

  print(reachable);
  print(retained);
}

class Point {
  final int x, y;
  Point(this.x, this.y);

  @override
  bool operator ==(Object other) =>
      other is Point && x == other.x && y == other.y;

  @override
  int get hashCode => x.hashCode ^ y.hashCode;

  Point operator +(Point other) => Point(x + other.x, y + other.y);

  @override
  String toString() => '($x, $y)';
}

Map<Point, String> parseInput(List<String> lines) {
  final grid = <Point, String>{};
  for (final line in lines) {
    final parts = line.split(', ');
    final first = parts[0].substring(2).split('..');
    final second = parts[1].substring(2).split('..');
    if (parts[0][0] == 'x') {
      final x = int.parse(first[0]);
      final yStart = int.parse(second[0]);
      final yEnd = int.parse(second.length > 1 ? second[1] : second[0]);
      for (int y = yStart; y <= yEnd; y++) {
        grid[Point(x, y)] = '#';
      }
    } else {
      final y = int.parse(first[0]);
      final xStart = int.parse(second[0]);
      final xEnd = int.parse(second.length > 1 ? second[1] : second[0]);
      for (int x = xStart; x <= xEnd; x++) {
        grid[Point(x, y)] = '#';
      }
    }
  }
  return grid;
}

void fill(Map<Point, String> grid, Point start, int minY, int maxY) {
  if (start.y > maxY) return;
  if (grid.containsKey(start)) return;

  grid[start] = '|';

  final below = start + Point(0, 1);
  fill(grid, below, minY, maxY);

  if (grid[below] == '#' || grid[below] == '~') {
    final left = start + Point(-1, 0);
    final right = start + Point(1, 0);
    fill(grid, left, minY, maxY);
    fill(grid, right, minY, maxY);

    if (isBounded(grid, start)) {
      settle(grid, start);
    }
  }
}

bool isBounded(Map<Point, String> grid, Point p) {
  bool leftWall = false;
  bool rightWall = false;
  
  for (int x = p.x - 1; x >= 0; x--) {
    final current = Point(x, p.y);
    final below = current + Point(0, 1);
    if (!grid.containsKey(below) || grid[below] == '|') {
      return false;
    }
    if (grid[current] == '#') {
      leftWall = true;
      break;
    }
  }

  for (int x = p.x + 1; ; x++) {
    final current = Point(x, p.y);
    final below = current + Point(0, 1);
    if (!grid.containsKey(below) || grid[below] == '|') {
      return false;
    }
    if (grid[current] == '#') {
      rightWall = true;
      break;
    }
  }
  return leftWall && rightWall;
}

void settle(Map<Point, String> grid, Point p) {
  grid[p] = '~';
  for (int x = p.x - 1; ; x--) {
    final current = Point(x, p.y);
    if (grid[current] == '#') break;
    grid[current] = '~';
  }
  for (int x = p.x + 1; ; x++) {
    final current = Point(x, p.y);
    if (grid[current] == '#') break;
    grid[current] = '~';
  }
}
