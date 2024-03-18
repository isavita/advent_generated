import 'dart:io';
import 'dart:math';

const neighbors8 = [
  Point(0, 1),
  Point(0, -1),
  Point(1, 0),
  Point(-1, 0),
  Point(-1, -1),
  Point(-1, 1),
  Point(1, -1),
  Point(1, 1),
];

class Part {
  int xmin = 0, xmax = 0, y = 0, n = 0;

  bool valid(Map<Point<int>, int> grid) {
    for (int x = xmin; x <= xmax; x++) {
      for (var n in neighbors8) {
        int? c = grid[Point(x, y) + n];
        if (c != null && c != 46 && (c < 48 || c > 57)) {
          return true;
        }
      }
    }
    return false;
  }
}

void main() {
  final input = File('input.txt').readAsStringSync().trim().split('\n');

  final grid = <Point<int>, int>{};
  final parts = <Part>[];
  Part? curr;

  for (int y = 0; y < input.length; y++) {
    if (curr != null) {
      parts.add(curr);
      curr = null;
    }
    for (int x = 0; x < input[y].length; x++) {
      int c = input[y].codeUnitAt(x);
      grid[Point(x, y)] = c;
      if (c >= 48 && c <= 57) {
        if (curr == null) {
          curr = Part()
            ..y = y
            ..xmin = x
            ..xmax = x
            ..n = c - 48;
        } else {
          curr.n = curr.n * 10 + c - 48;
          curr.xmax = x;
        }
      } else if (curr != null) {
        parts.add(curr);
        curr = null;
      }
    }
  }

  final partsGrid = <Point<int>, int>{};
  for (int i = 0; i < parts.length; i++) {
    var p = parts[i];
    for (int x = p.xmin; x <= p.xmax; x++) {
      partsGrid[Point(x, p.y)] = i;
    }
  }

  int sum = 0;
  for (var p in grid.keys) {
    if (grid[p] == 42) {
      final neighborParts = <int>{};
      for (var n in neighbors8) {
        int? i = partsGrid[p + n];
        if (i != null) {
          neighborParts.add(i);
        }
      }
      if (neighborParts.length == 2) {
        int prod = 1;
        for (int i in neighborParts) {
          prod *= parts[i].n;
        }
        sum += prod;
      }
    }
  }
  print(sum);
}