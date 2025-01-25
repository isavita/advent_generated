
import 'dart:io';
import 'dart:collection';

void main() {
  final input = File('input.txt').readAsLinesSync();
  final heightmap = input.map((line) => line.split('')).toList();

  Point start = Point(-1, -1);
  Point end = Point(-1, -1);

  for (int r = 0; r < heightmap.length; r++) {
    for (int c = 0; c < heightmap[r].length; c++) {
      if (heightmap[r][c] == 'S') {
        start = Point(r, c);
        heightmap[r][c] = 'a';
      } else if (heightmap[r][c] == 'E') {
        end = Point(r, c);
        heightmap[r][c] = 'z';
      }
    }
  }

  int part1Result = bfs(heightmap, start, end);
  print('Part 1: $part1Result');

  int part2Result = bfsPart2(heightmap, end);
  print('Part 2: $part2Result');
}

int bfs(List<List<String>> heightmap, Point start, Point end) {
  final rows = heightmap.length;
  final cols = heightmap[0].length;
  final distances = List.generate(rows, (_) => List.filled(cols, -1));
  final queue = Queue<Point>();

  distances[start.r][start.c] = 0;
  queue.add(start);

  while (queue.isNotEmpty) {
    final current = queue.removeFirst();
    if (current == end) {
      return distances[current.r][current.c];
    }

    final currentHeight = heightmap[current.r][current.c].codeUnitAt(0);
    final neighbors = [
      Point(current.r - 1, current.c),
      Point(current.r + 1, current.c),
      Point(current.r, current.c - 1),
      Point(current.r, current.c + 1),
    ];

    for (final neighbor in neighbors) {
      if (neighbor.r >= 0 && neighbor.r < rows && neighbor.c >= 0 && neighbor.c < cols) {
        final neighborHeight = heightmap[neighbor.r][neighbor.c].codeUnitAt(0);
        if (neighborHeight <= currentHeight + 1 && distances[neighbor.r][neighbor.c] == -1) {
          distances[neighbor.r][neighbor.c] = distances[current.r][current.c] + 1;
          queue.add(neighbor);
        }
      }
    }
  }
  return -1; // Should not reach here in this problem
}

int bfsPart2(List<List<String>> heightmap, Point end) {
  final rows = heightmap.length;
  final cols = heightmap[0].length;
  final distances = List.generate(rows, (_) => List.filled(cols, -1));
  final queue = Queue<Point>();

  distances[end.r][end.c] = 0;
  queue.add(end);

  int minSteps = -1;

  while (queue.isNotEmpty) {
    final current = queue.removeFirst();
    if (heightmap[current.r][current.c] == 'a') {
      if (minSteps == -1 || distances[current.r][current.c] < minSteps) {
        minSteps = distances[current.r][current.c];
      }
    }

    final currentHeight = heightmap[current.r][current.c].codeUnitAt(0);
    final neighbors = [
      Point(current.r - 1, current.c),
      Point(current.r + 1, current.c),
      Point(current.r, current.c - 1),
      Point(current.r, current.c + 1),
    ];

    for (final neighbor in neighbors) {
      if (neighbor.r >= 0 && neighbor.r < rows && neighbor.c >= 0 && neighbor.c < cols) {
        final neighborHeight = heightmap[neighbor.r][neighbor.c].codeUnitAt(0);
        if (currentHeight <= neighborHeight + 1 && distances[neighbor.r][neighbor.c] == -1) {
          distances[neighbor.r][neighbor.c] = distances[current.r][current.c] + 1;
          queue.add(neighbor);
        }
      }
    }
  }
  return minSteps;
}


class Point {
  int r, c;
  Point(this.r, this.c);

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is Point &&
          runtimeType == other.runtimeType &&
          r == other.r &&
          c == other.c;

  @override
  int get hashCode => r.hashCode ^ c.hashCode;
}
