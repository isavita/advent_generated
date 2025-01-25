
import 'dart:io';
import 'dart:collection';

class Point {
  int x;
  int y;

  Point(this.x, this.y);

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is Point &&
          runtimeType == other.runtimeType &&
          x == other.x &&
          y == other.y;

  @override
  int get hashCode => x.hashCode ^ y.hashCode;

  @override
  String toString() {
    return '($x,$y)';
  }
}

void main() {
  final input = File('input.txt').readAsLinesSync();
  final bytes = input.map((line) {
    final parts = line.split(',');
    return Point(int.parse(parts[0]), int.parse(parts[1]));
  }).toList();

  // Part 1
  final gridPart1 = createGrid();
  for (int i = 0; i < 1024 && i < bytes.length; ++i) {
    markCorrupted(gridPart1, bytes[i]);
  }
  final pathLength = findShortestPath(gridPart1);
  print(pathLength);

  // Part 2
  for (int i = 1; i <= bytes.length; ++i) {
    final gridPart2 = createGrid();
    for (int j = 0; j < i; ++j) {
      markCorrupted(gridPart2, bytes[j]);
    }
    if (findShortestPath(gridPart2) == -1) {
      print('${bytes[i - 1].x},${bytes[i - 1].y}');
      break;
    }
  }
}

List<List<String>> createGrid() {
  return List.generate(71, (_) => List.filled(71, '.'));
}

void markCorrupted(List<List<String>> grid, Point p) {
  if (p.x >= 0 && p.x < 71 && p.y >= 0 && p.y < 71) {
    grid[p.y][p.x] = '#';
  }
}

int findShortestPath(List<List<String>> grid) {
  final queue = Queue<Tuple>();
  queue.add(Tuple(Point(0, 0), 0));
  final visited = <Point>{Point(0, 0)};

  while (queue.isNotEmpty) {
    final current = queue.removeFirst();
    final p = current.point;
    final dist = current.distance;

    if (p.x == 70 && p.y == 70) {
      return dist;
    }

    final neighbors = [
      Point(p.x + 1, p.y),
      Point(p.x - 1, p.y),
      Point(p.x, p.y + 1),
      Point(p.x, p.y - 1),
    ];

    for (final neighbor in neighbors) {
      if (isValid(grid, neighbor) && !visited.contains(neighbor)) {
        visited.add(neighbor);
        queue.add(Tuple(neighbor, dist + 1));
      }
    }
  }
  return -1;
}

bool isValid(List<List<String>> grid, Point p) {
  return p.x >= 0 &&
      p.x < 71 &&
      p.y >= 0 &&
      p.y < 71 &&
      grid[p.y][p.x] == '.';
}

class Tuple {
  Point point;
  int distance;
  Tuple(this.point, this.distance);
}
