
import 'dart:io';
import 'dart:math';

class Point {
  int row;
  int col;

  Point(this.row, this.col);

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is Point && row == other.row && col == other.col;

  @override
  int get hashCode => row.hashCode ^ col.hashCode;

  @override
  String toString() {
    return '($row, $col)';
  }
}

int solvePart1(List<String> map, Map<int, Point> locations) {
  final numLocations = locations.length;
  final distances = <int, Map<int, int>>{};

  for (final startLoc in locations.keys) {
    distances[startLoc] = {};
    for (final endLoc in locations.keys) {
      if (startLoc == endLoc) {
        distances[startLoc]![endLoc] = 0;
        continue;
      }
      distances[startLoc]![endLoc] =
          bfs(map, locations[startLoc]!, locations[endLoc]!);
    }
  }

  final otherLocations = locations.keys.where((loc) => loc != 0).toList();
  int minPath = double.maxFinite.toInt();

  void findShortestPath(List<int> currentPath, int currentLocation, int currentDistance) {
    if (currentPath.length == otherLocations.length) {
      minPath = min(minPath, currentDistance);
      return;
    }

    for (final nextLocation in otherLocations) {
      if (!currentPath.contains(nextLocation)) {
        final newPath = List<int>.from(currentPath)..add(nextLocation);
        findShortestPath(
          newPath,
          nextLocation,
          currentDistance + distances[currentLocation]![nextLocation]!,
        );
      }
    }
  }

  findShortestPath([], 0, 0);
  return minPath;
}

int solvePart2(List<String> map, Map<int, Point> locations) {
  final numLocations = locations.length;
  final distances = <int, Map<int, int>>{};

  for (final startLoc in locations.keys) {
    distances[startLoc] = {};
    for (final endLoc in locations.keys) {
      if (startLoc == endLoc) {
        distances[startLoc]![endLoc] = 0;
        continue;
      }
      distances[startLoc]![endLoc] =
          bfs(map, locations[startLoc]!, locations[endLoc]!);
    }
  }

  final otherLocations = locations.keys.where((loc) => loc != 0).toList();
  int minPath = double.maxFinite.toInt();

  void findShortestPath(List<int> currentPath, int currentLocation, int currentDistance) {
    if (currentPath.length == otherLocations.length) {
      final returnDistance = distances[currentLocation]![0]!;
      minPath = min(minPath, currentDistance + returnDistance);
      return;
    }

    for (final nextLocation in otherLocations) {
      if (!currentPath.contains(nextLocation)) {
        final newPath = List<int>.from(currentPath)..add(nextLocation);
        findShortestPath(
          newPath,
          nextLocation,
          currentDistance + distances[currentLocation]![nextLocation]!,
        );
      }
    }
  }

  findShortestPath([], 0, 0);
  return minPath;
}

int bfs(List<String> map, Point start, Point end) {
  final rows = map.length;
  final cols = map[0].length;
  final queue = [(start, 0)];
  final visited = <Point>{start};
  final directions = [
    Point(0, 1),
    Point(0, -1),
    Point(1, 0),
    Point(-1, 0)
  ];

  while (queue.isNotEmpty) {
    final current = queue.removeAt(0);
    final currentPoint = current.$1;
    final currentDistance = current.$2;

    if (currentPoint == end) {
      return currentDistance;
    }

    for (final dir in directions) {
      final nextRow = currentPoint.row + dir.row;
      final nextCol = currentPoint.col + dir.col;
      final nextPoint = Point(nextRow, nextCol);

      if (nextRow >= 0 &&
          nextRow < rows &&
          nextCol >= 0 &&
          nextCol < cols &&
          map[nextRow][nextCol] != '#' &&
          !visited.contains(nextPoint)) {
        visited.add(nextPoint);
        queue.add((nextPoint, currentDistance + 1));
      }
    }
  }
  return -1; // Should not reach here in this problem
}

void main() {
  final map = File('input.txt').readAsLinesSync();
  final locations = <int, Point>{};

  for (int i = 0; i < map.length; i++) {
    for (int j = 0; j < map[i].length; j++) {
      if (RegExp(r'\d').hasMatch(map[i][j])) {
        locations[int.parse(map[i][j])] = Point(i, j);
      }
    }
  }

  final part1Result = solvePart1(map, locations);
  print('Part 1: $part1Result');

  final part2Result = solvePart2(map, locations);
  print('Part 2: $part2Result');
}
