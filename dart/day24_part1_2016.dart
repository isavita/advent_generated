
import 'dart:io';
import 'dart:collection';
import 'dart:math';

void main() {
  final grid = readFile('input.txt');
  final locations = findLocations(grid);
  final distances = calculateDistances(grid, locations);
  final minSteps = findShortestPath(distances, locations);
  print(minSteps);
}

List<String> readFile(String filePath) {
  return File(filePath).readAsLinesSync();
}

Map<int, Point<int>> findLocations(List<String> grid) {
  final locations = <int, Point<int>>{};
  for (int r = 0; r < grid.length; r++) {
    for (int c = 0; c < grid[r].length; c++) {
      final char = grid[r][c];
      if (RegExp(r'[0-9]').hasMatch(char)) {
        locations[int.parse(char)] = Point(r, c);
      }
    }
  }
  return locations;
}

Map<Point<int>, Map<Point<int>, int>> calculateDistances(
    List<String> grid, Map<int, Point<int>> locations) {
  final distanceMap = <Point<int>, Map<Point<int>, int>>{};
  final locationPoints = locations.values.toList();

  for (int i = 0; i < locationPoints.length; i++) {
    distanceMap[locationPoints[i]] = {};
    for (int j = 0; j < locationPoints.length; j++) {
      if (i == j) {
        distanceMap[locationPoints[i]]![locationPoints[j]] = 0;
      } else {
        distanceMap[locationPoints[i]]![locationPoints[j]] =
            bfs(grid, locationPoints[i], locationPoints[j]);
      }
    }
  }
  return distanceMap;
}

int bfs(List<String> grid, Point<int> start, Point<int> end) {
  final rows = grid.length;
  final cols = grid[0].length;
  final queue = Queue<Point<int>>();
  queue.add(start);
  final visited = <Point<int>>{start};
  final distance = <Point<int>, int>{start: 0};

  while (queue.isNotEmpty) {
    final current = queue.removeFirst();
    if (current == end) {
      return distance[current]!;
    }

    final r = current.x;
    final c = current.y;

    final neighbors = [
      Point(r + 1, c),
      Point(r - 1, c),
      Point(r, c + 1),
      Point(r, c - 1),
    ];

    for (final neighbor in neighbors) {
      if (neighbor.x >= 0 &&
          neighbor.x < rows &&
          neighbor.y >= 0 &&
          neighbor.y < cols &&
          grid[neighbor.x][neighbor.y] != '#' &&
          !visited.contains(neighbor)) {
        visited.add(neighbor);
        distance[neighbor] = distance[current]! + 1;
        queue.add(neighbor);
      }
    }
  }
  return -1; // No path found
}

int findShortestPath(
    Map<Point<int>, Map<Point<int>, int>> distances, Map<int, Point<int>> locations) {
  final nonZeroLocations = locations.keys.where((location) => location != 0).toList();
  if (nonZeroLocations.isEmpty) return 0;

  int minDistance = double.maxFinite.toInt();
  final startLocation = locations[0]!;
  final permutations = permute(nonZeroLocations);

  for (final permutation in permutations) {
    int currentDistance = 0;
    Point<int> currentLocation = startLocation;

    for (final locationNumber in permutation) {
      final nextLocation = locations[locationNumber]!;
      currentDistance += distances[currentLocation]![nextLocation]!;
      currentLocation = nextLocation;
    }
    minDistance = min(minDistance, currentDistance);
  }
  return minDistance;
}

List<List<int>> permute(List<int> items) {
  if (items.isEmpty) return [[]];
  if (items.length == 1) return [items];

  List<List<int>> result = [];
  for (int i = 0; i < items.length; i++) {
    int currentItem = items[i];
    List<int> remainingItems = items.sublist(0, i) + items.sublist(i + 1);
    List<List<int>> subPermutations = permute(remainingItems);
    for (List<int> permutation in subPermutations) {
      result.add([currentItem, ...permutation]);
    }
  }
  return result;
}
