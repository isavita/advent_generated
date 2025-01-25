
import 'dart:io';
import 'dart:collection';

void main() {
  final grid = List.generate(71, (_) => List.filled(71, false)); // false: safe, true: corrupted
  final bytePositions = <(int, int)>[];

  try {
    final file = File('input.txt');
    final lines = file.readAsLinesSync();

    for (var i = 0; i < lines.length && i < 1024; i++) {
      final line = lines[i];
      final parts = line.split(',');
      if (parts.length == 2) {
        final x = int.tryParse(parts[0]);
        final y = int.tryParse(parts[1]);
        if (x != null && y != null && x >= 0 && x <= 70 && y >= 0 && y <= 70) {
          bytePositions.add((x, y));
          grid[y][x] = true; // Mark as corrupted
        }
      }
    }
  } catch (e) {
    print('Error reading input file: $e');
    return;
  }

  final pathLength = findShortestPath(grid);
  print(pathLength);
}

int findShortestPath(List<List<bool>> grid) {
  final queue = Queue<(int, int, int)>(); // (x, y, distance)
  queue.add((0, 0, 0));
  final visited = <(int, int)>{(0, 0)};

  while (queue.isNotEmpty) {
    final current = queue.removeFirst();
    final x = current.$1;
    final y = current.$2;
    final dist = current.$3;

    if (x == 70 && y == 70) {
      return dist;
    }

    final directions = [(0, 1), (0, -1), (1, 0), (-1, 0)]; // Down, Up, Right, Left

    for (final dir in directions) {
      final nextX = x + dir.$1;
      final nextY = y + dir.$2;

      if (nextX >= 0 && nextX <= 70 && nextY >= 0 && nextY <= 70 && !grid[nextY][nextX] && !visited.contains((nextX, nextY))) {
        visited.add((nextX, nextY));
        queue.add((nextX, nextY, dist + 1));
      }
    }
  }

  return -1; // No path found (should not happen in this problem as per problem description usually, but good to handle)
}
