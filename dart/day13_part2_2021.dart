import 'dart:io';
import 'dart:collection';

void main() {
  final file = File('input.txt');
  final lines = file.readAsLinesSync();

  final points = HashSet<List<int>>();
  final folds = <List<int>>[];
  bool readingPoints = true;

  for (final line in lines) {
    if (line.isEmpty) {
      readingPoints = false;
      continue;
    }
    if (readingPoints) {
      final parts = line.split(',');
      final x = int.parse(parts[0]);
      final y = int.parse(parts[1]);
      points.add([x, y]);
    } else {
      final parts = line.split('=');
      final val = int.parse(parts[1]);
      if (parts[0].contains('x')) {
        folds.add([val, 0]);
      } else {
        folds.add([0, val]);
      }
    }
  }

  for (final fold in folds) {
    final newPoints = HashSet<List<int>>();
    for (final point in points) {
      final newPoint = [point[0], point[1]];
      if (fold[0] != 0 && point[0] > fold[0]) {
        newPoint[0] = fold[0] - (point[0] - fold[0]);
      } else if (fold[1] != 0 && point[1] > fold[1]) {
        newPoint[1] = fold[1] - (point[1] - fold[1]);
      }
      newPoints.add(newPoint);
    }
    points.clear();
    points.addAll(newPoints);
    if (folds.indexOf(fold) == 0) {
      print('Number of dots visible after first fold: ${points.length}');
    }
  }

  final maxX = points.map((p) => p[0]).reduce((a, b) => a > b ? a : b);
  final maxY = points.map((p) => p[1]).reduce((a, b) => a > b ? a : b);

  final grid = List.generate(maxY + 1, (_) => List.filled(maxX + 1, ' '));

  for (final point in points) {
    grid[point[1]][point[0]] = '#';
  }

  for (final row in grid) {
    print(row.join());
  }
}