
import 'dart:io';
import 'dart:math';

void main() async {
  // Read input from the file
  final lines = await File('input.txt').readAsLines();

  // Create a map to count overlaps
  final Map<Point<int>, int> ventMap = {};

  // Process each line segment
  for (var line in lines) {
    final points = line.split(' -> ');
    final start = _parsePoint(points[0]);
    final end = _parsePoint(points[1]);

    _markVentPoints(ventMap, start, end);
  }

  // Count the number of points with at least two overlaps
  final overlapCount = ventMap.values.where((count) => count >= 2).length;

  print(overlapCount);
}

// Parse a point from a string like "x,y"
Point<int> _parsePoint(String pointStr) {
  final coords = pointStr.split(',').map(int.parse).toList();
  return Point(coords[0], coords[1]);
}

// Mark points in the vent map based on the start and end points
void _markVentPoints(Map<Point<int>, int> ventMap, Point<int> start, Point<int> end) {
  final dx = end.x - start.x;
  final dy = end.y - start.y;

  final steps = max(dx.abs(), dy.abs());
  final xStep = dx == 0 ? 0 : dx ~/ steps;
  final yStep = dy == 0 ? 0 : dy ~/ steps;

  for (var i = 0; i <= steps; i++) {
    final point = Point(start.x + i * xStep, start.y + i * yStep);
    ventMap[point] = (ventMap[point] ?? 0) + 1;
  }
}
