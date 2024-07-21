
import 'dart:io';

void main() async {
  // Read input from the file
  final input = await File('input.txt').readAsString();
  final int target = int.parse(input.trim());

  // Part 1: Calculate the Manhattan distance
  int distance = manhattanDistance(target);
  print('Manhattan Distance for $target: $distance');

  // Part 2: Find the first value larger than the target
  int firstLargerValue = firstValueLargerThan(target);
  print('First value larger than $target: $firstLargerValue');
}

int manhattanDistance(int number) {
  if (number == 1) return 0;

  // Find the layer of the spiral
  int layer = 0;
  while ((layer * layer) < number) {
    layer++;
  }
  layer--;

  // The maximum number in this layer
  int maxInLayer = (layer * layer) + (layer * 2);
  // The distance from the center of the layer
  int distanceToCenter = (maxInLayer - number).abs();
  // The layer's width
  int layerWidth = layer * 2;

  // The distance to the closest edge of the layer
  int edgeDistance = distanceToCenter % layerWidth;

  // The total Manhattan distance is layer + edgeDistance
  return layer + edgeDistance;
}

int firstValueLargerThan(int target) {
  // Create a map to hold the values
  Map<Point, int> spiral = {Point(0, 0): 1};
  int x = 0, y = 0;
  int step = 1;

  while (true) {
    // Move right
    for (int i = 0; i < step; i++) {
      x++;
      int value = calculateAdjacentSum(spiral, x, y);
      spiral[Point(x, y)] = value;
      if (value > target) return value;
    }
    // Move up
    for (int i = 0; i < step; i++) {
      y++;
      int value = calculateAdjacentSum(spiral, x, y);
      spiral[Point(x, y)] = value;
      if (value > target) return value;
    }
    step++;
    // Move left
    for (int i = 0; i < step; i++) {
      x--;
      int value = calculateAdjacentSum(spiral, x, y);
      spiral[Point(x, y)] = value;
      if (value > target) return value;
    }
    // Move down
    for (int i = 0; i < step; i++) {
      y--;
      int value = calculateAdjacentSum(spiral, x, y);
      spiral[Point(x, y)] = value;
      if (value > target) return value;
    }
    step++;
  }
}

int calculateAdjacentSum(Map<Point, int> spiral, int x, int y) {
  int sum = 0;
  for (int dx = -1; dx <= 1; dx++) {
    for (int dy = -1; dy <= 1; dy++) {
      if (dx == 0 && dy == 0) continue; // Skip the center point
      sum += spiral[Point(x + dx, y + dy)] ?? 0;
    }
  }
  return sum;
}

class Point {
  final int x, y;
  Point(this.x, this.y);

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is Point && runtimeType == other.runtimeType && x == other.x && y == other.y;

  @override
  int get hashCode => x.hashCode ^ y.hashCode;
}
