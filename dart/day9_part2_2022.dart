
import 'dart:io';

void main() async {
  // Read input from the file
  final input = await File('input.txt').readAsLines();
  
  // Initialize the rope with 10 knots (head + 9 tails)
  List<Point> rope = List.generate(10, (_) => Point(0, 0));
  Set<Point> visited = {rope[0]}; // Set to track unique positions visited by the tail

  // Process each movement instruction
  for (String line in input) {
    final parts = line.split(' ');
    final direction = parts[0];
    final steps = int.parse(parts[1]);

    for (int step = 0; step < steps; step++) {
      // Move the head based on the direction
      switch (direction) {
        case 'R':
          rope[0] = Point(rope[0].x + 1, rope[0].y);
          break;
        case 'L':
          rope[0] = Point(rope[0].x - 1, rope[0].y);
          break;
        case 'U':
          rope[0] = Point(rope[0].x, rope[0].y + 1);
          break;
        case 'D':
          rope[0] = Point(rope[0].x, rope[0].y - 1);
          break;
      }

      // Update the positions of the rest of the knots
      for (int i = 1; i < rope.length; i++) {
        if (!rope[i].isTouching(rope[i - 1])) {
          rope[i] = rope[i].moveTowards(rope[i - 1]);
        }
      }

      // Add the position of the tail (last knot) to the visited set
      visited.add(rope.last);
    }
  }

  // Output the number of unique positions visited by the tail
  print('The tail visited ${visited.length} unique positions.');
}

class Point {
  final int x;
  final int y;

  Point(this.x, this.y);

  bool isTouching(Point other) {
    return (x - other.x).abs() <= 1 && (y - other.y).abs() <= 1;
  }

  Point moveTowards(Point target) {
    int newX = x;
    int newY = y;

    if (x < target.x) newX++;
    if (x > target.x) newX--;
    if (y < target.y) newY++;
    if (y > target.y) newY--;

    return Point(newX, newY);
  }

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is Point && runtimeType == other.runtimeType && x == other.x && y == other.y;

  @override
  int get hashCode => x.hashCode ^ y.hashCode;
}
