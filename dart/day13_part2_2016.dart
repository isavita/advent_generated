import 'dart:io';

const favoriteNumber = 1362; // Replace with your puzzle input

class Point {
  int x, y;
  Point(this.x, this.y);

  @override
  bool operator ==(Object other) =>
      other is Point && x == other.x && y == other.y;

  @override
  int get hashCode => x.hashCode ^ y.hashCode;
}

bool isWall(int x, int y) {
  int num = x * x + 3 * x + 2 * x * y + y + y * y + favoriteNumber;
  int bits = 0;
  while (num > 0) {
    if (num % 2 == 1) {
      bits++;
    }
    num ~/= 2;
  }
  return bits % 2 != 0;
}

int bfsMaxSteps(Point start, int maxSteps) {
  Map<Point, bool> visited = {};
  List<Point> queue = [start];
  visited[start] = true;
  int steps = 0;

  while (queue.isNotEmpty && steps < maxSteps) {
    int size = queue.length;
    for (int i = 0; i < size; i++) {
      Point point = queue[i];

      for (Point delta in [Point(1, 0), Point(-1, 0), Point(0, 1), Point(0, -1)]) {
        Point next = Point(point.x + delta.x, point.y + delta.y);
        if (next.x >= 0 && next.y >= 0 && !isWall(next.x, next.y) && !visited.containsKey(next)) {
          visited[next] = true;
          queue.add(next);
        }
      }
    }
    queue.removeRange(0, size);
    steps++;
  }

  return visited.length;
}

void main() async {
  List<String> lines = await File('input.txt').readAsLines();
  Point start = Point(1, 1);
  int reachableLocations = bfsMaxSteps(start, 50);
  print(reachableLocations);
}