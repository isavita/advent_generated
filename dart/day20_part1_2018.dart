
import 'dart:io';
import 'dart:collection';

class Point {
  final int x, y;
  Point(this.x, this.y);

  @override
  bool operator ==(Object other) =>
      other is Point && x == other.x && y == other.y;

  @override
  int get hashCode => x.hashCode ^ y.hashCode;
}

void main() {
  final input = File('input.txt').readAsStringSync().trim();
  final regex = input.substring(1, input.length - 1);
  final dm = buildMap(regex);
  final maxDoors = findFurthestRoom(dm);
  print(maxDoors);
}

Map<Point, Map<Point, bool>> buildMap(String regex) {
  final dm = <Point, Map<Point, bool>>{};
  final stack = <Point>[];
  var cp = Point(0, 0);

  for (final c in regex.runes) {
    final char = String.fromCharCode(c);
    if (char == '(') {
      stack.add(cp);
    } else if (char == '|') {
      cp = stack.last;
    } else if (char == ')') {
      cp = stack.last;
      stack.removeLast();
    } else {
      final np = move(cp, char);
      dm.putIfAbsent(cp, () => {});
      dm[cp]![np] = true;
      cp = np;
    }
  }
  return dm;
}

Point move(Point p, String dir) {
  switch (dir) {
    case 'N':
      return Point(p.x, p.y - 1);
    case 'S':
      return Point(p.x, p.y + 1);
    case 'E':
      return Point(p.x + 1, p.y);
    case 'W':
      return Point(p.x - 1, p.y);
    default:
      return p;
  }
}

int findFurthestRoom(Map<Point, Map<Point, bool>> dm) {
  final visited = <Point, int>{};
  final queue = Queue<Point>();
  queue.add(Point(0, 0));
  var maxDoors = 0;

  while (queue.isNotEmpty) {
    final p = queue.removeFirst();
    dm[p]?.forEach((np, _) {
      if (!visited.containsKey(np)) {
        visited[np] = (visited[p] ?? 0) + 1;
        maxDoors = max(maxDoors, visited[np]!);
        queue.add(np);
      }
    });
  }
  return maxDoors;
}

int max(int a, int b) => a > b ? a : b;
