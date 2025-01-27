
import 'dart:io';
import 'dart:collection';

class Point {
  int x, y;
  Point(this.x, this.y);

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is Point && runtimeType == other.runtimeType && x == other.x && y == other.y;

  @override
  int get hashCode => x.hashCode ^ y.hashCode;

  Point operator +(Point other) => Point(x + other.x, y + other.y);

  Point operator -(Point other) => Point(x - other.x, y - other.y);

  @override
  String toString() {
    return 'Point{x: $x, y: $y}';
  }
}

class Node {
  int used, avail;
  Node(this.used, this.avail);
}

List<Point> neighbors4 = [Point(0, 1), Point(0, -1), Point(1, 0), Point(-1, 0)];

void main() {
  final nodes = <Point, Node>{};
  final input = File('input.txt').readAsStringSync().trim().split('\n').sublist(2);

  for (final line in input) {
    final parts = line.split(RegExp(r'\s+'));
    final match = RegExp(r'-x(\d+)-y(\d+)').firstMatch(parts[0]);
    final p = Point(int.parse(match!.group(1)!), int.parse(match.group(2)!));
    final n = Node(
      int.parse(parts[2].substring(0, parts[2].length - 1)),
      int.parse(parts[3].substring(0, parts[3].length - 1)),
    );
    nodes[p] = n;
  }

  print(minmoves(nodes));
}

int minmoves(Map<Point, Node> nodes) {
  final dimensions = dim(nodes);
  final w = dimensions[0];
  final goal = Point(w, 0);
  final hole = findHole(nodes);

  var sum = 0;
  var currentGoal = goal;
  var currentHole = hole;

  while (currentGoal != Point(0, 0)) {
    final next = currentGoal - Point(1, 0);
    final m1 = moves(nodes, currentGoal, currentHole, next);
    sum += m1;
    currentHole = next;
    final m2 = moves(nodes, currentGoal, currentGoal, currentHole);
    sum += m2;
    final temp = currentGoal;
    currentGoal = currentHole;
    currentHole = temp;
  }
  return sum;
}

Point findHole(Map<Point, Node> nodes) {
  for (final entry in nodes.entries) {
    if (entry.value.used == 0) {
      return entry.key;
    }
  }
  throw Exception('no hole');
}

int moves(Map<Point, Node> nodes, Point goal, Point from, Point to) {
  final dimensions = dim(nodes);
  final w = dimensions[0];
  final h = dimensions[1];
  final depth = <Point, int>{from: 0};
  final queue = Queue<Point>()..add(from);

  while (queue.isNotEmpty) {
    final p = queue.removeFirst();
    if (p == to) {
      return depth[p]!;
    }
    final currentDepth = depth[p]! + 1;
    for (final n in neighbors4) {
      final next = p + n;
      if (next.x < 0 || next.y < 0 || next.x > w || next.y > h || nodes[next]!.used > 400 || next == goal) {
        continue;
      }
      if (!depth.containsKey(next)) {
        depth[next] = currentDepth;
        queue.add(next);
      }
    }
  }
  return -1;
}


List<int> dim(Map<Point, Node> m) {
  var w = 0;
  var h = 0;
  for (final p in m.keys) {
    if (p.x > w) {
      w = p.x;
    }
    if (p.y > h) {
      h = p.y;
    }
  }
  return [w, h];
}
