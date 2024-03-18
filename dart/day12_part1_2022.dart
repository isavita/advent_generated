import 'dart:io';

void main() {
  var grid = <Point, String>{};
  var start = Point(0, 0);
  var end = Point(0, 0);
  var as = <Point>[];
  var y = 0;

  var file = File('input.txt');
  var lines = file.readAsLinesSync();
  
  for (var line in lines) {
    for (var x = 0; x < line.length; x++) {
      var p = Point(x, y);
      grid[p] = line[x];
      if (line[x] == 'S') {
        start = p;
      } else if (line[x] == 'E') {
        end = p;
      } else if (line[x] == 'a') {
        as.add(p);
      }
    }
    y++;
  }

  grid[start] = 'a';
  grid[end] = 'z';

  var dists = djikstra(grid, end);

  var l = dists[start];
  print(l);
}

class Point {
  final int x;
  final int y;

  Point(this.x, this.y);

  Point add(Point other) {
    return Point(x + other.x, y + other.y);
  }

  @override
  int get hashCode => x.hashCode ^ y.hashCode;

  @override
  bool operator ==(Object other) =>
      other is Point && x == other.x && y == other.y;
}

class Item {
  final Object obj;
  final int priority;

  Item(this.obj, this.priority);
}

class PQ {
  final List<Item> _items = [];

  int get length => _items.length;

  void push(Item item) {
    _items.add(item);
  }

  Item pop() {
    _items.sort((a, b) => b.priority.compareTo(a.priority));
    return _items.removeLast();
  }
}

final neighbors4 = [Point(0, 1), Point(0, -1), Point(1, 0), Point(-1, 0)];

Map<Point, int> djikstra(Map<Point, String> grid, Point end) {
  var pq = PQ();
  pq.push(Item(end, 0));
  var dist = {end: 0};

  while (pq.length > 0) {
    var curr = pq.pop().obj as Point;
    for (var n in neighbors4) {
      var next = curr.add(n);
      if (!grid.containsKey(next)) {
        continue;
      }
      if (grid[curr]!.codeUnitAt(0) - grid[next]!.codeUnitAt(0) > 1) {
        continue;
      }
      var nextDist = dist[curr]! + 1;
      if (!dist.containsKey(next) || nextDist < dist[next]!) {
        dist[next] = nextDist;
        pq.push(Item(next, nextDist));
      }
    }
  }

  return dist;
}