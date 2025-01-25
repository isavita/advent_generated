
import 'dart:io';

class Point {
  int x, y;
  Point(this.x, this.y);

  @override
  bool operator ==(Object other) =>
      other is Point && x == other.x && y == other.y;

  @override
  int get hashCode => x.hashCode ^ y.hashCode;
}

typedef DoorMap = Map<Point, Set<Point>>;

DoorMap buildMap(String regex) {
  DoorMap dm = {};
  List<Point> stack = [];
  Point cp = Point(0, 0);
  for (var c in regex.runes) {
    if (c == '('.codeUnitAt(0)) {
      stack.add(cp);
    } else if (c == '|'.codeUnitAt(0)) {
      cp = stack.last;
    } else if (c == ')'.codeUnitAt(0)) {
      cp = stack.removeLast();
    } else {
      Point np = move(cp, String.fromCharCode(c));
      dm.putIfAbsent(cp, () => {}).add(np);
      dm.putIfAbsent(np, () => {}).add(cp);
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
  }
  return p;
}

int countRooms(DoorMap dm, int minDoors) {
  Map<Point, int> visited = {};
  List<Point> queue = [Point(0, 0)];
  visited[Point(0, 0)] = 0;
  int roomCount = 0;
  int head = 0;

  while (head < queue.length) {
    Point p = queue[head++];
    for (Point np in dm[p] ?? {}) {
      if (!visited.containsKey(np)) {
        visited[np] = visited[p]! + 1;
        if (visited[np]! >= minDoors) {
          roomCount++;
        }
        queue.add(np);
      }
    }
  }
  return roomCount;
}

void main() {
  String data = File("input.txt").readAsStringSync();
  String regex = data.substring(1, data.length - 1);
  DoorMap dm = buildMap(regex);
  int rooms = countRooms(dm, 1000);
  print(rooms);
}
