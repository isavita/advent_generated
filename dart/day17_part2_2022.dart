
import 'dart:io';

const rockstr = '''####

 # 
###
 # 

  #
  #
###

#
#
#
#

##
##''';

class Point {
  final int x;
  final int y;
  const Point(this.x, this.y);

  Point operator +(Point other) => Point(x + other.x, y + other.y);
  Point operator -(Point other) => Point(x - other.x, y - other.y);

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is Point && runtimeType == other.runtimeType && x == other.x && y == other.y;

  @override
  int get hashCode => x.hashCode ^ y.hashCode;

  @override
  String toString() => 'Point{x: $x, y: $y}';
}

Point dirFromByte(int b) {
  if (b == 60) return const Point(-1, 0); // '<'
  if (b == 62) return const Point(1, 0);  // '>'
  return const Point(0, 0);
}

bool collision(Set<Point> grid, Set<Point> rock, Point pos) {
  for (final p in rock) {
    final np = Point(p.x + pos.x, p.y + pos.y);
    if (grid.contains(np) || np.x < 0 || np.x > 7 - 1) {
      return true;
    }
  }
  return false;
}

List<Set<Point>> getRocks() {
  final rocks = <Set<Point>>[];
  final rockList = rockstr.split('\n\n');
  for (final rock in rockList) {
    final rockSet = <Point>{};
    final lines = rock.split('\n');
    for (int y = 0; y < lines.length; y++) {
      for (int x = 0; x < lines[y].length; x++) {
        if (lines[y][x] == '#') {
          rockSet.add(Point(x, lines.length - 1 - y));
        }
      }
    }
    rocks.add(rockSet);
  }
  return rocks;
}

String readAll(String path) {
  return File(path).readAsStringSync().trim();
}

void main() {
  final jetPattern = readAll('input.txt').codeUnits;
  final rocks = getRocks();
  final grid = <Point>{};
  for (int x = 0; x < 7; x++) {
    grid.add(Point(x, 0));
  }

  int floor = 0;
  int j = 0;
  final repeat = <String, List<int>>{};

  for (int i = 0, curr = 0; ; i++, curr = (curr + 1) % rocks.length) {
    final key = '${curr}_$j';
    if (repeat.containsKey(key)) {
      final r = repeat[key]!;
      final previ = r[0];
      final prevFloor = r[1];
      if ((1000000000000 - i) % (i - previ) == 0) {
        print(floor + (1000000000000 - i) ~/ (i - previ) * (floor - prevFloor));
        break;
      }
    }
    repeat[key] = [i, floor];

    final currRock = rocks[curr];
    Point pos = Point(2, floor + 4);

    while (true) {
      final jet = jetPattern[j];
      j = (j + 1) % jetPattern.length;
      pos = pos + dirFromByte(jet);
      if (collision(grid, currRock, pos)) {
        pos = pos - dirFromByte(jet);
      }
      pos = pos + const Point(0, -1);
      if (collision(grid, currRock, pos)) {
        pos = pos - const Point(0, -1);
        for (final p in currRock) {
          final settledPoint = Point(p.x + pos.x, p.y + pos.y);
          grid.add(settledPoint);
          if (settledPoint.y > floor) {
            floor = settledPoint.y;
          }
        }
        break;
      }
    }
  }
}
