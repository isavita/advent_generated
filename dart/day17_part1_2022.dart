import 'dart:io';
import 'dart:math';

const rockStr = '''####

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

void main() {
  final jetPattern = File('input.txt').readAsStringSync().trim().codeUnits;

  final rocks = getRocks();
  final grid = <Point<int>, void>{};
  for (var x = 0; x < 7; x++) {
    grid[Point(x, 0)] = null;
  }
  var floor = 0, j = 0;
  final repeat = <List<dynamic>, List<dynamic>>{};

  for (var i = 0, curr = 0;;) {
    if (i == 2022) {
      print(floor);
      break;
    }
    final key = [curr, j];
    if (repeat.containsKey(key)) {
      final prevIAndFloor = repeat[key]!;
      final prevI = prevIAndFloor[0] as int;
      final prevFloor = prevIAndFloor[1] as int;
      final diff = i - prevI;
      final height = floor - prevFloor;
      final remaining = 2022 - i;
      final cycles = remaining ~/ diff;
      i += cycles * diff;
      floor += cycles * height;
    }
    repeat[key] = [i, floor];
    final currRock = rocks[curr];
    var pos = Point(2, floor + 4);
    while (true) {
      final jet = jetPattern[j];
      j = (j + 1) % jetPattern.length;
      final dir = dirFromByte(jet);
      pos += dir.point;
      if (collision(grid, currRock, pos)) {
        pos -= dir.point;
      }
      pos += const Point(0, -1);
      if (collision(grid, currRock, pos)) {
        pos -= const Point(0, -1);
        for (final p in currRock.keys) {
          grid[p + pos] = null;
          if (p.y + pos.y > floor) {
            floor = p.y + pos.y;
          }
        }
        break;
      }
    }
    i++;
    curr = (curr + 1) % rocks.length;
  }
}

bool collision(Map<Point<int>, void> grid, Map<Point<int>, void> rock, Point<int> pos) {
  for (final p in rock.keys) {
    final newPos = p + pos;
    if (grid.containsKey(newPos) || newPos.x < 0 || newPos.x > 6) {
      return true;
    }
  }
  return false;
}

List<Map<Point<int>, void>> getRocks() {
  final rocks = <Map<Point<int>, void>>[];
  for (final rock in rockStr.split('\n\n')) {
    final rockMap = <Point<int>, void>{};
    final lines = rock.split('\n');
    for (var y = 0; y < lines.length; y++) {
      for (var x = 0; x < lines[y].length; x++) {
        if (lines[y][x] == '#') {
          rockMap[Point(x, lines.length - 1 - y)] = null;
        }
      }
    }
    rocks.add(rockMap);
  }
  return rocks;
}

enum Dir { n, e, s, w }

extension on Dir {
  Point<int> get point {
    switch (this) {
      case Dir.n:
        return const Point(0, 1);
      case Dir.e:
        return const Point(1, 0);
      case Dir.s:
        return const Point(0, -1);
      case Dir.w:
        return const Point(-1, 0);
    }
  }
}

Dir dirFromByte(int b) {
  switch (String.fromCharCode(b)) {
    case '^':
    case 'N':
      return Dir.n;
    case '>':
    case 'E':
      return Dir.e;
    case 'v':
    case 'S':
      return Dir.s;
    case '<':
    case 'W':
      return Dir.w;
    default:
      throw ArgumentError('Invalid direction byte: $b');
  }
}