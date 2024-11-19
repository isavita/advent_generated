
import 'dart:io';

void main() {
  parse();

  for (int i = 0; i < 10; i++) {
    run();
  }

  final minMax = findMinMax();
  int count = 0;
  for (int x = minMax.$1; x <= minMax.$2; x++) {
    for (int y = minMax.$3; y <= minMax.$4; y++) {
      if (!map.containsKey((x, y))) {
        count++;
      }
    }
  }

  print(count);
}

class Point {
  final int x, y;
  Point(this.x, this.y);
}

class Elf {
  Point pos;
  bool moving;
  Point nextPos;
  Elf(this.pos) : moving = false, nextPos = Point(0, 0);
}

const int N = 1, E = 3, S = 5, W = 7;

final map = <(int, int), bool>{};
final elves = <Elf>[];
final order = [N, S, W, E];
int currDir = 0;
final dirs = [
  Point(-1, -1), // NW
  Point(-1, 0),  // N
  Point(-1, 1),  // NE
  Point(0, 1),   // E
  Point(1, 1),   // SE
  Point(1, 0),   // S
  Point(1, -1),  // SW
  Point(0, -1),  // W
];

bool aroundAllEmpty(Elf e) {
  for (final d in dirs) {
    final adj = Point(e.pos.x + d.x, e.pos.y + d.y);
    if (map.containsKey((adj.x, adj.y))) return false;
  }
  return true;
}

bool elfInDirection(Elf e, int wannaGo) {
  for (int j = -1; j <= 1; j++) {
    final dxy = dirs[(wannaGo + j + 8) % 8];
    final adj = Point(e.pos.x + dxy.x, e.pos.y + dxy.y);
    if (map.containsKey((adj.x, adj.y))) return true;
  }
  return false;
}

void run() {
  final proposes = <(int, int), int>{};

  for (final e in elves) {
    if (aroundAllEmpty(e)) continue;

    for (int i = 0; i < 4; i++) {
      final dir = order[(currDir + i) % 4];

      if (elfInDirection(e, dir)) continue;

      final dxy = dirs[dir];
      final dest = Point(e.pos.x + dxy.x, e.pos.y + dxy.y);
      proposes[(dest.x, dest.y)] = (proposes[(dest.x, dest.y)] ?? 0) + 1;
      e.nextPos = dest;
      e.moving = true;
      break;
    }
  }

  for (final e in elves) {
    if (!e.moving) continue;

    if (proposes[(e.nextPos.x, e.nextPos.y)]! > 1) {
      e.moving = false;
      continue;
    }

    map.remove((e.pos.x, e.pos.y));
    map[(e.nextPos.x, e.nextPos.y)] = true;
    e.pos = e.nextPos;
    e.moving = false;
  }

  currDir = (currDir + 1) % 4;
}

(int, int, int, int) findMinMax() {
  int minX = 1 << 30, minY = 1 << 30, maxX = -(1 << 30), maxY = -(1 << 30);
  for (final p in map.keys) {
    if (p.$1 < minX) minX = p.$1;
    if (p.$2 < minY) minY = p.$2;
    if (p.$1 > maxX) maxX = p.$1;
    if (p.$2 > maxY) maxY = p.$2;
  }
  return (minX, maxX, minY, maxY);
}

void parse() {
  final file = File('input.txt');
  final lines = file.readAsLinesSync();
  for (int row = 0; row < lines.length; row++) {
    final line = lines[row];
    for (int col = 0; col < line.length; col++) {
      if (line[col] == '#') {
        final p = Point(row, col);
        map[(p.x, p.y)] = true;
        elves.add(Elf(p));
      }
    }
  }
}
