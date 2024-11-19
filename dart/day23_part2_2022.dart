
import 'dart:io';

void main() {
  parse();

  for (int i = 0;; i++) {
    if (!run()) {
      print(i + 1);
      return;
    }
  }
}

class P {
  int x, y;
  P(this.x, this.y);

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is P && other.x == x && other.y == y;

  @override
  int get hashCode => x.hashCode ^ y.hashCode;
}

class Elf {
  P pos;
  bool moving;
  P nextPos;
  Elf(this.pos) : moving = false, nextPos = P(0, 0);
}

const int N = 1, E = 3, S = 5, W = 7;

final Map<P, bool> map = {};
final List<Elf> elves = [];
final List<int> order = [N, S, W, E];
int currDir = 0;
final List<P> dirs = [
  P(-1, -1), // NW
  P(-1, 0),  // N
  P(-1, 1),  // NE
  P(0, 1),   // E
  P(1, 1),   // SE
  P(1, 0),   // S
  P(1, -1),  // SW
  P(0, -1),  // W
];

bool aroundAllEmpty(Elf e) {
  for (var d in dirs) {
    P adj = P(e.pos.x + d.x, e.pos.y + d.y);
    if (map.containsKey(adj)) return false;
  }
  return true;
}

bool elfInDirection(Elf e, int wannaGo) {
  for (int j = -1; j <= 1; j++) {
    P dxy = dirs[(wannaGo + j + 8) % 8];
    P adj = P(e.pos.x + dxy.x, e.pos.y + dxy.y);
    if (map.containsKey(adj)) return true;
  }
  return false;
}

bool run() {
  Map<P, int> proposes = {};

  for (var e in elves) {
    if (aroundAllEmpty(e)) continue;

    for (int i = 0; i < 4; i++) {
      int dir = order[(currDir + i) % 4];

      if (elfInDirection(e, dir)) continue;

      P dxy = dirs[dir];
      P dest = P(e.pos.x + dxy.x, e.pos.y + dxy.y);
      proposes[dest] = (proposes[dest] ?? 0) + 1;
      e.nextPos = dest;
      e.moving = true;
      break;
    }
  }

  bool someoneMoved = false;
  for (var e in elves) {
    if (!e.moving) continue;

    if (proposes[e.nextPos]! > 1) {
      e.moving = false;
      continue;
    }

    someoneMoved = true;
    map.remove(e.pos);
    map[e.nextPos] = true;
    e.pos = e.nextPos;
    e.moving = false;
  }

  currDir = (currDir + 1) % 4;

  return someoneMoved;
}

void parse() {
  File file = File('input.txt');
  List<String> lines = file.readAsLinesSync();

  for (int row = 0; row < lines.length; row++) {
    String line = lines[row];
    for (int col = 0; col < line.length; col++) {
      if (line[col] == '#') {
        P p = P(row, col);
        map[p] = true;
        elves.add(Elf(p));
      }
    }
  }
}
