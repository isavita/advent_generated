
import 'dart:io';
import 'dart:collection';

const wall = '#';
const free = '.';

class P {
  final int x;
  final int y;

  const P(this.x, this.y);

  List<P> neighbours() {
    return [
      P(x, y + 1),
      P(x + 1, y),
      P(x, y - 1),
      P(x - 1, y),
    ];
  }

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is P && runtimeType == other.runtimeType && x == other.x && y == other.y;

  @override
  int get hashCode => x.hashCode ^ y.hashCode;
}

class Maze {
  final int xMax;
  final int yMax;
  final Map<P, String> grid;
  final P aa;
  final P zz;
  final Map<P, P> teleport;
  final Map<P, String> portalName;
  final Map<P, bool> isOuter;

  Maze(this.xMax, this.yMax, this.grid, this.aa, this.zz, this.teleport,
      this.portalName, this.isOuter);
}

Maze parse() {
  final grid = <P, String>{};
  int xMax = 0, yMax = 0;

  final lines = File('input.txt').readAsLinesSync();
  for (int i = 0; i < lines.length; i++) {
    final line = lines[i];
    if (line.length > yMax) {
      yMax = line.length;
    }
    for (int j = 0; j < line.length; j++) {
      grid[P(i, j)] = line[j];
    }
  }
  xMax = lines.length;

  P aa = P(0, 0);
  P zz = P(0, 0);
  final isOuter = <P, bool>{};
  final portalName = <P, String>{};
  final teleport = <P, P>{};
  final cache = <String, P>{};

  for (int i = 0; i < xMax; i++) {
    for (int j = 0; j < yMax; j++) {
      final c = grid[P(i, j)];
      if (c == null || !isLetter(c)) {
        continue;
      }
      final res = extractPortal(grid, P(i, j));
      if (res == null) {
        continue;
      }
      final pName = res[0] as String;
      final pPoint = res[1] as P;
      portalName[pPoint] = pName;
      if (pName == 'AA') {
        aa = pPoint;
        isOuter[pPoint] = true;
        continue;
      }
      if (pName == 'ZZ') {
        zz = pPoint;
        isOuter[pPoint] = true;
        continue;
      }
      if (cache.containsKey(pName)) {
        teleport[pPoint] = cache[pName]!;
        teleport[cache[pName]!] = pPoint;
      } else {
        cache[pName] = pPoint;
      }
      if (j == 0 || i == 0 || i == xMax - 2 || j == yMax - 2) {
        isOuter[pPoint] = true;
      } else {
        isOuter[pPoint] = false;
      }
    }
  }
  return Maze(xMax, yMax, grid, aa, zz, teleport, portalName, isOuter);
}

bool isLetter(String c) {
  return c.compareTo('A') >= 0 && c.compareTo('Z') <= 0;
}

List<Object>? extractPortal(Map<P, String> grid, P p) {
  final c1 = grid[p];
  if (c1 == null) return null;

  final p2 = P(p.x + 1, p.y);
  final c2 = grid[p2];
  if (c2 != null && isLetter(c2)) {
    final portalName = c1 + c2;
    var portalPoint = P(p.x + 2, p.y);
    if (grid[portalPoint] == free) {
      return [portalName, portalPoint];
    }
    portalPoint = P(p.x - 1, p.y);
    if (grid[portalPoint] == free) {
      return [portalName, portalPoint];
    }
  }

  final p3 = P(p.x, p.y + 1);
  final c3 = grid[p3];
  if (c3 != null && isLetter(c3)) {
    final portalName = c1 + c3;
    var portalPoint = P(p.x, p.y + 2);
    if (grid[portalPoint] == free) {
      return [portalName, portalPoint];
    }
    portalPoint = P(p.x, p.y - 1);
    if (grid[portalPoint] == free) {
      return [portalName, portalPoint];
    }
  }
  return null;
}

class Status {
  final P p;
  final int depth;

  Status(this.p, this.depth);

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is Status &&
          runtimeType == other.runtimeType &&
          p == other.p &&
          depth == other.depth;

  @override
  int get hashCode => p.hashCode ^ depth.hashCode;
}

int bfsNested(Maze m) {
  final discovered = HashSet<Status>();
  final toDo = Queue<Status>();
  final root = Status(m.aa, 0);
  discovered.add(root);
  toDo.add(root);
  int steps = 0;
  while (toDo.isNotEmpty) {
    for (int levelSize = toDo.length; levelSize > 0; levelSize--) {
      final curr = toDo.removeFirst();
      for (final n in curr.p.neighbours()) {
        final dest = m.grid[n];
        if (dest == null) continue;
        switch (dest) {
          case wall:
            continue;
          case free:
            final target = Status(n, curr.depth);
            if (!discovered.contains(target)) {
              discovered.add(target);
              toDo.add(target);
            }
            break;
          default:
            if (!isLetter(dest)) continue;
            Status target;
            final isOuter = m.isOuter[curr.p]!;
            if (!isOuter) {
              target = Status(m.teleport[curr.p]!, curr.depth + 1);
            } else {
              final portalName = m.portalName[curr.p]!;
              if (curr.depth == 0) {
                if (portalName == 'ZZ') {
                  return steps;
                }
                continue;
              }
              if (portalName == 'AA' || portalName == 'ZZ') {
                continue;
              }
              target = Status(m.teleport[curr.p]!, curr.depth - 1);
            }
            if (!discovered.contains(target)) {
              discovered.add(target);
              toDo.add(target);
            }
        }
      }
    }
    steps++;
  }
  return -1;
}

void main() {
  final m = parse();
  print(bfsNested(m));
}
