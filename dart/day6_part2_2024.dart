
import 'dart:io';
import 'dart:collection';

class State {
  final int x, y, dir;
  State(this.x, this.y, this.dir);

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is State &&
          runtimeType == other.runtimeType &&
          x == other.x &&
          y == other.y &&
          dir == other.dir;

  @override
  int get hashCode => x.hashCode ^ y.hashCode ^ dir.hashCode;
}

void main() {
  final file = File('input.txt');
  final lines = file.readAsLinesSync();

  final grid = lines.map((line) => line.runes.toList()).toList();
  final h = grid.length;
  final w = grid[0].length;

  int startX = 0, startY = 0, startDir = 0;
  for (int i = 0; i < h; i++) {
    for (int j = 0; j < w; j++) {
      switch (String.fromCharCode(grid[i][j])) {
        case '^':
          startX = j;
          startY = i;
          startDir = 0;
          break;
        case '>':
          startX = j;
          startY = i;
          startDir = 1;
          break;
        case 'v':
          startX = j;
          startY = i;
          startDir = 2;
          break;
        case '<':
          startX = j;
          startY = i;
          startDir = 3;
          break;
      }
    }
  }

  grid[startY][startX] = '.'.codeUnitAt(0);

  int canLoop = 0;
  for (int y = 0; y < h; y++) {
    for (int x = 0; x < w; x++) {
      if (x == startX && y == startY) continue;
      if (grid[y][x] != '.'.codeUnitAt(0)) continue;
      grid[y][x] = '#'.codeUnitAt(0);
      if (loops(grid, startX, startY, startDir)) canLoop++;
      grid[y][x] = '.'.codeUnitAt(0);
    }
  }

  print(canLoop);
}

bool loops(List<List<int>> grid, int sx, int sy, int sdir) {
  final h = grid.length;
  final w = grid[0].length;
  final dirs = [
    [0, -1],
    [1, 0],
    [0, 1],
    [-1, 0]
  ];
  int x = sx, y = sy, dir = sdir;
  final seen = HashSet<State>();
  for (int step = 0; step < 2000000; step++) {
    final st = State(x, y, dir);
    if (seen.contains(st)) return true;
    seen.add(st);
    final dx = dirs[dir][0], dy = dirs[dir][1];
    final nx = x + dx, ny = y + dy;
    if (nx < 0 || nx >= w || ny < 0 || ny >= h) return false;
    if (grid[ny][nx] == '#'.codeUnitAt(0)) {
      dir = (dir + 1) % 4;
      continue;
    }
    x = nx;
    y = ny;
  }
  return false;
}
