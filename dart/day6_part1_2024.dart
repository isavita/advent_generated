
import 'dart:io';

void main() {
  final lines = File('input.txt').readAsLinesSync();
  final grid = lines.map((line) => line.split('')).toList();
  final h = grid.length;
  final w = grid[0].length;
  int x = 0, y = 0;
  int dirX = 0, dirY = 0;
  final dirs = [[0, -1], [1, 0], [0, 1], [-1, 0]];
  int dirIdx = 0;
  bool found = false;
  for (int i = 0; i < h && !found; i++) {
    for (int j = 0; j < w && !found; j++) {
      switch (grid[i][j]) {
        case '^':
          x = j;
          y = i;
          dirIdx = 0;
          dirX = dirs[dirIdx][0];
          dirY = dirs[dirIdx][1];
          found = true;
        case '>':
          x = j;
          y = i;
          dirIdx = 1;
          dirX = dirs[dirIdx][0];
          dirY = dirs[dirIdx][1];
          found = true;
        case 'v':
          x = j;
          y = i;
          dirIdx = 2;
          dirX = dirs[dirIdx][0];
          dirY = dirs[dirIdx][1];
          found = true;
        case '<':
          x = j;
          y = i;
          dirIdx = 3;
          dirX = dirs[dirIdx][0];
          dirY = dirs[dirIdx][1];
          found = true;
      }
    }
  }
  final visited = <String>{'$x,$y'};
  while (true) {
    final nx = x + dirX;
    final ny = y + dirY;
    if (nx < 0 || nx >= w || ny < 0 || ny >= h) {
      break;
    }
    if (grid[ny][nx] == '#') {
      dirIdx = (dirIdx + 1) % 4;
      dirX = dirs[dirIdx][0];
      dirY = dirs[dirIdx][1];
      continue;
    }
    x = nx;
    y = ny;
    visited.add('$x,$y');
  }
  print(visited.length);
}
