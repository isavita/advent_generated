import 'dart:io';

void main() {
  final file = File('input.txt');
  final grid = file.readAsLinesSync().map((line) => line.codeUnits.toList()).toList();

  int x = 0, y = 0;
  for (int i = 0; i < grid[0].length; i++) {
    if (grid[0][i] == 124) { // ASCII code for '|'
      x = i;
      break;
    }
  }

  int dx = 0, dy = 1;
  int steps = 0;

  while (x >= 0 && x < grid[0].length && y >= 0 && y < grid.length) {
    if (grid[y][x] == 32) break; // ASCII code for ' '

    steps++;

    if (grid[y][x] == 43) { // ASCII code for '+'
      if (dx == 0) {
        if (x > 0 && (grid[y][x - 1] == 45 || (grid[y][x - 1] >= 65 && grid[y][x - 1] <= 90))) { // ASCII code for '-' and 'A' to 'Z'
          dx = -1;
          dy = 0;
        } else {
          dx = 1;
          dy = 0;
        }
      } else {
        if (y > 0 && (grid[y - 1][x] == 124 || (grid[y - 1][x] >= 65 && grid[y - 1][x] <= 90))) { // ASCII code for '|' and 'A' to 'Z'
          dx = 0;
          dy = -1;
        } else {
          dx = 0;
          dy = 1;
        }
      }
    }

    x += dx;
    y += dy;
  }

  print(steps);
}