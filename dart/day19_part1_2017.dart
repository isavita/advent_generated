import 'dart:io';

void main() {
  final file = File('input.txt');
  final lines = file.readAsLinesSync();

  final grid = lines.map((line) => line.codeUnits.map((c) => String.fromCharCode(c)).toList()).toList();

  int x = 0, y = 0;
  for (int i = 0; i < grid[0].length; i++) {
    if (grid[0][i] == '|') {
      x = i;
      break;
    }
  }

  int dx = 0, dy = 1;
  final letters = <String>[];

  while (x >= 0 && x < grid[0].length && y >= 0 && y < grid.length) {
    final cell = grid[y][x];

    if (cell == ' ') {
      break;
    }

    if (cell.codeUnitAt(0) >= 'A'.codeUnitAt(0) && cell.codeUnitAt(0) <= 'Z'.codeUnitAt(0)) {
      letters.add(cell);
    }

    if (cell == '+') {
      if (dx == 0) {
        if (x > 0 && (grid[y][x - 1] == '-' || (grid[y][x - 1].codeUnitAt(0) >= 'A'.codeUnitAt(0) && grid[y][x - 1].codeUnitAt(0) <= 'Z'.codeUnitAt(0)))) {
          dx = -1;
          dy = 0;
        } else {
          dx = 1;
          dy = 0;
        }
      } else {
        if (y > 0 && (grid[y - 1][x] == '|' || (grid[y - 1][x].codeUnitAt(0) >= 'A'.codeUnitAt(0) && grid[y - 1][x].codeUnitAt(0) <= 'Z'.codeUnitAt(0)))) {
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

  print(letters.join());
}