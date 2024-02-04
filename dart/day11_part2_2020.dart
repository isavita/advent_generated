
import 'dart:io';

void main() {
  List<String> seating = File('input.txt').readAsLinesSync();

  List<List<String>> grid = seating.map((row) => row.split('')).toList();

  int rows = grid.length;
  int cols = grid[0].length;

  List<List<String>> newGrid = List<List<String>>.generate(rows, (index) => List<String>.filled(cols, '.'));

  bool changed = true;

  while (changed) {
    changed = false;

    for (int i = 0; i < rows; i++) {
      for (int j = 0; j < cols; j++) {
        int occupiedSeats = 0;

        for (int x = -1; x <= 1; x++) {
          for (int y = -1; y <= 1; y++) {
            if (x == 0 && y == 0) continue;

            int dx = i + x;
            int dy = j + y;

            while (dx >= 0 && dx < rows && dy >= 0 && dy < cols) {
              if (grid[dx][dy] == '#') {
                occupiedSeats++;
                break;
              } else if (grid[dx][dy] == 'L') {
                break;
              }

              dx += x;
              dy += y;
            }
          }
        }

        if (grid[i][j] == 'L' && occupiedSeats == 0) {
          newGrid[i][j] = '#';
          changed = true;
        } else if (grid[i][j] == '#' && occupiedSeats >= 5) {
          newGrid[i][j] = 'L';
          changed = true;
        } else {
          newGrid[i][j] = grid[i][j];
        }
      }
    }

    for (int i = 0; i < rows; i++) {
      for (int j = 0; j < cols; j++) {
        grid[i][j] = newGrid[i][j];
      }
    }
  }

  int occupiedSeats = 0;
  for (int i = 0; i < rows; i++) {
    for (int j = 0; j < cols; j++) {
      if (grid[i][j] == '#') {
        occupiedSeats++;
      }
    }
  }

  print(occupiedSeats);
}
