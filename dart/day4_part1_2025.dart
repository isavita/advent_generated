
import 'dart:io';

void main() {
  final grid = File('input.txt').readAsLinesSync()
      .map((l) => l.trim())
      .where((l) => l.isNotEmpty)
      .toList();

  if (grid.isEmpty) {
    print(0);
    return;
  }

  int accessible = 0;
  final rows = grid.length;
  final cols = grid[0].length;

  for (int r = 0; r < rows; r++) {
    for (int c = 0; c < cols; c++) {
      if (grid[r][c] == '@') {
        int neighbors = 0;
        for (int dr = -1; dr <= 1; dr++) {
          for (int dc = -1; dc <= 1; dc++) {
            if (dr == 0 && dc == 0) continue;
            final nr = r + dr;
            final nc = c + dc;
            if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
              if (grid[nr][nc] == '@') neighbors++;
            }
          }
        }
        if (neighbors < 4) accessible++;
      }
    }
  }

  print(accessible);
}
