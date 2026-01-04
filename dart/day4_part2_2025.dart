
import 'dart:io';

void main() async {
  final lines = await File('input.txt').readAsLines();
  final grid = lines.map((l) => l.split('').toList()).toList();
  final rows = grid.length;
  final cols = grid[0].length;

  int countAccessible(List<List<String>> g) {
    int c = 0;
    for (int r = 0; r < rows; r++) {
      for (int c2 = 0; c2 < cols; c2++) {
        if (g[r][c2] != '@') continue;
        int neighbors = 0;
        for (int dr = -1; dr <= 1; dr++) {
          for (int dc = -1; dc <= 1; dc++) {
            if (dr == 0 && dc == 0) continue;
            final nr = r + dr, nc = c2 + dc;
            if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && g[nr][nc] == '@') {
              neighbors++;
            }
          }
        }
        if (neighbors < 4) c++;
      }
    }
    return c;
  }

  // Part 1
  final part1 = countAccessible(grid);
  print(part1);

  // Part 2: simulate removals
  final g = grid.map((row) => row.toList()).toList();
  int total = 0;
  while (true) {
    final toRemove = <List<int>>[];
    for (int r = 0; r < rows; r++) {
      for (int c2 = 0; c2 < cols; c2++) {
        if (g[r][c2] != '@') continue;
        int neighbors = 0;
        for (int dr = -1; dr <= 1; dr++) {
          for (int dc = -1; dc <= 1; dc++) {
            if (dr == 0 && dc == 0) continue;
            final nr = r + dr, nc = c2 + dc;
            if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && g[nr][nc] == '@') {
              neighbors++;
            }
          }
        }
        if (neighbors < 4) toRemove.add([r, c2]);
      }
    }
    if (toRemove.isEmpty) break;
    for (final p in toRemove) g[p[0]][p[1]] = '.';
    total += toRemove.length;
  }
  print(total);
}
