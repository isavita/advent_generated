
import 'dart:io';

void main() {
  final grid = File('input.txt').readAsLinesSync();
  int startRow = -1, startCol = -1;
  for (int r = 0; r < grid.length; r++) {
    final c = grid[r].indexOf('S');
    if (c != -1) {
      startRow = r;
      startCol = c;
      break;
    }
  }
  if (startRow == -1) {
    print(0);
    return;
  }
  var beams = <int>{startCol};
  int splits = 0;
  for (int r = startRow + 1; r < grid.length && beams.isNotEmpty; r++) {
    final next = <int>{};
    for (final c in beams) {
      if (c >= 0 && c < grid[r].length) {
        if (grid[r][c] == '^') {
          splits++;
          next..add(c - 1)..add(c + 1);
        } else {
          next.add(c);
        }
      }
    }
    beams = next;
  }
  print(splits);
}
