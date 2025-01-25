
import 'dart:io';

void main() {
  final grid = readFile('input.txt');
  tiltNorth(grid);
  final load = calculateLoad(grid);
  print(load);
}

List<List<String>> readFile(String filePath) {
  final file = File(filePath);
  final lines = file.readAsLinesSync();
  return lines.map((line) => line.split('')).toList();
}

void tiltNorth(List<List<String>> grid) {
  if (grid.isEmpty) return;
  final rows = grid.length;
  final cols = grid[0].length;

  for (var c = 0; c < cols; c++) {
    int nextEmptyRow = 0;
    for (var r = 0; r < rows; r++) {
      if (grid[r][c] == 'O') {
        if (r != nextEmptyRow) {
          grid[nextEmptyRow][c] = 'O';
          grid[r][c] = '.';
        }
        nextEmptyRow++;
      } else if (grid[r][c] == '#') {
        nextEmptyRow = r + 1;
      }
    }
  }
}

int calculateLoad(List<List<String>> grid) {
  int totalLoad = 0;
  final rows = grid.length;
  for (var r = 0; r < rows; r++) {
    for (var c = 0; c < grid[r].length; c++) {
      if (grid[r][c] == 'O') {
        totalLoad += rows - r;
      }
    }
  }
  return totalLoad;
}
