
import 'dart:io';

void main() {
  final grid = readFile('input.txt');
  final targetWord = 'XMAS';
  final count = countWordOccurrences(grid, targetWord);
  print(count);
}

List<String> readFile(String filePath) {
  final file = File(filePath);
  return file.readAsLinesSync();
}

int countWordOccurrences(List<String> grid, String targetWord) {
  if (grid.isEmpty || grid[0].isEmpty) {
    return 0;
  }
  final rows = grid.length;
  final cols = grid[0].length;
  int count = 0;

  final directions = [
    [0, 1],  // Right
    [0, -1], // Left
    [1, 0],  // Down
    [-1, 0], // Up
    [1, 1],  // Down-Right
    [1, -1], // Down-Left
    [-1, -1], // Up-Left
    [-1, 1]  // Up-Right
  ];

  for (int r = 0; r < rows; r++) {
    for (int c = 0; c < cols; c++) {
      if (grid[r][c] == targetWord[0]) {
        for (final dir in directions) {
          if (checkWord(grid, r, c, dir[0], dir[1], targetWord)) {
            count++;
          }
        }
      }
    }
  }
  return count;
}

bool checkWord(
    List<String> grid, int row, int col, int rowDir, int colDir, String targetWord) {
  final rows = grid.length;
  final cols = grid[0].length;
  for (int i = 0; i < targetWord.length; i++) {
    final currentRow = row + i * rowDir;
    final currentCol = col + i * colDir;

    if (currentRow < 0 || currentRow >= rows || currentCol < 0 || currentCol >= cols) {
      return false;
    }
    if (grid[currentRow][currentCol] != targetWord[i]) {
      return false;
    }
  }
  return true;
}
