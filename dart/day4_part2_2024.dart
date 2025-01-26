
import 'dart:io';

bool checkMAS(List<String> grid, int x, int y, int dx, int dy) {
  if (x < 0 || y < 0 || x >= grid.length || y >= grid[0].length) {
    return false;
  }

  bool forward = true;
  bool backward = true;
  String word = "MAS";

  for (int i = 0; i < word.length; i++) {
    int newX = x + (dx * i);
    int newY = y + (dy * i);
    if (newX < 0 || newY < 0 || newX >= grid.length || newY >= grid[0].length) {
      forward = false;
      break;
    }
    if (grid[newX][newY] != word[i]) {
      forward = false;
    }
  }

  for (int i = 0; i < word.length; i++) {
    int newX = x + (dx * i);
    int newY = y + (dy * i);
    if (newX < 0 || newY < 0 || newX >= grid.length || newY >= grid[0].length) {
      backward = false;
      break;
    }
    if (grid[newX][newY] != word[word.length - 1 - i]) {
      backward = false;
    }
  }

  return forward || backward;
}

bool checkXMAS(List<String> grid, int x, int y) {
  if (checkMAS(grid, x - 1, y - 1, 1, 1) && checkMAS(grid, x - 1, y + 1, 1, -1)) {
    return true;
  }

  if (checkMAS(grid, x + 1, y - 1, -1, 1) && checkMAS(grid, x + 1, y + 1, -1, -1)) {
    return true;
  }

  return false;
}

int countXMASPatterns(List<String> grid) {
  int count = 0;

  if (grid.length < 3 || grid[0].length < 3) {
    return 0;
  }

  for (int i = 1; i < grid.length - 1; i++) {
    for (int j = 1; j < grid[i].length - 1; j++) {
      if (grid[i][j] == 'A' && checkXMAS(grid, i, j)) {
        count++;
      }
    }
  }

  return count;
}

void main() {
  File file = File('input.txt');
  List<String> grid = file.readAsLinesSync().where((line) => line.isNotEmpty).toList();

  int count = countXMASPatterns(grid);
  print('X-MAS patterns appear $count times in the word search');
}
