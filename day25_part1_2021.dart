import 'dart:io';

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  List<List<String>> grid = lines.map((line) => line.split('')).toList();
  print(findSafeStep(grid));
}

int findSafeStep(List<List<String>> grid) {
  int step = 0;
  while (true) {
    bool eastMoved = moveEast(grid);
    bool southMoved = moveSouth(grid);
    step++;
    if (!eastMoved && !southMoved) {
      break;
    }
  }
  return step;
}

bool moveEast(List<List<String>> grid) {
  bool moved = false;
  int height = grid.length;
  int width = grid[0].length;
  List<List<String>> oldPositions = List.generate(height, (index) => List.filled(width, ''));

  for (int y = 0; y < height; y++) {
    for (int x = 0; x < width; x++) {
      if (grid[y][x] == '>') {
        int nextX = (x + 1) % width;
        if (grid[y][nextX] == '.') {
          oldPositions[y][x] = '.';
          grid[y][nextX] = '>';
          x++;
          moved = true;
        }
      }
    }
  }
  freeEmptyPositions(grid, oldPositions);
  return moved;
}

bool moveSouth(List<List<String>> grid) {
  bool moved = false;
  int height = grid.length;
  int width = grid[0].length;
  List<List<String>> oldPositions = List.generate(height, (index) => List.filled(width, ''));

  for (int x = 0; x < width; x++) {
    for (int y = 0; y < height; y++) {
      if (grid[y][x] == 'v') {
        int nextY = (y + 1) % height;
        if (grid[nextY][x] == '.') {
          oldPositions[y][x] = '.';
          grid[nextY][x] = 'v';
          y++;
          moved = true;
        }
      }
    }
  }
  freeEmptyPositions(grid, oldPositions);
  return moved;
}

void freeEmptyPositions(List<List<String>> grid, List<List<String>> oldPositions) {
  for (int y = 0; y < grid.length; y++) {
    for (int x = 0; x < grid[0].length; x++) {
      if (oldPositions[y][x] == '.') {
        grid[y][x] = '.';
      }
    }
  }
}