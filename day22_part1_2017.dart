
import 'dart:io';

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  int gridSize = lines.length;
  int virusX = gridSize ~/ 2;
  int virusY = gridSize ~/ 2;
  List<List<bool>> grid = List.generate(gridSize, (_) => List.filled(gridSize, false));
  int infections = 0;
  int direction = 0;
  List<List<int>> directions = [[-1, 0], [0, 1], [1, 0], [0, -1]];

  for (int i = 0; i < gridSize; i++) {
    for (int j = 0; j < gridSize; j++) {
      if (lines[i][j] == '#') {
        grid[i][j] = true;
      }
    }
  }

  for (int i = 0; i < 10000; i++) {
    if (grid[virusX][virusY]) {
      direction = (direction + 1) % 4;
      grid[virusX][virusY] = false;
    } else {
      direction = (direction + 3) % 4;
      grid[virusX][virusY] = true;
      infections++;
    }

    virusX += directions[direction][0];
    virusY += directions[direction][1];

    if (virusX < 0 || virusX >= gridSize || virusY < 0 || virusY >= gridSize) {
      grid = expandGrid(grid);
      virusX++;
      virusY++;
      gridSize += 2;
    }
  }

  print(infections);
}

List<List<bool>> expandGrid(List<List<bool>> grid) {
  int size = grid.length;
  List<List<bool>> newGrid = List.generate(size + 2, (_) => List.filled(size + 2, false));

  for (int i = 0; i < size; i++) {
    for (int j = 0; j < size; j++) {
      newGrid[i + 1][j + 1] = grid[i][j];
    }
  }

  return newGrid;
}
