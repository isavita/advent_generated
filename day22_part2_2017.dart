import 'dart:io';

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  int gridSize = lines.length;
  int virusX = gridSize ~/ 2;
  int virusY = gridSize ~/ 2;
  List<List<String>> grid = List.generate(gridSize, (index) => List.filled(gridSize, '.'));

  for (int y = 0; y < gridSize; y++) {
    for (int x = 0; x < gridSize; x++) {
      grid[y][x] = lines[y][x];
    }
  }

  int infections = 0;
  int bursts = 10000000;
  List<List<int>> directions = [
    [-1, 0], // up
    [0, 1], // right
    [1, 0], // down
    [0, -1] // left
  ];
  int direction = 0; // up

  for (int i = 0; i < bursts; i++) {
    if (grid[virusY][virusX] == '#') {
      direction = (direction + 1) % 4;
      grid[virusY][virusX] = 'F'; // flag
    } else if (grid[virusY][virusX] == '.') {
      direction = (direction - 1 + 4) % 4;
      grid[virusY][virusX] = 'W'; // weaken
    } else if (grid[virusY][virusX] == 'W') {
      grid[virusY][virusX] = '#'; // infect
      infections++;
    } else if (grid[virusY][virusX] == 'F') {
      direction = (direction + 2) % 4;
      grid[virusY][virusX] = '.'; // clean
    }

    virusY += directions[direction][0];
    virusX += directions[direction][1];

    if (virusY < 0 || virusY >= gridSize || virusX < 0 || virusX >= gridSize) {
      List<List<String>> newGrid = List.generate(gridSize + 2, (index) => List.filled(gridSize + 2, '.'));
      for (int y = 0; y < gridSize; y++) {
        for (int x = 0; x < gridSize; x++) {
          newGrid[y + 1][x + 1] = grid[y][x];
        }
      }
      grid = newGrid;
      virusY++;
      virusX++;
      gridSize += 2;
    }
  }

  print(infections);
}