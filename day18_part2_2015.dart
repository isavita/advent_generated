import 'dart:io';

const gridSize = 100;
const steps = 100;

int countOnNeighbors(List<List<bool>> grid, int x, int y) {
  int on = 0;
  for (int dx = -1; dx <= 1; dx++) {
    for (int dy = -1; dy <= 1; dy++) {
      if (dx == 0 && dy == 0) {
        continue;
      }
      int nx = x + dx;
      int ny = y + dy;
      if (nx >= 0 && nx < gridSize && ny >= 0 && ny < gridSize && grid[nx][ny]) {
        on++;
      }
    }
  }
  return on;
}

List<List<bool>> step(List<List<bool>> grid) {
  List<List<bool>> newGrid = List.generate(gridSize, (index) => List.filled(gridSize, false));

  for (int x = 0; x < gridSize; x++) {
    for (int y = 0; y < gridSize; y++) {
      int onNeighbors = countOnNeighbors(grid, x, y);
      if (grid[x][y]) {
        newGrid[x][y] = onNeighbors == 2 || onNeighbors == 3;
      } else {
        newGrid[x][y] = onNeighbors == 3;
      }
    }
  }

  // Ensure corners are always on
  newGrid[0][0] = true;
  newGrid[0][gridSize - 1] = true;
  newGrid[gridSize - 1][0] = true;
  newGrid[gridSize - 1][gridSize - 1] = true;

  return newGrid;
}

void main() {
  File file = File('input.txt');
  List<List<bool>> grid = List.generate(gridSize, (index) => List.filled(gridSize, false));

  List<String> lines = file.readAsLinesSync();
  int y = 0;
  for (String line in lines) {
    for (int x = 0; x < line.length; x++) {
      grid[x][y] = line[x] == '#';
    }
    y++;
  }

  // Initialize corners as always on
  grid[0][0] = true;
  grid[0][gridSize - 1] = true;
  grid[gridSize - 1][0] = true;
  grid[gridSize - 1][gridSize - 1] = true;

  for (int i = 0; i < steps; i++) {
    grid = step(grid);
  }

  int onCount = 0;
  for (List<bool> row in grid) {
    for (bool light in row) {
      if (light) {
        onCount++;
      }
    }
  }

  print(onCount);
}