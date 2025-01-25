
import 'dart:io';

void main() {
  final grid = readInput('input.txt');

  // Part 1
  final tiltedNorthGrid = tiltNorth(grid.map((row) => row.toList()).toList());
  final loadPart1 = calculateLoad(tiltedNorthGrid);
  print(loadPart1);

  // Part 2
  List<List<String>> currentGrid = grid.map((row) => row.toList()).toList();
  final seenGrids = <String, int>{};
  final loads = <int>[];

  for (int cycle = 0; cycle < 1000000000; cycle++) {
    currentGrid = spinCycle(currentGrid);
    final gridString = gridToString(currentGrid);

    if (seenGrids.containsKey(gridString)) {
      final firstSeenCycle = seenGrids[gridString]!;
      final cycleLength = cycle - firstSeenCycle;
      final remainingCycles = 1000000000 - 1 - cycle;
      final remainingCyclesMod = remainingCycles % cycleLength;

      final finalLoad = loads[firstSeenCycle + remainingCyclesMod];
      print(finalLoad);
      return;
    }

    seenGrids[gridString] = cycle;
    loads.add(calculateLoad(currentGrid));
  }

  print(calculateLoad(currentGrid));
}

List<List<String>> readInput(String filename) {
  final file = File(filename);
  return file.readAsLinesSync().map((line) => line.split('')).toList();
}

List<List<String>> tiltNorth(List<List<String>> grid) {
  final rows = grid.length;
  final cols = grid[0].length;

  for (int j = 0; j < cols; j++) {
    int nextEmptyRow = 0;
    for (int i = 0; i < rows; i++) {
      if (grid[i][j] == 'O') {
        if (i != nextEmptyRow) {
          grid[nextEmptyRow][j] = 'O';
          grid[i][j] = '.';
        }
        nextEmptyRow++;
      } else if (grid[i][j] == '#') {
        nextEmptyRow = i + 1;
      }
    }
  }
  return grid;
}

List<List<String>> tiltWest(List<List<String>> grid) {
  final rows = grid.length;
  final cols = grid[0].length;

  for (int i = 0; i < rows; i++) {
    int nextEmptyCol = 0;
    for (int j = 0; j < cols; j++) {
      if (grid[i][j] == 'O') {
        if (j != nextEmptyCol) {
          grid[i][nextEmptyCol] = 'O';
          grid[i][j] = '.';
        }
        nextEmptyCol++;
      } else if (grid[i][j] == '#') {
        nextEmptyCol = j + 1;
      }
    }
  }
  return grid;
}

List<List<String>> tiltSouth(List<List<String>> grid) {
  final rows = grid.length;
  final cols = grid[0].length;

  for (int j = 0; j < cols; j++) {
    int nextEmptyRow = rows - 1;
    for (int i = rows - 1; i >= 0; i--) {
      if (grid[i][j] == 'O') {
        if (i != nextEmptyRow) {
          grid[nextEmptyRow][j] = 'O';
          grid[i][j] = '.';
        }
        nextEmptyRow--;
      } else if (grid[i][j] == '#') {
        nextEmptyRow = i - 1;
      }
    }
  }
  return grid;
}

List<List<String>> tiltEast(List<List<String>> grid) {
  final rows = grid.length;
  final cols = grid[0].length;

  for (int i = 0; i < rows; i++) {
    int nextEmptyCol = cols - 1;
    for (int j = cols - 1; j >= 0; j--) {
      if (grid[i][j] == 'O') {
        if (j != nextEmptyCol) {
          grid[i][nextEmptyCol] = 'O';
          grid[i][j] = '.';
        }
        nextEmptyCol--;
      } else if (grid[i][j] == '#') {
        nextEmptyCol = j - 1;
      }
    }
  }
  return grid;
}

int calculateLoad(List<List<String>> grid) {
  final rows = grid.length;
  int totalLoad = 0;
  for (int i = 0; i < rows; i++) {
    for (int j = 0; j < grid[i].length; j++) {
      if (grid[i][j] == 'O') {
        totalLoad += rows - i;
      }
    }
  }
  return totalLoad;
}

List<List<String>> spinCycle(List<List<String>> grid) {
  List<List<String>> currentGrid = grid;
  currentGrid = tiltNorth(currentGrid);
  currentGrid = tiltWest(currentGrid);
  currentGrid = tiltSouth(currentGrid);
  currentGrid = tiltEast(currentGrid);
  return currentGrid;
}

String gridToString(List<List<String>> grid) {
  return grid.map((row) => row.join('')).join('\n');
}
