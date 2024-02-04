
import 'dart:io';

const open = '.';
const trees = '|';
const lumberyard = '#';
const size = 50;

void main() {
  var grid = readInput('input.txt');
  var seenStates = <String, int>{};
  var cycleStart, cycleLength;
  for (var minute = 0;; minute++) {
    var state = gridToString(grid);
    if (seenStates.containsKey(state)) {
      cycleStart = seenStates[state]!;
      cycleLength = minute - seenStates[state]!;
      break;
    }
    seenStates[state] = minute;
    grid = transform(grid);
  }

  var remainingMinutes = (1000000000 - cycleStart) % cycleLength;
  for (var i = 0; i < remainingMinutes; i++) {
    grid = transform(grid);
  }

  var resources = countResources(grid);
  print(resources[0] * resources[1]);
}

List<List<String>> readInput(String filename) {
  var file = File(filename);
  var lines = file.readAsLinesSync();
  var grid = <List<String>>[];
  for (var line in lines) {
    grid.add(line.split(''));
  }
  return grid;
}

List<List<String>> transform(List<List<String>> grid) {
  var newGrid = List.generate(grid.length, (index) => List.filled(grid[index].length, ''));
  for (var i = 0; i < grid.length; i++) {
    for (var j = 0; j < grid[i].length; j++) {
      newGrid[i][j] = nextAcreState(grid, i, j);
    }
  }
  return newGrid;
}

String nextAcreState(List<List<String>> grid, int i, int j) {
  switch (grid[i][j]) {
    case open:
      if (countAdjacent(grid, i, j, trees) >= 3) {
        return trees;
      }
      break;
    case trees:
      if (countAdjacent(grid, i, j, lumberyard) >= 3) {
        return lumberyard;
      }
      break;
    case lumberyard:
      if (countAdjacent(grid, i, j, lumberyard) >= 1 && countAdjacent(grid, i, j, trees) >= 1) {
        return lumberyard;
      }
      return open;
  }
  return grid[i][j];
}

int countAdjacent(List<List<String>> grid, int i, int j, String acreType) {
  var count = 0;
  for (var x = -1; x <= 1; x++) {
    for (var y = -1; y <= 1; y++) {
      if (x == 0 && y == 0) continue;
      if (i + x >= 0 && i + x < grid.length && j + y >= 0 && j + y < grid[i].length && grid[i + x][j + y] == acreType) {
        count++;
      }
    }
  }
  return count;
}

List<int> countResources(List<List<String>> grid) {
  var wooded = 0;
  var lumberyards = 0;
  for (var i = 0; i < grid.length; i++) {
    for (var j = 0; j < grid[i].length; j++) {
      switch (grid[i][j]) {
        case trees:
          wooded++;
          break;
        case lumberyard:
          lumberyards++;
          break;
      }
    }
  }
  return [wooded, lumberyards];
}

String gridToString(List<List<String>> grid) {
  var sb = StringBuffer();
  for (var row in grid) {
    sb.writeAll(row);
    sb.write('\n');
  }
  return sb.toString();
}
