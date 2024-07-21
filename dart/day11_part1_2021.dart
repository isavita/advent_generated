
import 'dart:io';

void main() async {
  var grid = await readInput('input.txt');
  var totalFlashes = 0;

  for (var step = 0; step < 100; step++) {
    totalFlashes += simulateStep(grid);
  }

  print(totalFlashes);
}

Future<List<List<int>>> readInput(String filename) async {
  var lines = await File(filename).readAsLines();
  return lines.map((line) => line.split('').map(int.parse).toList()).toList();
}

int simulateStep(List<List<int>> grid) {
  var flashes = 0;
  var flashed = <List<int>>[];

  for (var row in grid) {
    for (var i = 0; i < row.length; i++) {
      row[i]++;
    }
  }

  for (var y = 0; y < grid.length; y++) {
    for (var x = 0; x < grid[y].length; x++) {
      if (grid[y][x] > 9) {
        flashes += flash(grid, x, y, flashed);
      }
    }
  }

  for (var coords in flashed) {
    grid[coords[0]][coords[1]] = 0;
  }

  return flashes;
}

int flash(List<List<int>> grid, int x, int y, List<List<int>> flashed) {
  if (flashed.any((coord) => coord[0] == y && coord[1] == x)) return 0;

  flashed.add([y, x]);
  var flashes = 1;
  var directions = [
    [-1, -1], [-1, 0], [-1, 1],
    [0, -1],          [0, 1],
    [1, -1], [1, 0], [1, 1]
  ];

  for (var dir in directions) {
    var newX = x + dir[1];
    var newY = y + dir[0];
    if (newX >= 0 && newX < grid[0].length && newY >= 0 && newY < grid.length) {
      grid[newY][newX]++;
      if (grid[newY][newX] > 9) {
        flashes += flash(grid, newX, newY, flashed);
      }
    }
  }

  return flashes;
}
