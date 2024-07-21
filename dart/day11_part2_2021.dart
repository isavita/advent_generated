
import 'dart:io';
import 'dart:convert';

void main() async {
  var grid = await readInput('input.txt');
  int step = 0;

  while (true) {
    step++;
    if (simulateStep(grid) == 100) break;
  }

  print(step);
}

Future<List<List<int>>> readInput(String filename) async {
  var lines = await File(filename).readAsLines();
  return lines.map((line) => line.split('').map(int.parse).toList()).toList();
}

int simulateStep(List<List<int>> grid) {
  int flashes = 0;
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
  if (flashed.any((coords) => coords[0] == y && coords[1] == x)) return 0;

  flashed.add([y, x]);
  int flashCount = 1;
  var directions = [
    [-1, -1], [-1, 0], [-1, 1],
    [0, -1],          [0, 1],
    [1, -1], [1, 0], [1, 1]
  ];

  for (var dir in directions) {
    int newX = x + dir[1], newY = y + dir[0];
    if (newX >= 0 && newX < grid[0].length && newY >= 0 && newY < grid.length) {
      grid[newY][newX]++;
      if (grid[newY][newX] > 9) {
        flashCount += flash(grid, newX, newY, flashed);
      }
    }
  }

  return flashCount;
}
