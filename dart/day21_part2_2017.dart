
import 'dart:io';

void main() {
  List<String> rules = [];
  List<String> grid = [
    '.#.',
    '..#',
    '###'
  ];

  File file = File('input.txt');
  List<String> lines = file.readAsLinesSync();

  for (String line in lines) {
    List<String> parts = line.split(' => ');
    rules.add(parts[0] + '=>' + parts[1]);
  }

  for (int i = 0; i < 18; i++) {
    int size = grid.length;
    int subSize = size % 2 == 0 ? 2 : 3;
    int newSize = size ~/ subSize * (subSize + 1);

    List<String> newGrid = List.generate(newSize, (index) => '');

    for (int r = 0; r < size; r += subSize) {
      for (int c = 0; c < size; c += subSize) {
        List<String> subGrid = [];

        for (int sr = 0; sr < subSize; sr++) {
          subGrid.add(grid[r + sr].substring(c, c + subSize));
        }

        List<String> newSubGrid = enhance(subGrid, rules);

        for (int sr = 0; sr < newSubGrid.length; sr++) {
          newGrid[r ~/ subSize * (subSize + 1) + sr] += newSubGrid[sr];
        }
      }
    }

    grid = newGrid;
  }

  int count = 0;
  for (String row in grid) {
    for (int i = 0; i < row.length; i++) {
      if (row[i] == '#') {
        count++;
      }
    }
  }

  print(count);
}

List<String> enhance(List<String> grid, List<String> rules) {
  for (String rule in rules) {
    List<String> parts = rule.split('=>').map((e) => e.trim()).toList();
    List<String> pattern = parts[0].split('/').map((e) => e.trim()).toList();
    List<String> output = parts[1].split('/').map((e) => e.trim()).toList();

    for (int i = 0; i < 4; i++) {
      if (grid.join('/') == pattern.join('/')) {
        return output;
      }

      List<String> flipped = flip(grid);
      if (flipped.join('/') == pattern.join('/')) {
        return output;
      }

      grid = rotate(grid);
    }
  }

  return [];
}

List<String> rotate(List<String> grid) {
  List<String> newGrid = List.generate(grid.length, (index) => '');

  for (int r = 0; r < grid.length; r++) {
    for (int c = 0; c < grid.length; c++) {
      newGrid[c] = grid[r][c] + newGrid[c];
    }
  }

  return newGrid;
}

List<String> flip(List<String> grid) {
  return grid.reversed.toList();
}
