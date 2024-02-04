
import 'dart:io';

void main() {
  List<String> input = File('input.txt').readAsLinesSync();
  List<String> grid = List.from(input);

  int calculateBiodiversityRating(List<String> grid) {
    int rating = 0;
    int index = 0;

    for (int i = 0; i < grid.length; i++) {
      for (int j = 0; j < grid[i].length; j++) {
        if (grid[i][j] == '#') {
          rating += 1 << index;
        }
        index++;
      }
    }

    return rating;
  }

  List<String> getNextMinute(List<String> grid) {
    List<String> newGrid = List.from(grid);

    for (int i = 0; i < grid.length; i++) {
      String row = '';
      for (int j = 0; j < grid[i].length; j++) {
        int bugsAdjacent = 0;

        if (i > 0 && grid[i - 1][j] == '#') bugsAdjacent++;
        if (i < grid.length - 1 && grid[i + 1][j] == '#') bugsAdjacent++;
        if (j > 0 && grid[i][j - 1] == '#') bugsAdjacent++;
        if (j < grid[i].length - 1 && grid[i][j + 1] == '#') bugsAdjacent++;

        if (grid[i][j] == '#' && bugsAdjacent != 1) {
          row += '.';
        } else if (grid[i][j] == '.' && (bugsAdjacent == 1 || bugsAdjacent == 2)) {
          row += '#';
        } else {
          row += grid[i][j];
        }
      }
      newGrid[i] = row;
    }

    return newGrid;
  }

  Set<String> layouts = {};
  String layout = grid.join('');

  while (!layouts.contains(layout)) {
    layouts.add(layout);
    grid = getNextMinute(grid);
    layout = grid.join('');
  }

  print(calculateBiodiversityRating(grid));
}
