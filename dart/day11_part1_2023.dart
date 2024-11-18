
import 'dart:io';
import 'dart:math';

class Coord {
  final int x;
  final int y;

  Coord(this.x, this.y);

  @override
  bool operator ==(Object other) =>
      other is Coord && x == other.x && y == other.y;

  @override
  int get hashCode => x.hashCode ^ y.hashCode;
}

class Grid {
  final int width;
  final int height;
  final Map<Coord, String> data;

  Grid(this.width, this.height, this.data);

  static Grid buildGrid(List<String> input, String empty) {
    Map<Coord, String> gridData = {};
    
    for (int y = 0; y < input.length; y++) {
      for (int x = 0; x < input[y].length; x++) {
        if (input[y][x] != empty) {
          gridData[Coord(x, y)] = input[y][x];
        }
      }
    }

    return Grid(input[0].length, input.length, gridData);
  }

  List<int> getEmptyRows() {
    List<int> emptyRows = [];
    for (int y = 0; y < height; y++) {
      if (!data.keys.any((coord) => coord.y == y)) {
        emptyRows.add(y);
      }
    }
    return emptyRows;
  }

  List<int> getEmptyCols() {
    List<int> emptyCols = [];
    for (int x = 0; x < width; x++) {
      if (!data.keys.any((coord) => coord.x == x)) {
        emptyCols.add(x);
      }
    }
    return emptyCols;
  }

  Grid expandGrid(int expansionFactor) {
    List<int> emptyCols = getEmptyCols();
    List<int> emptyRows = getEmptyRows();
    int numLinesToAdd = expansionFactor - 1;

    Map<Coord, String> newData = {};

    for (var entry in data.entries) {
      Coord oldCoord = entry.key;
      String value = entry.value;

      int dX = emptyCols.where((col) => col < oldCoord.x).length * numLinesToAdd;
      int dY = emptyRows.where((row) => row < oldCoord.y).length * numLinesToAdd;

      Coord newCoord = Coord(oldCoord.x + dX, oldCoord.y + dY);
      newData[newCoord] = value;
    }

    return Grid(
      width + emptyCols.length * numLinesToAdd, 
      height + emptyRows.length * numLinesToAdd, 
      newData
    );
  }
}

int calculateLength(Coord c1, Coord c2) {
  return (c1.x - c2.x).abs() + (c1.y - c2.y).abs();
}

int solve(List<String> input) {
  Grid grid = Grid.buildGrid(input, '.');
  Grid expandedGrid = grid.expandGrid(2);

  int res = 0;
  List<Coord> galaxies = expandedGrid.data.keys.toList();

  for (int i = 0; i < galaxies.length; i++) {
    for (int j = i + 1; j < galaxies.length; j++) {
      res += calculateLength(galaxies[i], galaxies[j]);
    }
  }

  return res;
}

void main() {
  List<String> input = File('input.txt').readAsLinesSync();
  print(solve(input));
}
