
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
    final emptyCols = getEmptyCols();
    final emptyRows = getEmptyRows();
    final numLinesToAdd = expansionFactor - 1;

    Map<Coord, String> newData = {};

    for (var entry in data.entries) {
      final coord = entry.key;
      final value = entry.value;

      final dX = emptyCols.where((col) => col < coord.x).length * numLinesToAdd;
      final dY = emptyRows.where((row) => row < coord.y).length * numLinesToAdd;

      newData[Coord(coord.x + dX, coord.y + dY)] = value;
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

int solve(List<String> input, int expansionFactor) {
  final grid = Grid.buildGrid(input, '.');
  final expandedGrid = grid.expandGrid(expansionFactor);

  int res = 0;
  final coords = expandedGrid.data.keys.toList();

  for (int i = 0; i < coords.length; i++) {
    for (int j = i + 1; j < coords.length; j++) {
      res += calculateLength(coords[i], coords[j]);
    }
  }

  return res;
}

void main() {
  final input = File('input.txt').readAsLinesSync();
  print(solve(input, 1000000));
}
