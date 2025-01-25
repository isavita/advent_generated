
import 'dart:io';
import 'dart:math';

class Tile {
  final int id;
  List<List<String>> contents;
  Tile(this.id, this.contents);
}

List<Tile> parseTilesFromInput(String input) {
  List<Tile> ans = [];
  for (String block in input.split('\n\n')) {
    List<String> split = block.split('\n');
    int tileID = int.parse(split[0].substring(5, split[0].length - 1));
    List<List<String>> contents = [];
    for (String line in split.sublist(1)) {
      contents.add(line.split(''));
    }
    ans.add(Tile(tileID, contents));
  }
  return ans;
}

String getCol(List<List<String>> grid, bool firstCol) {
  String str = '';
  for (int i = 0; i < grid.length; i++) {
    str += firstCol ? grid[i][0] : grid[i][grid[0].length - 1];
  }
  return str;
}

String getRow(List<List<String>> grid, bool firstRow) {
  String str = '';
  for (int i = 0; i < grid[0].length; i++) {
    str += firstRow ? grid[0][i] : grid[grid.length - 1][i];
  }
  return str;
}

List<List<String>> rotateStringGrid(List<List<String>> grid) {
  List<List<String>> rotated =
      List.generate(grid[0].length, (_) => List<String>.filled(grid.length, ''));
  for (int i = 0; i < grid.length; i++) {
    for (int j = 0; j < grid[0].length; j++) {
      rotated[grid[0].length - 1 - j][i] = grid[i][j];
    }
  }
  return rotated;
}

List<List<String>> mirrorStringGrid(List<List<String>> grid) {
  List<List<String>> flipped = [];
  for (int i = 0; i < grid.length; i++) {
    flipped.add([]);
    for (int j = grid[i].length - 1; j >= 0; j--) {
      flipped[i].add(grid[i][j]);
    }
  }
  return flipped;
}

List<List<List<String>>> allGridOrientations(List<List<String>> grid) {
  List<List<List<String>>> orientations = [grid];
  for (int i = 0; i < 3; i++) {
    orientations
        .add(rotateStringGrid(orientations[orientations.length - 1]));
  }
  for (int i = 0; i < 4; i++) {
    orientations.add(mirrorStringGrid(orientations[i]));
  }
  return orientations;
}

List<List<Tile?>>? backtrackAssemble(
    List<Tile> tiles, List<List<Tile?>>? assembledTiles, Set<int> usedIndices) {
  int edgeSize = sqrt(tiles.length).toInt();
  if (assembledTiles == null) {
    assembledTiles =
        List.generate(edgeSize, (_) => List<Tile?>.filled(edgeSize, null));
  }
  for (int row = 0; row < edgeSize; row++) {
    for (int col = 0; col < edgeSize; col++) {
      if (assembledTiles[row][col] == null) {
        for (int i = 0; i < tiles.length; i++) {
          if (!usedIndices.contains(i)) {
            for (List<List<String>> opt
                in allGridOrientations(tiles[i].contents)) {
              if (row != 0) {
                String currentTopRow = getRow(opt, true);
                String bottomOfAbove =
                    getRow(assembledTiles[row - 1][col]!.contents, false);
                if (currentTopRow != bottomOfAbove) {
                  continue;
                }
              }
              if (col != 0) {
                String currentLeftCol = getCol(opt, true);
                String rightColOfLeft =
                    getCol(assembledTiles[row][col - 1]!.contents, false);
                if (currentLeftCol != rightColOfLeft) {
                  continue;
                }
              }
              tiles[i].contents = opt;
              assembledTiles[row][col] = tiles[i];
              usedIndices.add(i);
              List<List<Tile?>>? recurseResult =
                  backtrackAssemble(tiles, assembledTiles, usedIndices);
              if (recurseResult != null) {
                return recurseResult;
              }
              assembledTiles[row][col] = null;
              usedIndices.remove(i);
            }
          }
        }
        if (assembledTiles[row][col] == null) {
          return null;
        }
      }
    }
  }
  return assembledTiles;
}

int solve(String input) {
  List<Tile> tiles = parseTilesFromInput(input);
  List<List<Tile?>>? assembledTiles =
      backtrackAssemble(tiles, null, <int>{});
  int edgeSize = sqrt(tiles.length).toInt();
  int product = assembledTiles![0][0]!.id;
  product *= assembledTiles[0][edgeSize - 1]!.id;
  product *= assembledTiles[edgeSize - 1][0]!.id;
  product *= assembledTiles[edgeSize - 1][edgeSize - 1]!.id;
  return product;
}

void main() {
  String input = File('input.txt').readAsStringSync().trim();
  int ans = solve(input);
  print(ans);
}
