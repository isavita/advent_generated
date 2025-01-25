
import 'dart:io';
import 'dart:math';

class Tile {
  final int id;
  List<List<String>> contents;

  Tile(this.id, this.contents);
}

List<Tile> parseTilesFromInput(String input) {
  final tiles = <Tile>[];
  for (final block in input.split('\n\n')) {
    final lines = block.split('\n');
    final tileID = int.parse(lines[0].substring(5, lines[0].length - 1));
    final contents = <List<String>>[];
    for (final line in lines.sublist(1)) {
      contents.add(line.split(''));
    }
    tiles.add(Tile(tileID, contents));
  }
  return tiles;
}

String getCol(List<List<String>> grid, bool firstCol) {
  final buffer = StringBuffer();
  for (final row in grid) {
    buffer.write(firstCol ? row.first : row.last);
  }
  return buffer.toString();
}

String getRow(List<List<String>> grid, bool firstRow) {
  final buffer = StringBuffer();
  final row = firstRow ? grid.first : grid.last;
  for (final cell in row) {
    buffer.write(cell);
  }
  return buffer.toString();
}

List<List<String>> removeBordersFromGrid(List<List<String>> grid) {
  final result = <List<String>>[];
  for (int i = 1; i < grid.length - 1; i++) {
    result.add([]);
    for (int j = 1; j < grid[0].length - 1; j++) {
      result.last.add(grid[i][j]);
    }
  }
  return result;
}

const monster = '''                  # 
#    ##    ##    ###
 #  #  #  #  #  #   ''';

List<List<int>> findMonsterCoords(List<List<String>> image) {
  final monsterOffsets = <List<int>>[];
  final monsterLines = monster.split('\n');
  final monsterHeight = monsterLines.length;
  final monsterLength = monsterLines.first.length;
  for (int r = 0; r < monsterHeight; r++) {
    for (int c = 0; c < monsterLength; c++) {
      if (monsterLines[r][c] == '#') {
        monsterOffsets.add([r, c]);
      }
    }
  }
  final monsterStartingCoords = <List<int>>[];
  for (int r = 0; r < image.length - monsterHeight + 1; r++) {
    for (int c = 0; c < image[0].length - monsterLength + 1; c++) {
      bool monsterFound = true;
      for (final diff in monsterOffsets) {
        if (image[r + diff[0]][c + diff[1]] != '#') {
          monsterFound = false;
          break;
        }
      }
      if (monsterFound) {
        monsterStartingCoords.add([r, c]);
      }
    }
  }
  final monsterCoords = <List<int>>[];
  for (final startingCoord in monsterStartingCoords) {
    for (final diff in monsterOffsets) {
      monsterCoords.add([startingCoord[0] + diff[0], startingCoord[1] + diff[1]]);
    }
  }
  return monsterCoords;
}

List<List<List<String>>> allGridOrientations(List<List<String>> grid) {
  final orientations = <List<List<String>>>[grid];
  for (int i = 0; i < 3; i++) {
    orientations.add(rotateStringGrid(orientations.last));
  }
  for (int i = 0; i < 4; i++) {
    orientations.add(mirrorStringGrid(orientations[i]));
  }
  return orientations;
}

List<List<String>> rotateStringGrid(List<List<String>> grid) {
  final rotated = List.generate(
      grid[0].length, (index) => List.filled(grid.length, ''));
  for (int i = 0; i < grid.length; i++) {
    for (int j = 0; j < grid[0].length; j++) {
      rotated[grid[0].length - 1 - j][i] = grid[i][j];
    }
  }
  return rotated;
}

List<List<String>> mirrorStringGrid(List<List<String>> grid) {
  final flipped = <List<String>>[];
  for (final row in grid) {
    flipped.add(row.reversed.toList());
  }
  return flipped;
}

List<List<Tile>> backtrackAssemble(
    List<Tile> tiles, List<List<Tile>>? assembledTiles, Set<int> usedIndices) {
  final edgeSize = sqrt(tiles.length).toInt();
  assembledTiles ??=
      List.generate(edgeSize, (index) => List.filled(edgeSize, Tile(0, [])));
  for (int row = 0; row < edgeSize; row++) {
    for (int col = 0; col < edgeSize; col++) {
      if (assembledTiles[row][col].id == 0) {
        for (int i = 0; i < tiles.length; i++) {
          if (!usedIndices.contains(i)) {
            for (final opt in allGridOrientations(tiles[i].contents)) {
              if (row != 0) {
                final currentTopRow = getRow(opt, true);
                final bottomOfAbove =
                    getRow(assembledTiles[row - 1][col].contents, false);
                if (currentTopRow != bottomOfAbove) {
                  continue;
                }
              }
              if (col != 0) {
                final currentLeftCol = getCol(opt, true);
                final rightColOfLeft =
                    getCol(assembledTiles[row][col - 1].contents, false);
                if (currentLeftCol != rightColOfLeft) {
                  continue;
                }
              }
              tiles[i].contents = opt;
              assembledTiles[row][col] = tiles[i];
              usedIndices.add(i);
              final recurseResult = backtrackAssemble(
                  tiles, assembledTiles, usedIndices);
              if (recurseResult.isNotEmpty) {
                return recurseResult;
              }
              assembledTiles[row][col] = Tile(0, []);
              usedIndices.remove(i);
            }
          }
        }
        if (assembledTiles[row][col].id == 0) {
          return [];
        }
      }
    }
  }
  return assembledTiles;
}

int solve(String input) {
  final tiles = parseTilesFromInput(input);
  final edgeSize = sqrt(tiles.length).toInt();
  final assembledTiles =
      backtrackAssemble(tiles, null, {});
  for (int r = 0; r < edgeSize; r++) {
    for (int c = 0; c < edgeSize; c++) {
      assembledTiles[r][c].contents =
          removeBordersFromGrid(assembledTiles[r][c].contents);
    }
  }
  final image = <List<String>>[];
  for (int bigRow = 0; bigRow < edgeSize; bigRow++) {
    for (int subRow = 0;
        subRow < assembledTiles[0][0].contents.length;
        subRow++) {
      image.add([]);
      for (int bigCol = 0; bigCol < edgeSize; bigCol++) {
        final subLine = assembledTiles[bigRow][bigCol].contents[subRow];
        image.last.addAll(subLine);
      }
    }
  }
  List<List<String>> finalImage = [];
  for (final opt in allGridOrientations(image)) {
    final monsterCoords = findMonsterCoords(opt);
    if (monsterCoords.isNotEmpty) {
      finalImage = opt;
      break;
    }
  }
  for (final coord in findMonsterCoords(finalImage)) {
    finalImage[coord[0]][coord[1]] = 'O';
  }
  int roughWatersCount = 0;
  for (final row in finalImage) {
    for (final cell in row) {
      if (cell == '#') {
        roughWatersCount++;
      }
    }
  }
  return roughWatersCount;
}

void main() {
  final input = File('input.txt').readAsStringSync().trim();
  final ans = solve(input);
  print(ans);
}
