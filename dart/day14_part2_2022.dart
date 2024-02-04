
import 'dart:io';

void main() {
  var file = File('input.txt');
  var input = file.readAsStringSync().trim();
  var matrix = parseInput(input);
  var originCol = 0;
  for (var i = 0; i < matrix[0].length; i++) {
    if (matrix[0][i] == '+') {
      originCol = i;
    }
    matrix[matrix.length - 1][i] = '#';
  }

  var ans = 0;
  while (!dropSand(matrix, originCol)) {
    ans++;
    if (matrix[0][originCol] == 'o') {
      break;
    }
  }

  print(ans);
}

List<List<String>> parseInput(String input) {
  var coordSets = <List<List<int>>>[];
  var lowestCol = 2147483647;
  var highestRow = 0;

  var lines = input.split('\n');
  for (var line in lines) {
    var rawCoords = line.split(' -> ');
    var coords = <List<int>>[];
    for (var rawCoord in rawCoords) {
      var rawNums = rawCoord.split(',');
      var col = int.parse(rawNums[0]);
      var row = int.parse(rawNums[1]);
      coords.add([col, row]);
      lowestCol = lowestCol < col ? lowestCol : col;
      highestRow = highestRow > row ? highestRow : row;
    }
    coordSets.add(coords);
  }

  var ExtraLeftSpace = 200;
  var highestCol = 0;
  for (var set in coordSets) {
    for (var i = 0; i < set.length; i++) {
      set[i][0] -= lowestCol - ExtraLeftSpace;
      highestCol = highestCol > set[i][0] ? highestCol : set[i][0];
    }
  }

  var matrix = List.generate(highestRow + 3, (index) => List.filled(highestCol + ExtraLeftSpace * 2, '.'));
  for (var set in coordSets) {
    for (var i = 1; i < set.length; i++) {
      var cols = [set[i - 1][0], set[i][0]];
      var rows = [set[i - 1][1], set[i][1]];
      cols.sort();
      rows.sort();

      if (cols[0] == cols[1]) {
        for (var r = rows[0]; r <= rows[1]; r++) {
          matrix[r][cols[0]] = '#';
        }
      } else if (rows[0] == rows[1]) {
        for (var c = cols[0]; c <= cols[1]; c++) {
          matrix[rows[0]][c] = '#';
        }
      }
    }
  }

  var originCol = 500 - lowestCol + ExtraLeftSpace;
  matrix[0][originCol] = '+';

  for (var i = 0; i < matrix.length; i++) {
    for (var j = 0; j < matrix[i].length; j++) {
      if (matrix[i][j] == '') {
        matrix[i][j] = '.';
      }
    }
  }

  return matrix;
}

bool dropSand(List<List<String>> matrix, int originCol) {
  var r = 0;
  var c = originCol;

  while (r < matrix.length - 1) {
    var below = matrix[r + 1][c];
    var diagonallyLeft = matrix[r + 1][c - 1];
    var diagonallyRight = matrix[r + 1][c + 1];
    if (below == '.') {
      r++;
    } else if (diagonallyLeft == '.') {
      r++;
      c--;
    } else if (diagonallyRight == '.') {
      r++;
      c++;
    } else {
      matrix[r][c] = 'o';
      return false;
    }
  }

  return true;
}
