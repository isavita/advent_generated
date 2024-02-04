
import 'dart:io';

void main() {
  List<String> rows = File('input.txt').readAsLinesSync();
  int numRows = 400000;
  int numSafeTiles = 0;

  for (int i = 0; i < numRows; i++) {
    numSafeTiles += rows[i].split('').where((tile) => tile == '.').length;
    String newRow = '';
    for (int j = 0; j < rows[0].length; j++) {
      String left = j > 0 ? rows[i][j - 1] : '.';
      String center = rows[i][j];
      String right = j < rows[0].length - 1 ? rows[i][j + 1] : '.';
      if ((left == '^' && center == '^' && right == '.') ||
          (center == '^' && right == '^' && left == '.') ||
          (left == '^' && center == '.' && right == '.') ||
          (right == '^' && center == '.' && left == '.')) {
        newRow += '^';
      } else {
        newRow += '.';
      }
    }
    rows.add(newRow);
  }

  print(numSafeTiles);
}
