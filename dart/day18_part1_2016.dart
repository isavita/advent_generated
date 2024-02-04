
import 'dart:io';

void main() {
  List<String> rows = File('input.txt').readAsLinesSync();
  int numRows = 40;
  
  int safeTiles = 0;
  
  for (int i = 0; i < numRows; i++) {
    safeTiles += rows[i].split('').where((tile) => tile == '.').length;
    
    List<String> newRow = [];
    
    for (int j = 0; j < rows[0].length; j++) {
      String left = j > 0 ? rows[i][j - 1] : '.';
      String center = rows[i][j];
      String right = j < rows[0].length - 1 ? rows[i][j + 1] : '.';
      
      if ((left == '^' && center == '^' && right == '.') ||
          (center == '^' && right == '^' && left == '.') ||
          (left == '^' && center == '.' && right == '.') ||
          (right == '^' && center == '.' && left == '.')) {
        newRow.add('^');
      } else {
        newRow.add('.');
      }
    }
    
    rows.add(newRow.join());
  }
  
  print(safeTiles);
}
