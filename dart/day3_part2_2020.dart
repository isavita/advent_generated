
import 'dart:io';

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  
  int result1 = countTrees(lines, 3, 1);
  int result2 = countTrees(lines, 1, 1);
  int result3 = countTrees(lines, 5, 1);
  int result4 = countTrees(lines, 7, 1);
  int result5 = countTrees(lines, 1, 2);
  
  print(result1 * result2 * result3 * result4 * result5);
}

int countTrees(List<String> lines, int right, int down) {
  int trees = 0;
  int x = 0;
  
  for (int y = 0; y < lines.length; y += down) {
    if (lines[y][x % lines[y].length] == '#') {
      trees++;
    }
    x += right;
  }
  
  return trees;
}
