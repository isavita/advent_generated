
import 'dart:io';

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  int rowLength = lines[0].length;
  int col = 0;
  int trees = 0;

  for (int i = 1; i < lines.length; i++) {
    col = (col + 3) % rowLength;
    if (lines[i][col] == '#') {
      trees++;
    }
  }

  print(trees);
}
