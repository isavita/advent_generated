import 'dart:io';

void main() {
  List<List<bool>> screen = List.generate(6, (_) => List.filled(50, false));

  List<String> instructions = File('input.txt').readAsLinesSync();

  for (String instruction in instructions) {
    if (instruction.startsWith('rect')) {
      List<int> dimensions = instruction.substring(5).split('x').map(int.parse).toList();
      for (int i = 0; i < dimensions[1]; i++) {
        for (int j = 0; j < dimensions[0]; j++) {
          screen[i][j] = true;
        }
      }
    } else if (instruction.startsWith('rotate row')) {
      List<String> parts = instruction.substring(13).split(' by ');
      int row = int.parse(parts[0]);
      int amount = int.parse(parts[1]);
      List<bool> newRow = List.generate(50, (_) => false);
      for (int i = 0; i < 50; i++) {
        newRow[(i + amount) % 50] = screen[row][i];
      }
      screen[row] = newRow;
    } else if (instruction.startsWith('rotate column')) {
      List<String> parts = instruction.substring(16).split(' by ');
      int col = int.parse(parts[0]);
      int amount = int.parse(parts[1]);
      List<bool> newCol = List.generate(6, (_) => false);
      for (int i = 0; i < 6; i++) {
        newCol[(i + amount) % 6] = screen[i][col];
      }
      for (int i = 0; i < 6; i++) {
        screen[i][col] = newCol[i];
      }
    }
  }

  int litPixels = 0;
  for (int i = 0; i < 6; i++) {
    for (int j = 0; j < 50; j++) {
      if (screen[i][j]) {
        litPixels++;
      }
    }
  }

  print(litPixels);
}