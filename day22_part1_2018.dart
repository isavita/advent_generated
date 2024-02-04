
import 'dart:io';

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  int depth = int.parse(lines[0].split(' ')[1]);
  List<int> target = lines[1].split(' ')[1].split(',').map(int.parse).toList();

  int riskLevel = 0;
  List<List<int>> erosionLevels = List.generate(target[1] + 1, (i) => List<int>.filled(target[0] + 1, 0));

  for (int y = 0; y <= target[1]; y++) {
    for (int x = 0; x <= target[0]; x++) {
      int geologicIndex;
      if ((x == 0 && y == 0) || (x == target[0] && y == target[1])) {
        geologicIndex = 0;
      } else if (y == 0) {
        geologicIndex = x * 16807;
      } else if (x == 0) {
        geologicIndex = y * 48271;
      } else {
        geologicIndex = erosionLevels[y][x - 1] * erosionLevels[y - 1][x];
      }
      int erosionLevel = (geologicIndex + depth) % 20183;
      erosionLevels[y][x] = erosionLevel;
      riskLevel += erosionLevel % 3;
    }
  }

  print(riskLevel);
}
