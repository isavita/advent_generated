
import 'dart:io';

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  int serialNumber = int.parse(lines[0]);

  List<List<int>> grid = List.generate(301, (_) => List<int>.filled(301, 0));

  for (int y = 1; y <= 300; y++) {
    for (int x = 1; x <= 300; x++) {
      int rackId = x + 10;
      int powerLevel = rackId * y;
      powerLevel += serialNumber;
      powerLevel *= rackId;
      powerLevel = (powerLevel ~/ 100) % 10;
      powerLevel -= 5;
      grid[y][x] = powerLevel + grid[y - 1][x] + grid[y][x - 1] - grid[y - 1][x - 1];
    }
  }

  int maxPower = 0;
  int maxX = 0;
  int maxY = 0;
  int maxSize = 0;

  for (int size = 1; size <= 300; size++) {
    for (int y = 1; y <= 300 - size + 1; y++) {
      for (int x = 1; x <= 300 - size + 1; x++) {
        int totalPower = grid[y + size - 1][x + size - 1] - grid[y - 1][x + size - 1] - grid[y + size - 1][x - 1] + grid[y - 1][x - 1];
        if (totalPower > maxPower) {
          maxPower = totalPower;
          maxX = x;
          maxY = y;
          maxSize = size;
        }
      }
    }
  }

  print('$maxX,$maxY,$maxSize');
}
