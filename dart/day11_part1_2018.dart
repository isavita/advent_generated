
import 'dart:io';

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  int serialNumber = int.parse(lines[0]);

  int maxPower = 0;
  int maxX = 0;
  int maxY = 0;

  for (int x = 1; x <= 298; x++) {
    for (int y = 1; y <= 298; y++) {
      int power = 0;
      for (int i = x; i < x + 3; i++) {
        for (int j = y; j < y + 3; j++) {
          int rackId = i + 10;
          int level = rackId * j;
          level += serialNumber;
          level *= rackId;
          level = (level ~/ 100) % 10;
          level -= 5;
          power += level;
        }
      }
      if (power > maxPower) {
        maxPower = power;
        maxX = x;
        maxY = y;
      }
    }
  }

  print('$maxX,$maxY');
}
