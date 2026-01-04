
import 'dart:io';

int floorDiv(int a, int b) {
  int q = a ~/ b;
  return (a < 0 && a % b != 0) ? q - 1 : q;
}

void main() {
  const int dialSize = 100;
  int currentPos = 50;
  int totalZeroHits = 0;

  final lines = File('input.txt').readAsLinesSync();
  for (final line in lines) {
    final trimmed = line.trim();
    if (trimmed.isEmpty) continue;
    final direction = trimmed[0];
    final amount = int.parse(trimmed.substring(1));
    switch (direction) {
      case 'R':
        totalZeroHits += (currentPos + amount) ~/ dialSize;
        currentPos = (currentPos + amount) % dialSize;
        break;
      case 'L':
        totalZeroHits += floorDiv(currentPos - 1, dialSize) - floorDiv(currentPos - amount - 1, dialSize);
        currentPos = (currentPos - amount) % dialSize;
        if (currentPos < 0) currentPos += dialSize;
        break;
    }
  }

  print(totalZeroHits);
}
