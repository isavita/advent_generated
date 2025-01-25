
import 'dart:io';

void main() {
  try {
    final file = File('input.txt');
    final lines = file.readAsLinesSync();

    int totalSum = 0;
    for (final line in lines) {
      totalSum += processLine(line);
    }

    print(totalSum);
  } catch (e) {
    print('Error reading input file: $e');
  }
}

int processLine(String line) {
  int lineSum = 0;
  final regex = RegExp(r'mul\(\s*(\d{1,3})\s*,\s*(\d{1,3})\s*\)');
  final matches = regex.allMatches(line);

  for (final match in matches) {
    final xStr = match.group(1);
    final yStr = match.group(2);

    if (xStr != null && yStr != null) {
      final x = int.tryParse(xStr);
      final y = int.tryParse(yStr);

      if (x != null && y != null) {
        lineSum += x * y;
      }
    }
  }
  return lineSum;
}
