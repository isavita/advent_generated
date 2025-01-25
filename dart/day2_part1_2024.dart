
import 'dart:io';

bool isSafe(List<int> levels) {
  if (levels.length <= 1) {
    return true; // Single level is always safe
  }

  bool isIncreasing = false;
  bool isDecreasing = false;

  // Check for increasing trend
  bool possibleIncreasing = true;
  for (int i = 1; i < levels.length; i++) {
    if (levels[i] <= levels[i - 1]) {
      possibleIncreasing = false;
      break;
    }
  }
  if (possibleIncreasing) {
    isIncreasing = true;
  }

  // Check for decreasing trend
  bool possibleDecreasing = true;
  if (!isIncreasing) {
    for (int i = 1; i < levels.length; i++) {
      if (levels[i] >= levels[i - 1]) {
        possibleDecreasing = false;
        break;
      }
    }
    if (possibleDecreasing) {
      isDecreasing = true;
    }
  }

  if (!isIncreasing && !isDecreasing) {
    return false; // Neither increasing nor decreasing
  }

  // Check difference constraint
  for (int i = 1; i < levels.length; i++) {
    int diff = (levels[i] - levels[i - 1]).abs();
    if (diff < 1 || diff > 3) {
      return false;
    }
  }

  return true;
}

void main() {
  int safeReportCount = 0;
  try {
    final file = File('input.txt');
    final lines = file.readAsLinesSync();

    for (final line in lines) {
      if (line.trim().isEmpty) {
        continue; // Skip empty lines
      }
      List<int> levels = line.split(' ').map(int.parse).toList();
      if (isSafe(levels)) {
        safeReportCount++;
      }
    }
  } catch (e) {
    print("Error reading input file: $e");
    exit(1);
  }

  print(safeReportCount);
}
