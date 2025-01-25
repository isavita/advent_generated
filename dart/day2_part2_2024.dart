
import 'dart:io';
import 'dart:convert';

bool isIncreasingReport(List<int> levels) {
  if (levels.length <= 1) return true;
  for (int i = 0; i < levels.length - 1; ++i) {
    int diff = levels[i + 1] - levels[i];
    if (diff <= 0 || diff > 3) return false;
  }
  return true;
}

bool isDecreasingReport(List<int> levels) {
  if (levels.length <= 1) return true;
  for (int i = 0; i < levels.length - 1; ++i) {
    int diff = levels[i + 1] - levels[i];
    if (diff >= 0 || diff < -3) return false;
  }
  return true;
}

bool isSafeReport(List<int> levels) {
  if (levels.isEmpty) return false; // Empty report is not safe
  return isIncreasingReport(levels) || isDecreasingReport(levels);
}

bool isSafeReportWithDampener(List<int> levels) {
  if (isSafeReport(levels)) return true;
  if (levels.length <= 2) return false;
  for (int i = 0; i < levels.length; ++i) {
    List<int> tempLevels = List<int>.from(levels);
    tempLevels.removeAt(i);
    if (isSafeReport(tempLevels)) return true;
  }
  return false;
}

void main() {
  int safeReportCountPart1 = 0;
  int safeReportCountPart2 = 0;
  File inputFile = File('input.txt');
  List<String> lines = inputFile.readAsLinesSync();

  for (String line in lines) {
    if (line.trim().isEmpty) continue; // Skip empty lines
    List<int> levels = line.split(' ').map(int.parse).toList();
    if (isSafeReport(levels)) {
      safeReportCountPart1++;
      safeReportCountPart2++; // Part 1 safe reports are also Part 2 safe
    } else if (isSafeReportWithDampener(levels)) {
      safeReportCountPart2++;
    }
  }

  print(safeReportCountPart2);
}
