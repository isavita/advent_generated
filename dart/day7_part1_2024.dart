
import 'dart:io';

void main() {
  final inputFile = File('input.txt');
  int totalCalibrationResult = 0;

  try {
    final lines = inputFile.readAsLinesSync();
    for (final line in lines) {
      final parts = line.split(':');
      if (parts.length != 2) {
        continue; // Skip malformed lines
      }

      final target = int.tryParse(parts[0].trim());
      final numbersStr = parts[1].trim().split(' ');
      final numbers = numbersStr.map(int.parse).toList();

      if (target == null || numbers.isEmpty) {
        continue; // Skip malformed lines
      }

      if (isEquationSolvable(target, numbers)) {
        totalCalibrationResult += target;
      }
    }
  } catch (e) {
    print('Error reading input file: $e');
    return;
  }

  print(totalCalibrationResult);
}

bool isEquationSolvable(int target, List<int> numbers) {
  if (numbers.length == 1) {
    return numbers[0] == target;
  }

  return _checkCombinations(target, numbers, 0, numbers[0]);
}

bool _checkCombinations(int target, List<int> numbers, int index, int currentValue) {
  if (index == numbers.length - 1) {
    return currentValue == target;
  }

  // Try addition
  if (_checkCombinations(target, numbers, index + 1, currentValue + numbers[index + 1])) {
    return true;
  }

  // Try multiplication
  if (_checkCombinations(target, numbers, index + 1, currentValue * numbers[index + 1])) {
    return true;
  }

  return false;
}
