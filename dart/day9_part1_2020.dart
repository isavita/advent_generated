import 'dart:io';

void main() {
  List<int> numbers = File('input.txt').readAsLinesSync().map(int.parse).toList();

  int preambleLength = 25;
  int invalidNumber = 0;

  for (int i = preambleLength; i < numbers.length; i++) {
    bool found = false;
    for (int j = i - preambleLength; j < i - 1; j++) {
      for (int k = j + 1; k < i; k++) {
        if (numbers[j] + numbers[k] == numbers[i]) {
          found = true;
          break;
        }
      }
      if (found) break;
    }
    if (!found) {
      invalidNumber = numbers[i];
      break;
    }
  }

  print(invalidNumber);
}