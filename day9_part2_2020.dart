
import 'dart:io';

void main() {
  List<int> numbers = File('input.txt').readAsLinesSync().map(int.parse).toList();

  int invalidNumber = findInvalidNumber(numbers, 25);
  print(findEncryptionWeakness(numbers, invalidNumber));
}

int findInvalidNumber(List<int> numbers, int preambleLength) {
  for (int i = preambleLength; i < numbers.length; i++) {
    bool found = false;
    for (int j = i - preambleLength; j < i; j++) {
      for (int k = j + 1; k < i; k++) {
        if (numbers[j] + numbers[k] == numbers[i]) {
          found = true;
          break;
        }
      }
      if (found) break;
    }
    if (!found) return numbers[i];
  }
  return -1;
}

int findEncryptionWeakness(List<int> numbers, int target) {
  for (int i = 0; i < numbers.length; i++) {
    int sum = numbers[i];
    int min = sum;
    int max = sum;
    for (int j = i + 1; j < numbers.length; j++) {
      sum += numbers[j];
      min = numbers[j] < min ? numbers[j] : min;
      max = numbers[j] > max ? numbers[j] : max;
      if (sum == target) {
        return min + max;
      } else if (sum > target) {
        break;
      }
    }
  }
  return -1;
}
