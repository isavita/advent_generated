import 'dart:io';

void main() {
  List<String> input = File('input.txt').readAsLinesSync();
  List<int> range = input[0].split('-').map(int.parse).toList();

  int count1 = 0;
  int count2 = 0;

  for (int i = range[0]; i <= range[1]; i++) {
    String password = i.toString();

    if (hasAdjacentDigits(password) && hasNonDecreasingDigits(password)) {
      count1++;
      if (hasOnlyTwoAdjacentDigits(password)) {
        count2++;
      }
    }
  }

  print(count1);
  print(count2);
}

bool hasAdjacentDigits(String password) {
  for (int i = 0; i < password.length - 1; i++) {
    if (password[i] == password[i + 1]) {
      return true;
    }
  }
  return false;
}

bool hasNonDecreasingDigits(String password) {
  for (int i = 0; i < password.length - 1; i++) {
    if (int.parse(password[i]) > int.parse(password[i + 1])) {
      return false;
    }
  }
  return true;
}

bool hasOnlyTwoAdjacentDigits(String password) {
  Map<String, int> digitCount = {};
  for (int i = 0; i < password.length; i++) {
    digitCount.update(password[i], (value) => value + 1, ifAbsent: () => 1);
  }
  return digitCount.containsValue(2);
}