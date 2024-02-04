
import 'dart:io';

void main() {
  List<String> input = File('input.txt').readAsLinesSync();
  List<int> range = input[0].split('-').map(int.parse).toList();

  int count = 0;
  for (int i = range[0]; i <= range[1]; i++) {
    String password = i.toString();
    if (hasAdjacentDigits(password) && neverDecrease(password)) {
      count++;
    }
  }

  print(count);
}

bool hasAdjacentDigits(String password) {
  for (int i = 0; i < password.length - 1; i++) {
    if (password[i] == password[i + 1]) {
      return true;
    }
  }
  return false;
}

bool neverDecrease(String password) {
  for (int i = 0; i < password.length - 1; i++) {
    if (int.parse(password[i]) > int.parse(password[i + 1])) {
      return false;
    }
  }
  return true;
}
