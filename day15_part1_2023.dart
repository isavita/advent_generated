
import 'dart:io';

void main() {
  File file = File('input.txt');
  List<String> lines = file.readAsLinesSync();
  String input = lines[0];

  List<String> steps = input.split(',');

  int sum = 0;
  for (String step in steps) {
    int result = hashAlgorithm(step);
    sum += result;
  }

  print(sum);
}

int hashAlgorithm(String input) {
  int current = 0;
  for (int i = 0; i < input.length; i++) {
    int ascii = input.codeUnitAt(i);
    current += ascii;
    current *= 17;
    current %= 256;
  }
  return current;
}
