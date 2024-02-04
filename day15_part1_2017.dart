import 'dart:io';

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  int genA = int.parse(lines[0].split(' ').last);
  int genB = int.parse(lines[1].split(' ').last);
  int factorA = 16807;
  int factorB = 48271;
  int divisor = 2147483647;
  int count = 0;

  for (int i = 0; i < 40000000; i++) {
    genA = (genA * factorA) % divisor;
    genB = (genB * factorB) % divisor;

    if (genA & 0xFFFF == genB & 0xFFFF) {
      count++;
    }
  }

  print(count);
}