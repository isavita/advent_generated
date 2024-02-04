
import 'dart:io';

void main() {
  List<int> generatorA = [];
  List<int> generatorB = [];
  int factorA = 16807;
  int factorB = 48271;
  int divisor = 2147483647;
  int count = 0;

  List<String> lines = File('input.txt').readAsLinesSync();
  int startA = int.parse(lines[0].split(' ').last);
  int startB = int.parse(lines[1].split(' ').last);

  int currentA = startA;
  int currentB = startB;

  for (int i = 0; i < 5000000; i++) {
    do {
      currentA = (currentA * factorA) % divisor;
    } while (currentA % 4 != 0);

    do {
      currentB = (currentB * factorB) % divisor;
    } while (currentB % 8 != 0);

    generatorA.add(currentA);
    generatorB.add(currentB);
  }

  for (int i = 0; i < 5000000; i++) {
    if ((generatorA[i] & 0xFFFF) == (generatorB[i] & 0xFFFF)) {
      count++;
    }
  }

  print(count);
}
