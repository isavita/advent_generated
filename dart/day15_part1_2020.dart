
import 'dart:io';

void main() {
  var startingNumbers = File('input.txt').readAsStringSync().trim().split(',');

  var lastSpoken = <int, int>{};
  var lastNumber, nextNumber;

  for (var turn = 1; turn <= 2020; turn++) {
    if (turn - 1 < startingNumbers.length) {
      lastNumber = int.parse(startingNumbers[turn - 1]);
      lastSpoken[lastNumber] = turn;
      continue;
    }
    if (lastSpoken.containsKey(lastNumber) && lastSpoken[lastNumber] != turn - 1) {
      nextNumber = turn - 1 - lastSpoken[lastNumber]!;
    } else {
      nextNumber = 0;
    }
    lastSpoken[lastNumber] = turn - 1;
    lastNumber = nextNumber;
  }

  print(lastNumber);
}
