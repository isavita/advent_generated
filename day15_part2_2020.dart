import 'dart:io';

void main() {
  var file = File('input.txt');
  var startingNumbers = file.readAsStringSync().trim().split(',');

  var spoken = <int, int>{};
  var lastSpoken = 0;

  for (var i = 0; i < startingNumbers.length; i++) {
    if (i == startingNumbers.length - 1) {
      lastSpoken = int.parse(startingNumbers[i]);
    } else {
      var num = int.parse(startingNumbers[i]);
      spoken[num] = i + 1;
    }
  }

  for (var turn = startingNumbers.length + 1; turn <= 30000000; turn++) {
    var nextNumber = 0;
    if (spoken.containsKey(lastSpoken)) {
      nextNumber = turn - 1 - spoken[lastSpoken]!;
    }
    spoken[lastSpoken] = turn - 1;
    lastSpoken = nextNumber;
  }

  print(lastSpoken);
}