
import 'dart:io';

const totalCups = 1000000;
const totalMoves = 10000000;

void main() {
  var file = new File('input.txt');
  var input = file.readAsStringSync().trim();

  var cups = List<int>.filled(totalCups + 1, 0);
  var lastCup = 0;

  for (var i = 0; i < input.length; i++) {
    var cup = int.parse(input[i]);
    if (i > 0) {
      cups[lastCup] = cup;
    }
    lastCup = cup;
  }

  for (var i = input.length + 1; i <= totalCups; i++) {
    cups[lastCup] = i;
    lastCup = i;
  }
  cups[lastCup] = int.parse(input[0]);

  var currentCup = int.parse(input[0]);
  for (var i = 0; i < totalMoves; i++) {
    var pickup1 = cups[currentCup];
    var pickup2 = cups[pickup1];
    var pickup3 = cups[pickup2];

    cups[currentCup] = cups[pickup3];

    var destinationCup = currentCup - 1;
    if (destinationCup == 0) {
      destinationCup = totalCups;
    }
    while (destinationCup == pickup1 || destinationCup == pickup2 || destinationCup == pickup3) {
      destinationCup--;
      if (destinationCup == 0) {
        destinationCup = totalCups;
      }
    }

    cups[pickup3] = cups[destinationCup];
    cups[destinationCup] = pickup1;

    currentCup = cups[currentCup];
  }

  var cup1 = cups[1];
  var cup2 = cups[cup1];
  print(cup1 * cup2);
}
