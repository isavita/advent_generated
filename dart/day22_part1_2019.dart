
import 'dart:io';

void main() {
  List<String> instructions = File('input.txt').readAsLinesSync();

  int cardPosition = 2019;

  for (String instruction in instructions) {
    if (instruction == 'deal into new stack') {
      cardPosition = 10007 - cardPosition - 1;
    } else if (instruction.startsWith('cut')) {
      int n = int.parse(instruction.split(' ')[1]);
      cardPosition = (cardPosition - n + 10007) % 10007;
    } else if (instruction.startsWith('deal with increment')) {
      int n = int.parse(instruction.split(' ')[3]);
      cardPosition = (cardPosition * n) % 10007;
    }
  }

  print(cardPosition);
}
