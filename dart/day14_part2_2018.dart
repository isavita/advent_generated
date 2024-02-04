import 'dart:io';

void main() {
  var file = File('input.txt');
  var input = file.readAsStringSync().trim();

  var scoreboard = [3, 7];
  var elf1 = 0;
  var elf2 = 1;
  var inputLen = input.length;
  var inputSequence = List<int>.filled(inputLen, 0);

  for (var i = 0; i < inputLen; i++) {
    inputSequence[i] = int.parse(input[i]);
  }

  while (true) {
    var newScore = scoreboard[elf1] + scoreboard[elf2];
    if (newScore >= 10) {
      scoreboard.add(newScore ~/ 10);
      if (checkSequence(scoreboard, inputSequence)) {
        break;
      }
    }
    scoreboard.add(newScore % 10);
    if (checkSequence(scoreboard, inputSequence)) {
      break;
    }

    elf1 = (elf1 + scoreboard[elf1] + 1) % scoreboard.length;
    elf2 = (elf2 + scoreboard[elf2] + 1) % scoreboard.length;
  }

  print(scoreboard.length - inputLen);
}

bool checkSequence(List<int> scoreboard, List<int> sequence) {
  if (scoreboard.length < sequence.length) {
    return false;
  }
  var start = scoreboard.length - sequence.length;
  for (var i = 0; i < sequence.length; i++) {
    if (scoreboard[start + i] != sequence[i]) {
      return false;
    }
  }
  return true;
}