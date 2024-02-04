
import 'dart:io';

void main() {
  String input = File('input.txt').readAsStringSync().trim();

  int score = 0;
  int garbageCount = 0;
  bool inGarbage = false;
  bool ignoreNext = false;

  for (int i = 0; i < input.length; i++) {
    if (ignoreNext) {
      ignoreNext = false;
    } else {
      if (input[i] == '!') {
        ignoreNext = true;
      } else if (inGarbage) {
        if (input[i] == '>') {
          inGarbage = false;
        } else {
          garbageCount++;
        }
      } else {
        if (input[i] == '<') {
          inGarbage = true;
        } else if (input[i] == '{') {
          score++;
        }
      }
    }
  }

  print(score);
  print(garbageCount);
}
