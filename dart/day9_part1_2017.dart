
import 'dart:io';

void main() {
  String input = File('input.txt').readAsStringSync();
  int score = 0;
  int depth = 0;
  
  for (int i = 0; i < input.length; i++) {
    if (input[i] == '{') {
      depth++;
      score += depth;
    } else if (input[i] == '}') {
      depth--;
    } else if (input[i] == '<') {
      while (input[i] != '>') {
        if (input[i] == '!') {
          i++;
        }
        i++;
      }
    }
  }
  
  print(score);
}
