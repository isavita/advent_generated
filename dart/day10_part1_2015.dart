
import 'dart:io';

void main() {
  String input = File('input.txt').readAsStringSync().trim();
  
  for (int i = 0; i < 40; i++) {
    input = lookAndSay(input);
  }
  
  print(input.length);
}

String lookAndSay(String input) {
  String result = '';
  int count = 1;
  
  for (int i = 0; i < input.length; i++) {
    if (i + 1 < input.length && input[i] == input[i + 1]) {
      count++;
    } else {
      result += count.toString() + input[i];
      count = 1;
    }
  }
  
  return result;
}
