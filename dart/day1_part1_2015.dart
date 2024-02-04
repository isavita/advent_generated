
import 'dart:io';

void main() {
  File file = File('input.txt');
  String instructions = file.readAsStringSync();
  
  int floor = 0;
  for (int i = 0; i < instructions.length; i++) {
    if (instructions[i] == '(') {
      floor++;
    } else {
      floor--;
    }
  }
  
  print(floor);
}
