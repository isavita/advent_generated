
import 'dart:io';

void main() {
  List<String> instructions = File('input.txt').readAsLinesSync();
  
  int horizontalPosition = 0;
  int depth = 0;
  int aim = 0;
  
  for (String instruction in instructions) {
    List<String> parts = instruction.split(' ');
    int value = int.parse(parts[1]);
    
    if (parts[0] == 'forward') {
      horizontalPosition += value;
      depth += aim * value;
    } else if (parts[0] == 'down') {
      aim += value;
    } else if (parts[0] == 'up') {
      aim -= value;
    }
  }
  
  print(horizontalPosition * depth);
}
