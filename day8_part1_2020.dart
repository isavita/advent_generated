
import 'dart:io';

void main() {
  List<String> instructions = File('input.txt').readAsLinesSync();
  
  int accumulator = 0;
  int index = 0;
  List<int> visited = List.filled(instructions.length, 0);
  
  while (visited[index] == 0) {
    visited[index]++;
    var parts = instructions[index].split(' ');
    var operation = parts[0];
    var argument = int.parse(parts[1]);
    
    if (operation == 'acc') {
      accumulator += argument;
      index++;
    } else if (operation == 'jmp') {
      index += argument;
    } else {
      index++;
    }
  }
  
  print(accumulator);
}
