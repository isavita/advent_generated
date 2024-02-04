
import 'dart:io';

void main() {
  List<int> program = File('input.txt').readAsStringSync().split(',').map(int.parse).toList();
  
  program[1] = 12;
  program[2] = 2;
  
  int i = 0;
  while (program[i] != 99) {
    if (program[i] == 1) {
      program[program[i + 3]] = program[program[i + 1]] + program[program[i + 2]];
    } else if (program[i] == 2) {
      program[program[i + 3]] = program[program[i + 1]] * program[program[i + 2]];
    }
    i += 4;
  }
  
  print(program[0]);
}
