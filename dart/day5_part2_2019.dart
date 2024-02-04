
import 'dart:io';

void main() {
  List<int> program = File('input.txt').readAsStringSync().trim().split(',').map(int.parse).toList();
  int i = 0;
  while (i < program.length) {
    int opcode = program[i] % 100;
    int mode1 = (program[i] ~/ 100) % 10;
    int mode2 = (program[i] ~/ 1000) % 10;
    int mode3 = (program[i] ~/ 10000) % 10;
    if (opcode == 99) {
      break;
    } else if (opcode == 1) {
      program[program[i + 3]] = getValue(program, i + 1, mode1) + getValue(program, i + 2, mode2);
      i += 4;
    } else if (opcode == 2) {
      program[program[i + 3]] = getValue(program, i + 1, mode1) * getValue(program, i + 2, mode2);
      i += 4;
    } else if (opcode == 3) {
      program[program[i + 1]] = 5; // System ID for thermal radiator controller
      i += 2;
    } else if (opcode == 4) {
      print(getValue(program, i + 1, mode1));
      i += 2;
    } else if (opcode == 5) {
      if (getValue(program, i + 1, mode1) != 0) {
        i = getValue(program, i + 2, mode2);
      } else {
        i += 3;
      }
    } else if (opcode == 6) {
      if (getValue(program, i + 1, mode1) == 0) {
        i = getValue(program, i + 2, mode2);
      } else {
        i += 3;
      }
    } else if (opcode == 7) {
      program[program[i + 3]] = getValue(program, i + 1, mode1) < getValue(program, i + 2, mode2) ? 1 : 0;
      i += 4;
    } else if (opcode == 8) {
      program[program[i + 3]] = getValue(program, i + 1, mode1) == getValue(program, i + 2, mode2) ? 1 : 0;
      i += 4;
    }
  }
}

int getValue(List<int> program, int index, int mode) {
  return mode == 0 ? program[program[index]] : program[index];
}
