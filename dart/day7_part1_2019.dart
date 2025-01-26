
import 'dart:io';
import 'dart:math';

void main() {
  final input = File('input.txt').readAsStringSync().trim();
  final program = input.split(',').map(int.parse).toList();

  int maxOutput = 0;
  final phaseSettings = [0, 1, 2, 3, 4];

  for (final permutation in permute(phaseSettings)) {
    int output = 0;
    for (final phase in permutation) {
      output = runAmplifier(program, phase, output);
    }
    maxOutput = max(maxOutput, output);
  }

  print(maxOutput);
}

List<List<int>> permute(List<int> items) {
  if (items.isEmpty) return [[]];
  final result = <List<int>>[];
  for (int i = 0; i < items.length; i++) {
    final item = items[i];
    final remaining = items.sublist(0, i) + items.sublist(i + 1);
    for (final subPermutation in permute(remaining)) {
      result.add([item, ...subPermutation]);
    }
  }
  return result;
}


int runAmplifier(List<int> program, int phase, int input) {
  final memory = [...program];
  int instructionPointer = 0;
  bool phaseUsed = false;

  while (true) {
      int opcode = memory[instructionPointer] % 100;
      int mode1 = (memory[instructionPointer] ~/ 100) % 10;
      int mode2 = (memory[instructionPointer] ~/ 1000) % 10;
      
      int getValue(int offset, int mode) {
        final address = instructionPointer + offset;
          if (mode == 1) {
            return memory[address];
          }
          return memory[memory[address]];
      }

    switch (opcode) {
      case 1: // Add
        memory[memory[instructionPointer + 3]] =
            getValue(1, mode1) + getValue(2, mode2);
        instructionPointer += 4;
        break;
      case 2: // Multiply
        memory[memory[instructionPointer + 3]] =
            getValue(1, mode1) * getValue(2, mode2);
        instructionPointer += 4;
        break;
      case 3: // Input
        if (!phaseUsed) {
            memory[memory[instructionPointer + 1]] = phase;
            phaseUsed = true;
        } else {
            memory[memory[instructionPointer + 1]] = input;
        }
        instructionPointer += 2;
        break;
      case 4: // Output
        final outputValue = getValue(1, mode1);
        instructionPointer += 2;
        return outputValue;
      case 5: // Jump-if-true
        if (getValue(1, mode1) != 0) {
          instructionPointer = getValue(2, mode2);
        } else {
          instructionPointer += 3;
        }
        break;
      case 6: // Jump-if-false
        if (getValue(1, mode1) == 0) {
          instructionPointer = getValue(2, mode2);
        } else {
          instructionPointer += 3;
        }
        break;
      case 7: // Less than
        memory[memory[instructionPointer + 3]] =
            getValue(1, mode1) < getValue(2, mode2) ? 1 : 0;
        instructionPointer += 4;
        break;
      case 8: // Equals
        memory[memory[instructionPointer + 3]] =
            getValue(1, mode1) == getValue(2, mode2) ? 1 : 0;
        instructionPointer += 4;
        break;
      case 99: // Halt
        return -1; // Indicate that the program halted
      default:
          throw Exception("Invalid Opcode: $opcode");
    }
  }
}
