
import 'dart:io';

void main() {
  final program = readInput('input.txt');
  if (program == null) {
    print('Error reading input file.');
    return;
  }

  int affectedPoints = 0;
  for (int y = 0; y < 50; ++y) {
    for (int x = 0; x < 50; ++x) {
      final output = runIntcode(List.from(program), [x, y]);
      if (output.isNotEmpty && output[0] == 1) {
        affectedPoints++;
      }
    }
  }

  print(affectedPoints);
}

List<int>? readInput(String filename) {
  try {
    final file = File(filename);
    final contents = file.readAsStringSync().trim();
    return contents.split(',').map(int.parse).toList();
  } catch (e) {
    return null;
  }
}

List<int> runIntcode(List<int> program, List<int> inputs) {
  int instructionPointer = 0;
  int relativeBase = 0;
  int inputIndex = 0;
  List<int> output = [];

  int getParameterValue(int mode, int offset) {
    final param = program[instructionPointer + offset];
    switch (mode) {
      case 0: // Position mode
        _ensureMemorySize(program, param);
        return program[param];
      case 1: // Immediate mode
        return param;
      case 2: // Relative mode
        final address = relativeBase + param;
        _ensureMemorySize(program, address);
        return program[address];
      default:
        throw Exception('Invalid parameter mode: $mode');
    }
  }

  int getWriteAddress(int mode, int offset) {
    final param = program[instructionPointer + offset];
    switch (mode) {
      case 0: // Position mode
        return param;
      case 2: // Relative mode
        return relativeBase + param;
      default:
        throw Exception('Invalid write parameter mode: $mode');
    }
  }

  while (program[instructionPointer] != 99) {
    final instruction = program[instructionPointer];
    final opcode = instruction % 100;
    final mode1 = (instruction ~/ 100) % 10;
    final mode2 = (instruction ~/ 1000) % 10;
    final mode3 = (instruction ~/ 10000) % 10;

    switch (opcode) {
      case 1: // Add
        final param1 = getParameterValue(mode1, 1);
        final param2 = getParameterValue(mode2, 2);
        final address = getWriteAddress(mode3, 3);
        _ensureMemorySize(program, address);
        program[address] = param1 + param2;
        instructionPointer += 4;
        break;
      case 2: // Multiply
        final param1 = getParameterValue(mode1, 1);
        final param2 = getParameterValue(mode2, 2);
        final address = getWriteAddress(mode3, 3);
        _ensureMemorySize(program, address);
        program[address] = param1 * param2;
        instructionPointer += 4;
        break;
      case 3: // Input
        final address = getWriteAddress(mode1, 1);
        _ensureMemorySize(program, address);
        if (inputIndex < inputs.length) {
          program[address] = inputs[inputIndex++];
        } else {
          throw Exception('Input requested but no input provided.');
        }
        instructionPointer += 2;
        break;
      case 4: // Output
        final param1 = getParameterValue(mode1, 1);
        output.add(param1);
        instructionPointer += 2;
        break;
      case 5: // Jump-if-true
        final param1 = getParameterValue(mode1, 1);
        final param2 = getParameterValue(mode2, 2);
        if (param1 != 0) {
          instructionPointer = param2;
        } else {
          instructionPointer += 3;
        }
        break;
      case 6: // Jump-if-false
        final param1 = getParameterValue(mode1, 1);
        final param2 = getParameterValue(mode2, 2);
        if (param1 == 0) {
          instructionPointer = param2;
        } else {
          instructionPointer += 3;
        }
        break;
      case 7: // Less than
        final param1 = getParameterValue(mode1, 1);
        final param2 = getParameterValue(mode2, 2);
        final address = getWriteAddress(mode3, 3);
        _ensureMemorySize(program, address);
        program[address] = (param1 < param2) ? 1 : 0;
        instructionPointer += 4;
        break;
      case 8: // Equals
        final param1 = getParameterValue(mode1, 1);
        final param2 = getParameterValue(mode2, 2);
        final address = getWriteAddress(mode3, 3);
        _ensureMemorySize(program, address);
        program[address] = (param1 == param2) ? 1 : 0;
        instructionPointer += 4;
        break;
      case 9: // Adjust relative base
        final param1 = getParameterValue(mode1, 1);
        relativeBase += param1;
        instructionPointer += 2;
        break;
      case 99: // Halt
        break;
      default:
        throw Exception('Invalid opcode: $opcode at position $instructionPointer');
    }
  }
  return output;
}

void _ensureMemorySize(List<int> memory, int address) {
  if (address >= memory.length) {
    memory.addAll(List.filled(address - memory.length + 1, 0));
  }
}
