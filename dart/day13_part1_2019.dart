
import 'dart:io';
import 'dart:math';

void main() {
  final program = readInput('input.txt');
  final output = runIntcode(program);
  final blockTileCount = countBlockTiles(output);
  print(blockTileCount);
}

List<int> readInput(String filename) {
  return File(filename)
      .readAsStringSync()
      .trim()
      .split(',')
      .map(int.parse)
      .toList();
}

List<int> runIntcode(List<int> initialProgram) {
  final program = [...initialProgram]; // Create a copy to avoid modifying the original
  int instructionPointer = 0;
  int relativeBase = 0;
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
        throw Exception('Invalid parameter mode for write: $mode');
    }
  }

  while (true) {
    final instruction = program[instructionPointer];
    final opcode = instruction % 100;
    final paramMode1 = (instruction ~/ 100) % 10;
    final paramMode2 = (instruction ~/ 1000) % 10;
    final paramMode3 = (instruction ~/ 10000) % 10;

    switch (opcode) {
      case 1: // Add
        final param1 = getParameterValue(paramMode1, 1);
        final param2 = getParameterValue(paramMode2, 2);
        final writeAddress = getWriteAddress(paramMode3, 3);
        _ensureMemorySize(program, writeAddress);
        program[writeAddress] = param1 + param2;
        instructionPointer += 4;
        break;
      case 2: // Multiply
        final param1 = getParameterValue(paramMode1, 1);
        final param2 = getParameterValue(paramMode2, 2);
        final writeAddress = getWriteAddress(paramMode3, 3);
        _ensureMemorySize(program, writeAddress);
        program[writeAddress] = param1 * param2;
        instructionPointer += 4;
        break;
      case 3: // Input (not used in part 1, but included for completeness)
        throw Exception('Input opcode 3 encountered in part 1');
      case 4: // Output
        final outputValue = getParameterValue(paramMode1, 1);
        output.add(outputValue);
        instructionPointer += 2;
        break;
      case 5: // Jump-if-true
        final param1 = getParameterValue(paramMode1, 1);
        final param2 = getParameterValue(paramMode2, 2);
        if (param1 != 0) {
          instructionPointer = param2;
        } else {
          instructionPointer += 3;
        }
        break;
      case 6: // Jump-if-false
        final param1 = getParameterValue(paramMode1, 1);
        final param2 = getParameterValue(paramMode2, 2);
        if (param1 == 0) {
          instructionPointer = param2;
        } else {
          instructionPointer += 3;
        }
        break;
      case 7: // Less than
        final param1 = getParameterValue(paramMode1, 1);
        final param2 = getParameterValue(paramMode2, 2);
        final writeAddress = getWriteAddress(paramMode3, 3);
        _ensureMemorySize(program, writeAddress);
        program[writeAddress] = param1 < param2 ? 1 : 0;
        instructionPointer += 4;
        break;
      case 8: // Equals
        final param1 = getParameterValue(paramMode1, 1);
        final param2 = getParameterValue(paramMode2, 2);
        final writeAddress = getWriteAddress(paramMode3, 3);
        _ensureMemorySize(program, writeAddress);
        program[writeAddress] = param1 == param2 ? 1 : 0;
        instructionPointer += 4;
        break;
      case 9: // Adjust relative base
        final param1 = getParameterValue(paramMode1, 1);
        relativeBase += param1;
        instructionPointer += 2;
        break;
      case 99: // Halt
        return output;
      default:
        throw Exception('Invalid opcode: $opcode');
    }
  }
}

void _ensureMemorySize(List<int> memory, int address) {
  if (address >= memory.length) {
    memory.addAll(List.filled(address - memory.length + 1, 0));
  }
}

int countBlockTiles(List<int> output) {
  int blockTileCount = 0;
  for (int i = 2; i < output.length; i += 3) {
    if (output[i] == 2) {
      blockTileCount++;
    }
  }
  return blockTileCount;
}
