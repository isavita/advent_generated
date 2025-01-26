
import 'dart:io';
import 'dart:convert';

void main() {
  final input = File('input.txt').readAsStringSync().trim();
  final program = input.split(',').map(int.parse).toList();

  final springScript = [
    'NOT A J',
    'NOT B T',
    'OR T J',
    'NOT C T',
    'OR T J',
    'AND D J',
    'WALK',
  ];
  final asciiInput = springScript.join('\n') + '\n';

  final output = runIntcode(program, asciiInput);
  print(output);
}

int runIntcode(List<int> program, String input) {
  final memory = List<int>.from(program);
  int relativeBase = 0;
  int instructionPointer = 0;
  int inputPointer = 0;
  int outputValue = 0;

  while (true) {
    final instruction = memory[instructionPointer];
    final opcode = instruction % 100;

    if (opcode == 99) {
      break;
    }

    int getParam(int offset, int mode) {
      final address = instructionPointer + offset;
      if (mode == 0) {
        final pos = memory[address];
        _ensureMemory(memory, pos);
        return memory[pos];
      } else if (mode == 1) {
        _ensureMemory(memory, address);
        return memory[address];
      } else {
        final pos = memory[address] + relativeBase;
        _ensureMemory(memory, pos);
        return memory[pos];
      }
    }

    void setParam(int offset, int mode, int value) {
      final address = instructionPointer + offset;
      if (mode == 0) {
        final pos = memory[address];
        _ensureMemory(memory, pos);
        memory[pos] = value;
      } else if (mode == 2) {
        final pos = memory[address] + relativeBase;
        _ensureMemory(memory, pos);
        memory[pos] = value;
      } else {
        throw ArgumentError('Invalid mode for write: $mode');
      }
    }

    final mode1 = (instruction ~/ 100) % 10;
    final mode2 = (instruction ~/ 1000) % 10;
    final mode3 = (instruction ~/ 10000) % 10;

    switch (opcode) {
      case 1:
        final param1 = getParam(1, mode1);
        final param2 = getParam(2, mode2);
        setParam(3, mode3, param1 + param2);
        instructionPointer += 4;
        break;
      case 2:
        final param1 = getParam(1, mode1);
        final param2 = getParam(2, mode2);
        setParam(3, mode3, param1 * param2);
        instructionPointer += 4;
        break;
      case 3:
        if (inputPointer >= input.length) {
          throw StateError('Input is depleted');
        }
        setParam(1, mode1, input.codeUnitAt(inputPointer));
        inputPointer++;
        instructionPointer += 2;
        break;
      case 4:
        final output = getParam(1, mode1);
        if (output > 127) {
          return output;
        }
        outputValue = output;
        instructionPointer += 2;
        break;
      case 5:
        final param1 = getParam(1, mode1);
        final param2 = getParam(2, mode2);
        if (param1 != 0) {
          instructionPointer = param2;
        } else {
          instructionPointer += 3;
        }
        break;
      case 6:
        final param1 = getParam(1, mode1);
        final param2 = getParam(2, mode2);
        if (param1 == 0) {
          instructionPointer = param2;
        } else {
          instructionPointer += 3;
        }
        break;
      case 7:
        final param1 = getParam(1, mode1);
        final param2 = getParam(2, mode2);
        setParam(3, mode3, param1 < param2 ? 1 : 0);
        instructionPointer += 4;
        break;
      case 8:
        final param1 = getParam(1, mode1);
        final param2 = getParam(2, mode2);
        setParam(3, mode3, param1 == param2 ? 1 : 0);
        instructionPointer += 4;
        break;
      case 9:
        final param1 = getParam(1, mode1);
        relativeBase += param1;
        instructionPointer += 2;
        break;
      default:
        throw ArgumentError('Invalid opcode: $opcode');
    }
  }
  return outputValue;
}


void _ensureMemory(List<int> memory, int address) {
  if (address >= memory.length) {
    memory.addAll(List.filled(address - memory.length + 1, 0));
  }
}
