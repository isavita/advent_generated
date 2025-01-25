
import 'dart:io';

class IntcodeComputer {
  List<int> memory;
  int ip = 0;
  int relativeBase = 0;
  List<int> inputQueue = [];
  List<int> outputQueue = [];

  IntcodeComputer(List<int> program) : memory = List.from(program);

  int getParameterValue(int mode, int offset) {
    int address;
    switch (mode) {
      case 0: // Position mode
        address = memory[ip + offset];
        break;
      case 1: // Immediate mode
        address = ip + offset;
        break;
      case 2: // Relative mode
        address = memory[ip + offset] + relativeBase;
        break;
      default:
        throw Exception('Invalid parameter mode: $mode');
    }
    if (address >= memory.length) {
      memory.addAll(List.filled(address - memory.length + 1, 0));
    }
    return memory[address];
  }

  void setParameterValue(int mode, int offset, int value) {
    int address;
    switch (mode) {
      case 0: // Position mode
        address = memory[ip + offset];
        break;
      case 2: // Relative mode
        address = memory[ip + offset] + relativeBase;
        break;
      default:
        throw Exception('Invalid parameter mode for write: $mode');
    }
    if (address >= memory.length) {
      memory.addAll(List.filled(address - memory.length + 1, 0));
    }
    memory[address] = value;
  }

  void run() {
    while (true) {
      int instruction = memory[ip];
      int opcode = instruction % 100;
      int mode1 = (instruction ~/ 100) % 10;
      int mode2 = (instruction ~/ 1000) % 10;
      int mode3 = (instruction ~/ 10000) % 10;

      switch (opcode) {
        case 1: // Add
          setParameterValue(
              mode3, 3, getParameterValue(mode1, 1) + getParameterValue(mode2, 2));
          ip += 4;
          break;
        case 2: // Multiply
          setParameterValue(
              mode3, 3, getParameterValue(mode1, 1) * getParameterValue(mode2, 2));
          ip += 4;
          break;
        case 3: // Input
          if (inputQueue.isEmpty) {
            return;
          }
          setParameterValue(mode1, 1, inputQueue.removeAt(0));
          ip += 2;
          break;
        case 4: // Output
          outputQueue.add(getParameterValue(mode1, 1));
          ip += 2;
          break;
        case 5: // Jump-if-true
          if (getParameterValue(mode1, 1) != 0) {
            ip = getParameterValue(mode2, 2);
          } else {
            ip += 3;
          }
          break;
        case 6: // Jump-if-false
          if (getParameterValue(mode1, 1) == 0) {
            ip = getParameterValue(mode2, 2);
          } else {
            ip += 3;
          }
          break;
        case 7: // Less than
          setParameterValue(mode3, 3,
              getParameterValue(mode1, 1) < getParameterValue(mode2, 2) ? 1 : 0);
          ip += 4;
          break;
        case 8: // Equals
          setParameterValue(mode3, 3,
              getParameterValue(mode1, 1) == getParameterValue(mode2, 2) ? 1 : 0);
          ip += 4;
          break;
        case 9: // Adjust relative base
          relativeBase += getParameterValue(mode1, 1);
          ip += 2;
          break;
        case 99: // Halt
          return;
        default:
          throw Exception('Invalid opcode: $opcode');
      }
    }
  }
}

void main() {
  String input = File('input.txt').readAsStringSync().trim();
  List<int> program = input.split(',').map(int.parse).toList();

  // Part 1
  IntcodeComputer computer = IntcodeComputer(program);
  computer.run();
  int blockCount = 0;
  for (int i = 2; i < computer.outputQueue.length; i += 3) {
    if (computer.outputQueue[i] == 2) {
      blockCount++;
    }
  }
  print('Part 1: $blockCount');

  // Part 2
  program[0] = 2; 
  computer = IntcodeComputer(program);
  Map<String, int> grid = {};
  int score = 0;
  int ballX = 0;
  int paddleX = 0;

  while (true) {
    computer.run();

    if (computer.outputQueue.isEmpty) {
      break; 
    }

    while (computer.outputQueue.isNotEmpty) {
        int x = computer.outputQueue.removeAt(0);
        int y = computer.outputQueue.removeAt(0);
        int tileId = computer.outputQueue.removeAt(0);
      if (x == -1 && y == 0) {
        score = tileId;
      } else {
        grid['$x,$y'] = tileId;
        if (tileId == 4) {
            ballX = x;
        } else if (tileId == 3) {
            paddleX = x;
        }
      }
    }

    if (ballX < paddleX) {
        computer.inputQueue.add(-1);
    } else if (ballX > paddleX) {
        computer.inputQueue.add(1);
    } else {
        computer.inputQueue.add(0);
    }
    
    if (!grid.values.contains(2)) break;
  }

  print('Part 2: $score');
}
