import 'dart:io';
import 'dart:convert';
import 'dart:math' as math;

void main() {
  final memory = <int, int>{};
  final file = File('input.txt');
  final lines = file.readAsLinesSync();
  final program = lines.first.split(',').map(int.parse).toList();

  for (int i = 0; i < program.length; i++) {
    memory[i] = program[i];
  }

  print(runIntcode(memory));
}

int runIntcode(Map<int, int> memory) {
  int output = 0, ip = 0, relativeBase = 0;

  while (true) {
    final opcode = memory[ip]! % 100;
    final modes = memory[ip]! ~/ 100;

    int getParam(int offset) {
      final mode = (modes ~/ math.pow(10, offset - 1).toInt()) % 10;
      final param = memory[ip + offset]!;
      switch (mode) {
        case 0:
          return memory[param] ?? 0;
        case 1:
          return param;
        case 2:
          return memory[relativeBase + param] ?? 0;
        default:
          throw Exception('Unknown parameter mode');
      }
    }

    void setParam(int offset, int value) {
      final mode = (modes ~/ math.pow(10, offset - 1).toInt()) % 10;
      final param = memory[ip + offset]!;
      switch (mode) {
        case 0:
          memory[param] = value;
          break;
        case 2:
          memory[relativeBase + param] = value;
          break;
        default:
          throw Exception('Unknown parameter mode');
      }
    }

    switch (opcode) {
      case 1:
        setParam(3, getParam(1) + getParam(2));
        ip += 4;
        break;
      case 2:
        setParam(3, getParam(1) * getParam(2));
        ip += 4;
        break;
      case 3:
        setParam(1, 1); // Test mode input
        ip += 2;
        break;
      case 4:
        output = getParam(1);
        ip += 2;
        break;
      case 5:
        if (getParam(1) != 0) {
          ip = getParam(2);
        } else {
          ip += 3;
        }
        break;
      case 6:
        if (getParam(1) == 0) {
          ip = getParam(2);
        } else {
          ip += 3;
        }
        break;
      case 7:
        setParam(3, getParam(1) < getParam(2) ? 1 : 0);
        ip += 4;
        break;
      case 8:
        setParam(3, getParam(1) == getParam(2) ? 1 : 0);
        ip += 4;
        break;
      case 9:
        relativeBase += getParam(1);
        ip += 2;
        break;
      case 99:
        return output;
      default:
        throw Exception('Unknown opcode: $opcode');
    }
  }
}