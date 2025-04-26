
import 'dart:io';
import 'dart:math';

class IntcodeComputer {
  Map<int, int> memory;
  int ip = 0;
  int relativeBase = 0;
  bool halted = false;
  List<int> _inputQueue = [];
  List<int> _outputQueue = [];

  IntcodeComputer(List<int> program) : memory = {} {
    for (int i = 0; i < program.length; i++) {
      memory[i] = program[i];
    }
  }

  int _readValue(int address) {
    return memory[address] ?? 0;
  }

  void _writeValue(int address, int value) {
    memory[address] = value;
  }

  int _getParameter(int mode, int offset) {
    int param = _readValue(ip + offset);
    switch (mode) {
      case 0: // Position mode
        return _readValue(param);
      case 1: // Immediate mode
        return param;
      case 2: // Relative mode
        return _readValue(relativeBase + param);
      default:
        throw ArgumentError('Unknown parameter mode: $mode');
    }
  }

  void _setParameter(int mode, int offset, int value) {
    int param = _readValue(ip + offset);
    switch (mode) {
      case 0: // Position mode
        _writeValue(param, value);
        break;
      case 2: // Relative mode
        _writeValue(relativeBase + param, value);
        break;
      default:
        throw ArgumentError('Unknown parameter mode for writing: $mode');
    }
  }

  List<int> run(List<int> inputs) {
    _inputQueue.addAll(inputs);
    _outputQueue = [];

    while (!halted) {
      int instruction = _readValue(ip);
      int opcode = instruction % 100;
      int mode1 = (instruction ~/ 100) % 10;
      int mode2 = (instruction ~/ 1000) % 10;
      int mode3 = (instruction ~/ 10000) % 10;

      switch (opcode) {
        case 1: // Addition
          int param1 = _getParameter(mode1, 1);
          int param2 = _getParameter(mode2, 2);
          _setParameter(mode3, 3, param1 + param2);
          ip += 4;
          break;
        case 2: // Multiplication
          int param1 = _getParameter(mode1, 1);
          int param2 = _getParameter(mode2, 2);
          _setParameter(mode3, 3, param1 * param2);
          ip += 4;
          break;
        case 3: // Input
          if (_inputQueue.isEmpty) {
             return List.unmodifiable(_outputQueue); // Need more input
          }
          _setParameter(mode1, 1, _inputQueue.removeAt(0));
          ip += 2;
          break;
        case 4: // Output
          int outputVal = _getParameter(mode1, 1);
          _outputQueue.add(outputVal);
          ip += 2;
          break;
        case 5: // Jump-if-true
          int param1 = _getParameter(mode1, 1);
          int param2 = _getParameter(mode2, 2);
          if (param1 != 0) {
            ip = param2;
          } else {
            ip += 3;
          }
          break;
        case 6: // Jump-if-false
          int param1 = _getParameter(mode1, 1);
          int param2 = _getParameter(mode2, 2);
          if (param1 == 0) {
            ip = param2;
          } else {
            ip += 3;
          }
          break;
        case 7: // Less than
          int param1 = _getParameter(mode1, 1);
          int param2 = _getParameter(mode2, 2);
          _setParameter(mode3, 3, (param1 < param2) ? 1 : 0);
          ip += 4;
          break;
        case 8: // Equals
          int param1 = _getParameter(mode1, 1);
          int param2 = _getParameter(mode2, 2);
          _setParameter(mode3, 3, (param1 == param2) ? 1 : 0);
          ip += 4;
          break;
        case 9: // Adjust relative base
          int param1 = _getParameter(mode1, 1);
          relativeBase += param1;
          ip += 2;
          break;
        case 99: // Halt
          halted = true;
          break;
        default:
          throw StateError('Unknown opcode: $opcode at ip $ip');
      }
    }
     return List.unmodifiable(_outputQueue);
  }
}

typedef Position = (int, int);

class Robot {
  IntcodeComputer computer;
  int direction = 0; // 0: Up, 1: Right, 2: Down, 3: Left
  Position position = (0, 0);
  Map<Position, int> panels = {};
  Set<Position> paintedPanels = {};

  Robot(List<int> program, int startPanelColor)
      : computer = IntcodeComputer(List.from(program)) {
    panels[position] = startPanelColor;
  }

  void _turnAndMove(int turnDirection) {
    if (turnDirection == 0) { // Turn left
      direction = (direction - 1 + 4) % 4;
    } else if (turnDirection == 1) { // Turn right
      direction = (direction + 1) % 4;
    } else {
      throw ArgumentError('Unknown turn direction: $turnDirection');
    }

    int x = position.$1;
    int y = position.$2;
    switch (direction) {
      case 0: // Up
        position = (x, y - 1);
        break;
      case 1: // Right
        position = (x + 1, y);
        break;
      case 2: // Down
        position = (x, y + 1);
        break;
      case 3: // Left
        position = (x - 1, y);
        break;
    }
  }

  void run() {
    while (!computer.halted) {
      int currentColor = panels[position] ?? 0;
      List<int> outputs = computer.run([currentColor]);

      if (outputs.length >= 2) {
        int paintColor = outputs[0];
        int turnDirection = outputs[1];

        panels[position] = paintColor;
        paintedPanels.add(position);
        _turnAndMove(turnDirection);
      } else if (!computer.halted && outputs.isNotEmpty) {
         throw StateError('Expected 2 outputs, got ${outputs.length}');
      }
    }
  }

  int getPaintedPanelsCount() {
    return paintedPanels.length;
  }

  void renderPanels() {
     if (panels.isEmpty) {
        print("No panels painted.");
        return;
     }

     int minX = panels.keys.map((p) => p.$1).reduce(min);
     int maxX = panels.keys.map((p) => p.$1).reduce(max);
     int minY = panels.keys.map((p) => p.$2).reduce(min);
     int maxY = panels.keys.map((p) => p.$2).reduce(max);

     var buffer = StringBuffer();
     buffer.writeln("\nRegistration Identifier:");
     for (int y = minY; y <= maxY; y++) {
         for (int x = minX; x <= maxX; x++) {
             buffer.write((panels[(x, y)] ?? 0) == 1 ? '#' : ' ');
         }
         buffer.writeln();
     }
     print(buffer.toString().trimRight());
  }
}

List<int> parseInput(String filePath) {
  var content = File(filePath).readAsStringSync().trim();
  return content.split(',').map(int.parse).toList();
}

void main() {
  var program = parseInput('input.txt');

  // Part One
  var robotPart1 = Robot(program, 0);
  robotPart1.run();
  print(robotPart1.getPaintedPanelsCount());

  // Part Two (Printed as the second part of the output)
  var robotPart2 = Robot(program, 1);
  robotPart2.run();
  robotPart2.renderPanels();
}

