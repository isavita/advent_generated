
import 'dart:io';
import 'dart:collection';

void main() {
  final program = readInput('input.txt');
  final shortestPathLength = findOxygenSystem(program);
  print(shortestPathLength);
}

List<int> readInput(String filename) {
  return File(filename)
      .readAsStringSync()
      .trim()
      .split(',')
      .map(int.parse)
      .toList();
}

int findOxygenSystem(List<int> initialProgram) {
  final grid = <Point, String>{};
  final startPoint = Point(0, 0);
  grid[startPoint] = '.'; // Mark start as explored

  final queue = Queue<DroidState>();
  queue.add(DroidState(program: List.from(initialProgram), position: startPoint, pathLength: 0));

  final visited = <Point>{startPoint};

  while (queue.isNotEmpty) {
    final currentState = queue.removeFirst();
    final currentPosition = currentState.position;
    final currentPathLength = currentState.pathLength;

    for (final direction in Direction.values) {
      final nextPosition = currentPosition.move(direction);
      if (visited.contains(nextPosition)) {
        continue; // Already visited, no need to explore again in this direction from here in BFS
      }

      final result = runIntcode(List.from(currentState.program), direction.command);
      final status = result.output;
      final nextProgram = result.program;

      if (status == 0) {
        grid[nextPosition] = '#'; // Mark wall
      } else if (status == 1 || status == 2) {
        grid[nextPosition] = '.'; // Mark path
        visited.add(nextPosition);
        if (status == 2) {
          return currentPathLength + 1; // Oxygen system found!
        }
        queue.add(DroidState(program: nextProgram, position: nextPosition, pathLength: currentPathLength + 1));
      }
    }
  }
  return -1; // Should not reach here in this problem, oxygen system is guaranteed to be found
}


class IntcodeResult {
  final List<int> program;
  final int output;

  IntcodeResult({required this.program, required this.output});
}


IntcodeResult runIntcode(List<int> program, int input) {
  int instructionPointer = 0;
  int relativeBase = 0;
  int outputValue = -1;

  while (program[instructionPointer] != 99) {
    final instruction = program[instructionPointer];
    final opcode = instruction % 100;
    final paramMode1 = (instruction ~/ 100) % 10;
    final paramMode2 = (instruction ~/ 1000) % 10;
    final paramMode3 = (instruction ~/ 10000) % 10;

    int getParam(int paramIndex, int mode) {
      final value = program[instructionPointer + paramIndex];
      switch (mode) {
        case 0: // Position mode
          _ensureMemorySize(program, value);
          return program[value];
        case 1: // Immediate mode
          return value;
        case 2: // Relative mode
          _ensureMemorySize(program, relativeBase + value);
          return program[relativeBase + value];
        default:
          throw Exception('Invalid parameter mode: $mode');
      }
    }

    void setParam(int paramIndex, int mode, int setValue) {
      final address = program[instructionPointer + paramIndex];
      int writeAddress;
      switch (mode) {
        case 0: // Position mode
          writeAddress = address;
          break;
        case 2: // Relative mode
          writeAddress = relativeBase + address;
          break;
        default:
          throw Exception('Invalid parameter mode for write: $mode');
      }
      _ensureMemorySize(program, writeAddress);
      program[writeAddress] = setValue;
    }


    switch (opcode) {
      case 1: // Add
        final param1 = getParam(1, paramMode1);
        final param2 = getParam(2, paramMode2);
        setParam(3, paramMode3, param1 + param2);
        instructionPointer += 4;
        break;
      case 2: // Multiply
        final param1 = getParam(1, paramMode1);
        final param2 = getParam(2, paramMode2);
        setParam(3, paramMode3, param1 * param2);
        instructionPointer += 4;
        break;
      case 3: // Input
        setParam(1, paramMode1, input);
        instructionPointer += 2;
        break;
      case 4: // Output
        outputValue = getParam(1, paramMode1);
        instructionPointer += 2;
        return IntcodeResult(program: program, output: outputValue);
      case 5: // Jump-if-true
        if (getParam(1, paramMode1) != 0) {
          instructionPointer = getParam(2, paramMode2);
        } else {
          instructionPointer += 3;
        }
        break;
      case 6: // Jump-if-false
        if (getParam(1, paramMode1) == 0) {
          instructionPointer = getParam(2, paramMode2);
        } else {
          instructionPointer += 3;
        }
        break;
      case 7: // Less than
        final param1 = getParam(1, paramMode1);
        final param2 = getParam(2, paramMode2);
        setParam(3, paramMode3, param1 < param2 ? 1 : 0);
        instructionPointer += 4;
        break;
      case 8: // Equals
        final param1 = getParam(1, paramMode1);
        final param2 = getParam(2, paramMode2);
        setParam(3, paramMode3, param1 == param2 ? 1 : 0);
        instructionPointer += 4;
        break;
      case 9: // Adjust relative base
        relativeBase += getParam(1, paramMode1);
        instructionPointer += 2;
        break;
      case 99: // Halt
        break;
      default:
        throw Exception('Invalid opcode: $opcode');
    }
  }
  return IntcodeResult(program: program, output: outputValue); // Should not reach here in this part 1 as it is guaranteed to find oxygen
}


void _ensureMemorySize(List<int> memory, int address) {
  if (address >= memory.length) {
    memory.addAll(List.filled(address - memory.length + 1, 0));
  }
}


class Point {
  final int x;
  final int y;

  Point(this.x, this.y);

  Point move(Direction direction) {
    switch (direction) {
      case Direction.north:
        return Point(x, y - 1);
      case Direction.south:
        return Point(x, y + 1);
      case Direction.west:
        return Point(x - 1, y);
      case Direction.east:
        return Point(x + 1, y);
      default:
        return this; // Should not happen
    }
  }

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is Point &&
          runtimeType == other.runtimeType &&
          x == other.x &&
          y == other.y;

  @override
  int get hashCode => x.hashCode ^ y.hashCode;

  @override
  String toString() {
    return '($x, $y)';
  }
}


enum Direction {
  north,
  south,
  west,
  east;

  int get command {
    switch (this) {
      case Direction.north:
        return 1;
      case Direction.south:
        return 2;
      case Direction.west:
        return 3;
      case Direction.east:
        return 4;
      default:
        return -1; // Should not happen
    }
  }
}

class DroidState {
  final List<int> program;
  final Point position;
  final int pathLength;

  DroidState({required this.program, required this.position, required this.pathLength});
}
