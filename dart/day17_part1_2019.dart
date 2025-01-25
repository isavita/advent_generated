
import 'dart:io';

void main() {
  final programCode = File('input.txt').readAsStringSync().trim().split(',').map(int.parse).toList();

  final computer = IntcodeComputer(programCode);
  final output = computer.run();

  final grid = parseGrid(output);
  final intersectionSum = calculateIntersectionSum(grid);

  print(intersectionSum);
}

class IntcodeComputer {
  List<int> memory;
  int instructionPointer = 0;
  int relativeBase = 0;
  List<int> outputBuffer = [];

  IntcodeComputer(List<int> program) : memory = [...program];

  List<int> run() {
    outputBuffer = [];
    while (true) {
      final opcode = memory[instructionPointer] % 100;
      final paramModes = (memory[instructionPointer] ~/ 100).toString().padLeft(3, '0').split('').reversed.map(int.parse).toList();

      if (opcode == 99) {
        break;
      }

      switch (opcode) {
        case 1: // Add
        case 2: // Multiply
          _executeArithmeticOp(opcode, paramModes);
          break;
        case 3: // Input (not used in this problem, but included for completeness)
          throw Exception('Input opcode 3 encountered but no input provided.');
        case 4: // Output
          outputBuffer.add(_getParameterValue(1, paramModes[0]));
          instructionPointer += 2;
          break;
        case 5: // Jump-if-true
        case 6: // Jump-if-false
          _executeJumpOp(opcode, paramModes);
          break;
        case 7: // Less than
        case 8: // Equals
          _executeComparisonOp(opcode, paramModes);
          break;
        case 9: // Adjust relative base
          relativeBase += _getParameterValue(1, paramModes[0]);
          instructionPointer += 2;
          break;
        default:
          throw Exception('Unknown opcode: $opcode at position $instructionPointer');
      }
    }
    return outputBuffer;
  }

  void _executeArithmeticOp(int opcode, List<int> paramModes) {
    final param1 = _getParameterValue(1, paramModes[0]);
    final param2 = _getParameterValue(2, paramModes[1]);
    final resultAddress = _getParameterAddress(3, paramModes[2]);

    final result = opcode == 1 ? param1 + param2 : param1 * param2;
    _ensureMemorySize(resultAddress);
    memory[resultAddress] = result;
    instructionPointer += 4;
  }

  void _executeJumpOp(int opcode, List<int> paramModes) {
    final param1 = _getParameterValue(1, paramModes[0]);
    final param2 = _getParameterValue(2, paramModes[1]);

    bool jump = false;
    if (opcode == 5 && param1 != 0) {
      jump = true;
    } else if (opcode == 6 && param1 == 0) {
      jump = true;
    }

    if (jump) {
      instructionPointer = param2;
    } else {
      instructionPointer += 3;
    }
  }

  void _executeComparisonOp(int opcode, List<int> paramModes) {
    final param1 = _getParameterValue(1, paramModes[0]);
    final param2 = _getParameterValue(2, paramModes[1]);
    final resultAddress = _getParameterAddress(3, paramModes[2]);

    int result = 0;
    if ((opcode == 7 && param1 < param2) || (opcode == 8 && param1 == param2)) {
      result = 1;
    }

    _ensureMemorySize(resultAddress);
    memory[resultAddress] = result;
    instructionPointer += 4;
  }


  int _getParameterValue(int paramIndex, int mode) {
    final address = memory[instructionPointer + paramIndex];
    switch (mode) {
      case 0: // Position mode
        _ensureMemorySize(address);
        return memory[address];
      case 1: // Immediate mode
        return address;
      case 2: // Relative mode
        _ensureMemorySize(relativeBase + address);
        return memory[relativeBase + address];
      default:
        throw Exception('Invalid parameter mode: $mode');
    }
  }

    int _getParameterAddress(int paramIndex, int mode) {
    final address = memory[instructionPointer + paramIndex];
    switch (mode) {
      case 0: // Position mode
        return address;
      case 2: // Relative mode
        return relativeBase + address;
      default:
        throw Exception('Invalid parameter mode for address: $mode');
    }
  }


  void _ensureMemorySize(int address) {
    if (address >= memory.length) {
      memory.addAll(List.filled(address - memory.length + 1, 0));
    }
  }
}

List<List<String>> parseGrid(List<int> output) {
  final grid = <List<String>>[];
  List<String> currentRow = [];
  for (final asciiCode in output) {
    if (asciiCode == 10) {
      grid.add(currentRow);
      currentRow = [];
    } else {
      currentRow.add(String.fromCharCode(asciiCode));
    }
  }
  if (currentRow.isNotEmpty) {
    grid.add(currentRow);
  }
  return grid;
}

int calculateIntersectionSum(List<List<String>> grid) {
  int sum = 0;
  for (int y = 1; y < grid.length - 1; y++) {
    for (int x = 1; x < grid[y].length - 1; x++) {
      if (isIntersection(grid, x, y)) {
        sum += x * y;
      }
    }
  }
  return sum;
}

bool isIntersection(List<List<String>> grid, int x, int y) {
  if (grid[y][x] == '#') {
    return isScaffold(grid, x + 1, y) &&
           isScaffold(grid, x - 1, y) &&
           isScaffold(grid, x, y + 1) &&
           isScaffold(grid, x, y - 1);
  }
  return false;
}

bool isScaffold(List<List<String>> grid, int x, int y) {
  if (y >= 0 && y < grid.length && x >= 0 && x < grid[y].length) {
    final char = grid[y][x];
    return char == '#' || char == '^' || char == 'v' || char == '<' || char == '>';
  }
  return false;
}
