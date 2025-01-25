
import 'dart:io';
import 'dart:math';

void main() {
  final program = _parseIntcode(File('input.txt').readAsStringSync());

  // Part 1
  int maxSignalPart1 = 0;
  final phaseSettingsPart1 = [0, 1, 2, 3, 4];
  final permutationsPart1 = _getPermutations(phaseSettingsPart1);
  for (final phases in permutationsPart1) {
    int currentSignal = 0;
    for (final phase in phases) {
      final computer = IntcodeComputer(program);
      computer.inputValues.add(phase);
      computer.inputValues.add(currentSignal);
      computer.run();
      currentSignal = computer.outputValues.removeAt(0);
    }
    maxSignalPart1 = max(maxSignalPart1, currentSignal);
  }
  print('Part 1 Max Thruster Signal: $maxSignalPart1');

  // Part 2
  int maxSignalPart2 = 0;
  final phaseSettingsPart2 = [5, 6, 7, 8, 9];
  final permutationsPart2 = _getPermutations(phaseSettingsPart2);

  for (final phases in permutationsPart2) {
    final computers = List.generate(5, (_) => IntcodeComputer(program));
    final feedbackOutputs = List.generate(5, (_) => <int>[]);

    for (int i = 0; i < 5; i++) {
      computers[i].inputValues.add(phases[i]);
    }
    feedbackOutputs[0].add(0);

    bool running = true;
    int amplifierIndex = 0;
    int lastOutput = 0;

    while (running) {
      final computer = computers[amplifierIndex];
      final inputQueue = feedbackOutputs[amplifierIndex];

      while (inputQueue.isNotEmpty && !computer.halted) {
        computer.inputValues.add(inputQueue.removeAt(0));
        computer.run();
        if (computer.outputValues.isNotEmpty) {
          final output = computer.outputValues.removeAt(0);
          lastOutput = output;
          feedbackOutputs[(amplifierIndex + 1) % 5].add(output);
        }
        if (computers.every((c) => c.halted)) {
          running = false;
          break;
        }
      }
      amplifierIndex = (amplifierIndex + 1) % 5;
       if (computers.every((c) => c.halted)) {
          running = false;
          break;
        }
      if (!running && !computers.every((c) => c.halted)) {
         running = computers.any((c) => !c.halted || c.outputValues.isNotEmpty || c.inputValues.isNotEmpty || feedbackOutputs.any((q) => q.isNotEmpty));
      }
    }
    maxSignalPart2 = max(maxSignalPart2, lastOutput);
  }
  print('Part 2 Max Thruster Signal: $maxSignalPart2');
}

List<int> _parseIntcode(String input) {
  return input.trim().split(',').map(int.parse).toList();
}

List<List<int>> _getPermutations(List<int> items) {
  if (items.isEmpty) {
    return [[]];
  }
  final permutations = <List<int>>[];
  for (int i = 0; i < items.length; i++) {
    final currentItem = items[i];
    final remainingItems = List<int>.from(items)..removeAt(i);
    final subPermutations = _getPermutations(remainingItems);
    for (final subPermutation in subPermutations) {
      permutations.add([currentItem, ...subPermutation]);
    }
  }
  return permutations;
}

class IntcodeComputer {
  List<int> memory;
  int instructionPointer = 0;
  List<int> inputValues = [];
  List<int> outputValues = [];
  bool halted = false;

  IntcodeComputer(List<int> program) : memory = List<int>.from(program);

  void run() {
    while (!halted) {
      final instruction = memory[instructionPointer];
      final opcode = instruction % 100;
      final paramMode1 = (instruction ~/ 100) % 10;
      final paramMode2 = (instruction ~/ 1000) % 10;
      final paramMode3 = (instruction ~/ 10000) % 10;

      switch (opcode) {
        case 1: // Add
          _executeOp(opcode, paramMode1, paramMode2, paramMode3);
          instructionPointer += 4;
          break;
        case 2: // Multiply
          _executeOp(opcode, paramMode1, paramMode2, paramMode3);
          instructionPointer += 4;
          break;
        case 3: // Input
          if (inputValues.isEmpty) {
            return; // Pause execution if no input
          }
          final inputValue = inputValues.removeAt(0);
          final param1 = memory[instructionPointer + 1];
          memory[_getParameterAddress(param1, paramMode1)] = inputValue;
          instructionPointer += 2;
          break;
        case 4: // Output
          final param1 = memory[instructionPointer + 1];
          outputValues.add(_getParameterValue(param1, paramMode1));
          instructionPointer += 2;
          break;
        case 5: // Jump-if-true
          if (_getParameterValue(memory[instructionPointer + 1], paramMode1) != 0) {
            instructionPointer = _getParameterValue(memory[instructionPointer + 2], paramMode2);
          } else {
            instructionPointer += 3;
          }
          break;
        case 6: // Jump-if-false
          if (_getParameterValue(memory[instructionPointer + 1], paramMode1) == 0) {
            instructionPointer = _getParameterValue(memory[instructionPointer + 2], paramMode2);
          } else {
            instructionPointer += 3;
          }
          break;
        case 7: // Less than
          _executeOp(opcode, paramMode1, paramMode2, paramMode3);
          instructionPointer += 4;
          break;
        case 8: // Equals
          _executeOp(opcode, paramMode1, paramMode2, paramMode3);
          instructionPointer += 4;
          break;
        case 99: // Halt
          halted = true;
          return;
        default:
          throw Exception('Unknown opcode: $opcode at position $instructionPointer');
      }
    }
  }

  void _executeOp(int opcode, int paramMode1, int paramMode2, int paramMode3) {
    int param1 = memory[instructionPointer + 1];
    int param2 = memory[instructionPointer + 2];
    int param3 = memory[instructionPointer + 3];

    int value1 = _getParameterValue(param1, paramMode1);
    int value2 = _getParameterValue(param2, paramMode2);
    int resultAddress = _getParameterAddress(param3, paramMode3);

    switch (opcode) {
      case 1: // Add
        memory[resultAddress] = value1 + value2;
        break;
      case 2: // Multiply
        memory[resultAddress] = value1 * value2;
        break;
      case 7: // Less than
        memory[resultAddress] = value1 < value2 ? 1 : 0;
        break;
      case 8: // Equals
        memory[resultAddress] = value1 == value2 ? 1 : 0;
        break;
      default:
        throw Exception('Unknown opcode in _executeOp: $opcode');
    }
  }

  int _getParameterValue(int parameter, int mode) {
    if (mode == 1) {
      return parameter; // Immediate mode
    } else {
      return memory[parameter]; // Position mode (mode 0 or default)
    }
  }

    int _getParameterAddress(int parameter, int mode) {
    return parameter; // Address is always the parameter itself for mode 0 and mode 1 in output positions
  }
}
