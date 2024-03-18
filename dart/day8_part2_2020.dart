import 'dart:io';

void main() {
  final instructions = File('input.txt').readAsLinesSync();
  int accumulator = 0;
  bool terminated = false;

  for (int i = 0; i < instructions.length; i++) {
    final op = instructions[i].split(' ')[0];
    final arg = int.parse(instructions[i].split(' ')[1]);

    if (op == 'acc') {
      continue;
    }

    List<String> modifiedInstructions = List.from(instructions);
    if (op == 'jmp') {
      modifiedInstructions[i] = 'nop $arg';
    } else {
      modifiedInstructions[i] = 'jmp $arg';
    }

    final result = executeBootCode(modifiedInstructions);
    if (result[1]) {
      print(result[0]);
      terminated = true;
      break;
    }
  }

  if (!terminated) {
    print('No solution found');
  }
}

List<dynamic> executeBootCode(List<String> instructions) {
  int accumulator = 0;
  final visited = <int, bool>{};
  int currentInstruction = 0;

  while (currentInstruction < instructions.length) {
    if (visited[currentInstruction] ?? false) {
      return [accumulator, false];
    }

    visited[currentInstruction] = true;
    final op = instructions[currentInstruction].split(' ')[0];
    final arg = int.parse(instructions[currentInstruction].split(' ')[1]);

    switch (op) {
      case 'acc':
        accumulator += arg;
        currentInstruction++;
        break;
      case 'jmp':
        currentInstruction += arg;
        break;
      case 'nop':
        currentInstruction++;
        break;
    }
  }

  return [accumulator, true];
}