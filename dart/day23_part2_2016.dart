
import 'dart:io';

void main() {
  final instructions = readInstructions('input.txt');

  // Part 1
  final registersPart1 = {'a': 7, 'b': 0, 'c': 0, 'd': 0};
  runAssembunny(instructions, registersPart1);
  print('Part 1: ${registersPart1['a']}');

  // Part 2
  final instructionsPart2 = readInstructions('input.txt'); // Re-read instructions as they are modified in place
  final registersPart2 = {'a': 12, 'b': 0, 'c': 0, 'd': 0};
  runAssembunny(instructionsPart2, registersPart2);
  print('Part 2: ${registersPart2['a']}');
}

List<List<String>> readInstructions(String filePath) {
  final instructions = <List<String>>[];
  final file = File(filePath);
  final lines = file.readAsLinesSync();
  for (final line in lines) {
    instructions.add(line.split(' '));
  }
  return instructions;
}

void runAssembunny(List<List<String>> instructions, Map<String, int> registers) {
  int instructionPointer = 0;
  while (instructionPointer >= 0 && instructionPointer < instructions.length) {
    final instruction = instructions[instructionPointer];
    final opcode = instruction[0];

    switch (opcode) {
      case 'cpy':
        if (instruction.length == 3) {
          final val = getValue(instruction[1], registers);
          final regDest = instruction[2];
          if (registers.containsKey(regDest)) {
            registers[regDest] = val;
          }
        }
        instructionPointer++;
        break;
      case 'inc':
        if (instruction.length == 2) {
          final reg = instruction[1];
          if (registers.containsKey(reg)) {
            registers[reg] = registers[reg]! + 1;
          }
        }
        instructionPointer++;
        break;
      case 'dec':
        if (instruction.length == 2) {
          final reg = instruction[1];
          if (registers.containsKey(reg)) {
            registers[reg] = registers[reg]! - 1;
          }
        }
        instructionPointer++;
        break;
      case 'jnz':
        if (instruction.length == 3) {
          final valCheck = getValue(instruction[1], registers);
          final jumpVal = getValue(instruction[2], registers);
          if (valCheck != 0) {
            instructionPointer += jumpVal;
          } else {
            instructionPointer++;
          }
        } else {
          instructionPointer++; // Skip invalid instruction
        }
        break;
      case 'tgl':
        if (instruction.length == 2) {
          final offset = getValue(instruction[1], registers);
          final targetIndex = instructionPointer + offset;
          if (targetIndex >= 0 && targetIndex < instructions.length) {
            instructions[targetIndex] = toggleInstruction(instructions[targetIndex]);
          }
        }
        instructionPointer++;
        break;
      default:
        instructionPointer++; // Skip unknown instruction
    }
  }
}

int getValue(String arg, Map<String, int> registers) {
  if (registers.containsKey(arg)) {
    return registers[arg]!;
  } else {
    return int.tryParse(arg) ?? 0;
  }
}

List<String> toggleInstruction(List<String> instruction) {
  if (instruction.isEmpty) return instruction;

  final opcode = instruction[0];
  switch (instruction.length) {
    case 2:
      if (opcode == 'inc') {
        return ['dec', instruction[1]];
      } else if (opcode == 'dec' || opcode == 'tgl') {
        return ['inc', instruction[1]];
      }
      break;
    case 3:
      if (opcode == 'jnz') {
        return ['cpy', instruction[1], instruction[2]];
      } else if (opcode == 'cpy') {
        return ['jnz', instruction[1], instruction[2]];
      }
      break;
  }
  return instruction; // No change for invalid toggle
}
