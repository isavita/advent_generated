import 'dart:io';
import 'dart:convert';

void main() {
  List<String> instructions = readInstructions('input.txt');
  Map<String, int> registers = {'a': 7, 'b': 0, 'c': 0, 'd': 0};
  executeInstructions(instructions, registers);
  print(registers['a']);
}

List<String> readInstructions(String filename) {
  List<String> instructions = [];
  File file = File(filename);
  for (String line in file.readAsLinesSync()) {
    instructions.add(line);
  }
  return instructions;
}

void executeInstructions(List<String> instructions, Map<String, int> registers) {
  int pc = 0;
  while (pc < instructions.length) {
    List<String> fields = instructions[pc].split(' ');
    switch (fields[0]) {
      case 'cpy':
        int x = getValue(fields[1], registers);
        if (registers.containsKey(fields[2])) {
          registers[fields[2]] = x;
        }
        break;
      case 'inc':
        if (registers.containsKey(fields[1])) {
          registers[fields[1]] = registers[fields[1]]! + 1;
        }
        break;
      case 'dec':
        if (registers.containsKey(fields[1])) {
          registers[fields[1]] = registers[fields[1]]! - 1;
        }
        break;
      case 'jnz':
        int x = getValue(fields[1], registers);
        if (x != 0) {
          pc += getValue(fields[2], registers) - 1;
        }
        break;
      case 'tgl':
        int x = getValue(fields[1], registers);
        int tgt = pc + x;
        if (tgt >= 0 && tgt < instructions.length) {
          instructions[tgt] = toggleInstruction(instructions[tgt]);
        }
        break;
    }
    pc++;
  }
}

int getValue(String s, Map<String, int> registers) {
  int? value = int.tryParse(s);
  return value ?? registers[s]!;
}

String toggleInstruction(String instr) {
  List<String> parts = instr.split(' ');
  switch (parts[0]) {
    case 'inc':
      parts[0] = 'dec';
      break;
    case 'dec':
    case 'tgl':
      parts[0] = 'inc';
      break;
    case 'jnz':
      parts[0] = 'cpy';
      break;
    case 'cpy':
      parts[0] = 'jnz';
      break;
  }
  return parts.join(' ');
}