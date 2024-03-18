import 'dart:io';
import 'dart:convert';

void main() {
  final file = File('input.txt');
  final instructions = file.readAsLinesSync();
  final registers = {'a': 0, 'b': 0, 'c': 0, 'd': 0};
  executeInstructions(instructions, registers);
  print(registers['a']!);
}

void executeInstructions(List<String> instructions, Map<String, int> registers) {
  for (var i = 0; i < instructions.length;) {
    final parts = instructions[i].split(' ');
    switch (parts[0]) {
      case 'cpy':
        final val = getValue(parts[1], registers);
        final register = parts[2];
        if (registers.containsKey(register)) {
          registers[register] = val!;
        }
        i++;
        break;
      case 'inc':
        final register = parts[1];
        if (registers.containsKey(register)) {
          registers[register] = (registers[register] ?? 0) + 1;
        }
        i++;
        break;
      case 'dec':
        final register = parts[1];
        if (registers.containsKey(register)) {
          registers[register] = (registers[register] ?? 0) - 1;
        }
        i++;
        break;
      case 'jnz':
        final val = getValue(parts[1], registers);
        if (val != null && val != 0) {
          final jump = int.parse(parts[2]);
          i += jump;
        } else {
          i++;
        }
        break;
    }
  }
}

int? getValue(String s, Map<String, int> registers) {
  final value = int.tryParse(s);
  if (value != null) {
    return value;
  } else if (registers.containsKey(s)) {
    return registers[s];
  } else {
    return null;
  }
}