import 'dart:io';
import 'dart:convert';

int getValue(String arg, Map<String, int> registers) {
  if (int.tryParse(arg) != null) {
    return int.parse(arg);
  }
  return registers[arg] ?? 0;
}

void main() async {
  final file = File('input.txt');
  final lines = await file.readAsLines();
  final instructions = lines.map((line) => line.split(' ')).toList();

  final registers = <String, int>{};
  int lastSound = 0;

  for (int i = 0; i < instructions.length;) {
    final instruction = instructions[i];
    final cmd = instruction[0];
    final arg1 = instruction[1];

    switch (cmd) {
      case 'snd':
        lastSound = getValue(arg1, registers);
        break;
      case 'set':
        registers[arg1] = getValue(instruction[2], registers);
        break;
      case 'add':
        registers[arg1] = (getValue(arg1, registers) + getValue(instruction[2], registers));
        break;
      case 'mul':
        registers[arg1] = (getValue(arg1, registers) * getValue(instruction[2], registers));
        break;
      case 'mod':
        registers[arg1] = (getValue(arg1, registers) % getValue(instruction[2], registers));
        break;
      case 'rcv':
        if (getValue(arg1, registers) != 0) {
          print(lastSound);
          return;
        }
        break;
      case 'jgz':
        if (getValue(arg1, registers) > 0) {
          i += getValue(instruction[2], registers);
          continue;
        }
        break;
    }
    i++;
  }
}