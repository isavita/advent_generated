import 'dart:io';
import 'dart:convert';

void main() {
  var instructions = File('input.txt').readAsLinesSync();
  int a = 1;
  while (true) {
    if (producesClockSignal(a, instructions)) {
      print(a);
      break;
    }
    a++;
  }
}

bool producesClockSignal(int a, List<String> instructions) {
  Map<String, int> registers = {'a': a, 'b': 0, 'c': 0, 'd': 0};
  int lastOutput = 0, outputCount = 0;

  for (int i = 0; i < instructions.length;) {
    List<String> parts = instructions[i].split(' ');
    switch (parts[0]) {
      case 'cpy':
        int val = getValue(parts[1], registers);
        registers[parts[2]] = val;
        break;
      case 'inc':
        if (registers.containsKey(parts[1])) {
          registers[parts[1]] = (registers[parts[1]] ?? 0) + 1;
        } else {
          registers[parts[1]] = 1;
        }
        break;
      case 'dec':
        if (registers.containsKey(parts[1])) {
          registers[parts[1]] = (registers[parts[1]] ?? 0) - 1;
        } else {
          registers[parts[1]] = -1;
        }
        break;
      case 'jnz':
        int val = getValue(parts[1], registers);
        if (val != 0) {
          int jump = getValue(parts[2], registers);
          i += jump;
          continue;
        }
        break;
      case 'out':
        int val = getValue(parts[1], registers);
        if (val != 0 && val != 1) {
          return false;
        }
        if (outputCount > 0 && val == lastOutput) {
          return false;
        }
        lastOutput = val;
        outputCount++;
        if (outputCount > 50) {
          return true;
        }
        break;
    }
    i++;
  }
  return false;
}

int getValue(String s, Map<String, int> registers) {
  int? val = int.tryParse(s);
  if (val != null) {
    return val;
  }
  return registers[s] ?? 0;
}