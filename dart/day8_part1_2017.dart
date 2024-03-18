import 'dart:io';

void main() {
  File file = File('input.txt');
  List<String> lines = file.readAsLinesSync();

  Map<String, int> registers = {};

  for (String line in lines) {
    List<String> parts = line.split(' ');
    String reg = parts[0];
    String op = parts[1];
    int amount = int.parse(parts[2]);
    String condReg = parts[4];
    String condOp = parts[5];
    int condVal = int.parse(parts[6]);

    registers.putIfAbsent(reg, () => 0);
    registers.putIfAbsent(condReg, () => 0);

    bool cond = false;
    switch (condOp) {
      case '>':
        cond = registers[condReg]! > condVal;
        break;
      case '>=':
        cond = registers[condReg]! >= condVal;
        break;
      case '<':
        cond = registers[condReg]! < condVal;
        break;
      case '<=':
        cond = registers[condReg]! <= condVal;
        break;
      case '==':
        cond = registers[condReg]! == condVal;
        break;
      case '!=':
        cond = registers[condReg]! != condVal;
        break;
    }

    if (cond) {
      switch (op) {
        case 'inc':
          registers[reg] = (registers[reg] ?? 0) + amount;
          break;
        case 'dec':
          registers[reg] = (registers[reg] ?? 0) - amount;
          break;
      }
    }
  }

  int maxValue = registers.values.reduce((value, element) => value > element ? value : element);

  print(maxValue);
}