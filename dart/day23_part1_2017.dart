import 'dart:io';

void main() {
  var mulCount = 0;
  var pointer = 0;
  var registers = <String, int>{};
  var instructions = <String>[];

  var file = File('input.txt');
  var lines = file.readAsLinesSync();

  for (var line in lines) {
    instructions.add(line);
  }

  int getValue(String s) {
    try {
      return int.parse(s);
    } catch (e) {
      return registers[s] ?? 0;
    }
  }

  while (pointer >= 0 && pointer < instructions.length) {
    var parts = instructions[pointer].split(' ');
    var cmd = parts[0];
    var x = parts[1];
    var y = parts[2];

    switch (cmd) {
      case 'set':
        registers[x] = getValue(y);
        break;
      case 'sub':
        registers[x] = (registers[x] ?? 0) - getValue(y);
        break;
      case 'mul':
        registers[x] = (registers[x] ?? 0) * getValue(y);
        mulCount++;
        break;
      case 'jnz':
        if (getValue(x) != 0) {
          pointer += getValue(y) - 1;
        }
        break;
    }

    pointer++;
  }

  print(mulCount);
}