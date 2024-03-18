import 'dart:io';

void main() {
  List<String> instructions = File('input.txt').readAsLinesSync();
  Map<String, int> registers = {'a': 1, 'b': 0};

  for (int i = 0; i < instructions.length; i++) {
    List<String> parts = instructions[i].split(' ');

    switch (parts[0]) {
      case 'hlf':
        registers[parts[1]] = (registers[parts[1]]! ~/ 2).toInt();
        break;
      case 'tpl':
        registers[parts[1]] = (registers[parts[1]]! * 3).toInt();
        break;
      case 'inc':
        registers[parts[1]] = (registers[parts[1]]! + 1).toInt();
        break;
      case 'jmp':
        int offset = int.parse(parts[1]);
        i += offset - 1;
        break;
      case 'jie':
        if (registers[parts[1][0]]! % 2 == 0) {
          int offset = int.parse(parts[2]);
          i += offset - 1;
        }
        break;
      case 'jio':
        if (registers[parts[1][0]]! == 1) {
          int offset = int.parse(parts[2]);
          i += offset - 1;
        }
        break;
      default:
        throw 'Unknown instruction: ${parts[0]}';
    }
  }

  print(registers['b']);
}