
import 'dart:io';
import 'dart:convert';

void main() {
  final file = File('input.txt');
  final lines = file.readAsLinesSync();

  int A = 0, B = 0, C = 0;
  List<int> program = [];

  for (var line in lines) {
    line = line.trim();
    if (line.isEmpty) continue;

    if (line.startsWith('Register A:')) {
      A = int.parse(line.split(':')[1].trim());
    } else if (line.startsWith('Register B:')) {
      B = int.parse(line.split(':')[1].trim());
    } else if (line.startsWith('Register C:')) {
      C = int.parse(line.split(':')[1].trim());
    } else if (line.startsWith('Program:')) {
      program = line.split(':')[1].trim().split(',').map(int.parse).toList();
    }
  }

  int getComboVal(int op) {
    switch (op) {
      case 0:
      case 1:
      case 2:
      case 3:
        return op;
      case 4:
        return A;
      case 5:
        return B;
      case 6:
        return C;
      default:
        throw Exception('invalid combo operand');
    }
  }

  List<String> outputVals = [];
  int ip = 0;
  while (ip < program.length) {
    int opcode = program[ip];
    int operand = program[ip + 1];

    switch (opcode) {
      case 0: // adv
        int den = getComboVal(operand);
        A = den == 0 ? 0 : (A >> den);
        ip += 2;
        break;
      case 1: // bxl
        B ^= operand;
        ip += 2;
        break;
      case 2: // bst
        B = getComboVal(operand) % 8;
        ip += 2;
        break;
      case 3: // jnz
        if (A != 0) {
          ip = operand;
        } else {
          ip += 2;
        }
        break;
      case 4: // bxc
        B ^= C;
        ip += 2;
        break;
      case 5: // out
        outputVals.add((getComboVal(operand) % 8).toString());
        ip += 2;
        break;
      case 6: // bdv
        int den = getComboVal(operand);
        B = A >> den;
        ip += 2;
        break;
      case 7: // cdv
        int den = getComboVal(operand);
        C = A >> den;
        ip += 2;
        break;
      default:
        break;
    }
  }

  print(outputVals.join(','));
}
