import 'dart:io';
import 'dart:convert';
import 'dart:math';

void main() {
  final input = File('input.txt').readAsLinesSync();
  final opcodes = [
    OP('addr', '+', 'r', 'r'),
    OP('addi', '+', 'r', 'v'),
    OP('mulr', '*', 'r', 'r'),
    OP('muli', '*', 'r', 'v'),
    OP('banr', '&', 'r', 'r'),
    OP('bani', '&', 'r', 'v'),
    OP('borr', '|', 'r', 'r'),
    OP('bori', '|', 'r', 'v'),
    OP('setr', 'a', 'r', 'r'),
    OP('seti', 'a', 'v', 'r'),
    OP('gtir', '>', 'v', 'r'),
    OP('gtri', '>', 'r', 'v'),
    OP('gtrr', '>', 'r', 'r'),
    OP('eqir', '=', 'v', 'r'),
    OP('eqri', '=', 'r', 'v'),
    OP('eqrr', '=', 'r', 'r'),
  ];

  int sum = 0;
  int lineCount = 0;
  while (lineCount < input.length) {
    if (input[lineCount].startsWith('B')) {
      final split = input[lineCount].split(RegExp(r'[^0-9]+'));
      final registers = [
        int.parse(split[1]),
        int.parse(split[2]),
        int.parse(split[3]),
        int.parse(split[4]),
      ];
      final split2 = input[lineCount + 1].split(RegExp(r'[^0-9]+'));
      final instruction = [
        int.parse(split2[0]),
        int.parse(split2[1]),
        int.parse(split2[2]),
        int.parse(split2[3]),
      ];
      final split3 = input[lineCount + 2].split(RegExp(r'[^0-9]+'));
      final n = [
        int.parse(split3[1]),
        int.parse(split3[2]),
        int.parse(split3[3]),
        int.parse(split3[4]),
      ];
      final tempSum = testCode(registers, n, instruction, opcodes);
      if (tempSum >= 3) {
        sum++;
      }
      lineCount += 4;
    } else {
      break;
    }
  }

  print(sum);
}

void remove(OP op, int c) {
  final i = op.matchCount.indexOf(c);
  if (i != -1) {
    op.matchCount.removeAt(i);
  }
}

void add(OP op, int c) {
  if (!op.matchCount.contains(c)) {
    op.matchCount.add(c);
  }
}

int testCode(List<int> registers, List<int> n, List<int> instruction, List<OP> opcodes) {
  int sum = 0;
  for (final op in opcodes) {
    if (match(n, runOp(op, registers, instruction))) {
      add(op, instruction[0]);
      sum++;
    }
  }
  return sum;
}

bool match(List<int> r, List<int> c) {
  if (r.length != c.length) {
    return false;
  }
  for (int i = 0; i < r.length; i++) {
    if (r[i] != c[i]) {
      return false;
    }
  }
  return true;
}

List<int> runOp(OP op, List<int> registers, List<int> instruction) {
  final registerCP = [...registers];
  int A, B;
  if (op.a == 'r') {
    A = registerCP[instruction[1]];
  } else {
    A = instruction[1];
  }
  if (op.b == 'r') {
    B = registerCP[instruction[2]];
  } else {
    B = instruction[2];
  }
  switch (op.action) {
    case '+':
      registerCP[instruction[3]] = A + B;
      break;
    case '*':
      registerCP[instruction[3]] = A * B;
      break;
    case '&':
      registerCP[instruction[3]] = A & B;
      break;
    case '|':
      registerCP[instruction[3]] = A | B;
      break;
    case 'a':
      registerCP[instruction[3]] = A;
      break;
    case '>':
      registerCP[instruction[3]] = A > B ? 1 : 0;
      break;
    case '=':
      registerCP[instruction[3]] = A == B ? 1 : 0;
      break;
    default:
      print('not valid instruction');
  }
  return registerCP;
}

class OP {
  final String name;
  final String a;
  final String b;
  final String action;
  final List<int> matchCount = [];

  OP(this.name, this.action, this.a, this.b);
}