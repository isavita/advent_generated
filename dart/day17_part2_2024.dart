
import 'dart:io';
import 'dart:math';

class Program {
  int a;
  int b;
  int c;
  List<int> program;

  Program(this.a, this.b, this.c, this.program);
}

int computeOperand(int val, int a, int b, int c) {
  switch (val) {
    case 0:
    case 1:
    case 2:
    case 3:
      return val;
    case 4:
      return a;
    case 5:
      return b;
    case 6:
      return c;
    default:
      throw Exception("Invalid combo operand: $val");
  }
}

List<int> simulateComputer(Program program) {
  List<int> outs = [];
  int a = program.a;
  int b = program.b;
  int c = program.c;
  List<int> input = program.program;

  for (int i = 1; i <= input.length; i += 2) {
    int cmd = input[i - 1];
    switch (cmd) {
      case 0:
        a >>= computeOperand(input[i], a, b, c);
        break;
      case 1:
        b ^= input[i];
        break;
      case 2:
        b = computeOperand(input[i], a, b, c) % 8;
        break;
      case 3:
        if (a != 0) {
          i = input[i] - 1;
        }
        break;
      case 4:
        b ^= c;
        break;
      case 5:
        outs.add(computeOperand(input[i], a, b, c) % 8);
        break;
      case 6:
        b = a >> computeOperand(input[i], a, b, c);
        break;
      case 7:
        c = a >> computeOperand(input[i], a, b, c);
        break;
      default:
        throw Exception("Invalid opcode: $cmd");
    }
  }
  return outs;
}

class Pair {
  int a;
  int b;

  Pair(this.a, this.b);

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is Pair &&
          runtimeType == other.runtimeType &&
          a == other.a &&
          b == other.b;

  @override
  int get hashCode => a.hashCode ^ b.hashCode;
}

List<int> check(Program p) {
  List<int> program = p.program;
  List<int> valids = [];
  List<Pair> stack = [Pair(0, 0)];
  Set<Pair> seen = {};

  while (stack.isNotEmpty) {
    Pair state = stack.removeLast();

    if (seen.contains(state)) {
      continue;
    }
    seen.add(state);

    int depth = state.a;
    int score = state.b;

    if (depth == program.length) {
      valids.add(score);
    } else {
      for (int i = 0; i < 8; i++) {
        int newScore = i + 8 * score;
        Program testProgram = Program(newScore, p.b, p.c, program);
        List<int> result = simulateComputer(testProgram);
        if (result.isNotEmpty && result[0] == program[program.length - 1 - depth]) {
          stack.add(Pair(depth + 1, newScore));
        }
      }
    }
  }
  return valids;
}

void main() {
  File file = File('input.txt');
  List<String> lines = file.readAsLinesSync();

  int a = 0, b = 0, c = 0;
  List<int> program = [];

  for (String line in lines) {
    line = line.trim();
    if (line.startsWith("Register A:")) {
      a = int.parse(line.split(":")[1].trim());
    } else if (line.startsWith("Register B:")) {
      b = int.parse(line.split(":")[1].trim());
    } else if (line.startsWith("Register C:")) {
      c = int.parse(line.split(":")[1].trim());
    } else if (line.startsWith("Program:")) {
      program = line.split(":")[1].trim().split(",").map((n) => int.parse(n.trim())).toList();
    }
  }

  Program p = Program(a, b, c, program);
  List<int> validValues = check(p);
  int minVal = validValues.reduce(min);

  print(minVal);
}
