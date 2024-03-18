import 'dart:io';
import 'dart:math';

typedef Operation = int Function(List<int>, int, int);

final instructions = {
  'addr': (r, a, b) => r[a] + r[b],
  'addi': (r, a, b) => r[a] + b,
  'mulr': (r, a, b) => r[a] * r[b],
  'muli': (r, a, b) => r[a] * b,
  'banr': (r, a, b) => r[a] & r[b],
  'bani': (r, a, b) => r[a] & b,
  'borr': (r, a, b) => r[a] | r[b],
  'bori': (r, a, b) => r[a] | b,
  'setr': (r, a, b) => r[a],
  'seti': (r, a, b) => a,
  'gtir': (r, a, b) => a > r[b] ? 1 : 0,
  'gtri': (r, a, b) => r[a] > b ? 1 : 0,
  'gtrr': (r, a, b) => r[a] > r[b] ? 1 : 0,
  'eqir': (r, a, b) => a == r[b] ? 1 : 0,
  'eqri': (r, a, b) => r[a] == b ? 1 : 0,
  'eqrr': (r, a, b) => r[a] == r[b] ? 1 : 0,
};

void main() {
  final lines = File('input.txt').readAsLinesSync();
  final ipRegister = int.parse(RegExp(r'\d+').firstMatch(lines.first)!.group(0)!);
  final program = <Function(List<int>)>[];

  for (final line in lines.skip(1)) {
    final parts = line.split(' ');
    final op = instructions[parts[0]]!;
    final a = int.parse(parts[1]);
    final b = int.parse(parts[2]);
    final c = int.parse(parts[3]);
    program.add((r) => r[c] = op(r, a, b));
  }

  final registers = List.filled(6, 0);
  registers[0] = 1;
  runProgram(ipRegister, program, registers, 1000);
  final n = registers.reduce(max);
  int total = 0;
  for (int i = 1; i <= n; i++) {
    if (n % i == 0) {
      total += i;
    }
  }
  print(total);
}

void runProgram(int ipRegister, List<Function(List<int>)> program, List<int> registers, int maxCycles) {
  int ip = 0;
  int cycles = 0;

  while (ip >= 0 && ip < program.length) {
    registers[ipRegister] = ip;
    program[ip](registers);
    ip = registers[ipRegister] + 1;
    cycles++;
    if (maxCycles > 0 && cycles >= maxCycles) {
      break;
    }
  }
}