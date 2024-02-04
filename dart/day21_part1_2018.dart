
import 'dart:io';

void main() {
  var input = File('input.txt').readAsLinesSync();

  var ipRegister = int.parse(input[0].substring(4));
  var instructions = input.sublist(1);

  var registers = List<int>.filled(6, 0);

  while (registers[ipRegister] < instructions.length) {
    if (registers[ipRegister] == 28) {
      print(registers[5]);
      break;
    }

    var parts = instructions[registers[ipRegister]].split(' ');
    var op = parts[0];
    var A = int.parse(parts[1]);
    var B = int.parse(parts[2]);
    var C = int.parse(parts[3]);

    switch (op) {
      case 'addr':
        registers[C] = registers[A] + registers[B];
        break;
      case 'addi':
        registers[C] = registers[A] + B;
        break;
      case 'mulr':
        registers[C] = registers[A] * registers[B];
        break;
      case 'muli':
        registers[C] = registers[A] * B;
        break;
      case 'banr':
        registers[C] = registers[A] & registers[B];
        break;
      case 'bani':
        registers[C] = registers[A] & B;
        break;
      case 'borr':
        registers[C] = registers[A] | registers[B];
        break;
      case 'bori':
        registers[C] = registers[A] | B;
        break;
      case 'setr':
        registers[C] = registers[A];
        break;
      case 'seti':
        registers[C] = A;
        break;
      case 'gtir':
        registers[C] = A > registers[B] ? 1 : 0;
        break;
      case 'gtri':
        registers[C] = registers[A] > B ? 1 : 0;
        break;
      case 'gtrr':
        registers[C] = registers[A] > registers[B] ? 1 : 0;
        break;
      case 'eqir':
        registers[C] = A == registers[B] ? 1 : 0;
        break;
      case 'eqri':
        registers[C] = registers[A] == B ? 1 : 0;
        break;
      case 'eqrr':
        registers[C] = registers[A] == registers[B] ? 1 : 0;
        break;
    }

    registers[ipRegister]++;
  }
}
