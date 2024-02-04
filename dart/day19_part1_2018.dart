
import 'dart:io';

void main() {
  var registers = [0, 0, 0, 0, 0, 0];
  var ipRegister = 0;
  var instructions = <List<String>>[];

  var lines = File('input.txt').readAsLinesSync();
  ipRegister = int.parse(lines[0].split(' ')[1]);

  for (int i = 1; i < lines.length; i++) {
    instructions.add(lines[i].split(' '));
  }

  while (registers[ipRegister] < instructions.length) {
    var instruction = instructions[registers[ipRegister]];
    executeInstruction(instruction, registers);
    registers[ipRegister]++;
  }

  print(registers[0]);
}

void executeInstruction(List<String> instruction, List<int> registers) {
  var opcode = instruction[0];
  var A = int.parse(instruction[1]);
  var B = int.parse(instruction[2]);
  var C = int.parse(instruction[3]);

  switch (opcode) {
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
}
