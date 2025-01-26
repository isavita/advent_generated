
import 'dart:io';

int getValue(String operand, Map<String, int> registers) {
  try {
    return int.parse(operand);
  } catch (e) {
    return registers[operand] ?? 0;
  }
}

void main() {
  final instructions = File('input.txt').readAsLinesSync();

  // Part 1
  int? recoveredFrequency;
  int lastSound = 0;
  Map<String, int> registers1 = {};
  int pc1 = 0;

  while (pc1 >= 0 && pc1 < instructions.length) {
    final instructionParts = instructions[pc1].split(' ');
    final opcode = instructionParts[0];
    final regX = instructionParts[1];
    final operandY = instructionParts.length > 2 ? instructionParts[2] : null;

    switch (opcode) {
      case 'snd':
        lastSound = getValue(regX, registers1);
        break;
      case 'set':
        registers1[regX] = getValue(operandY!, registers1);
        break;
      case 'add':
        registers1[regX] = (registers1[regX] ?? 0) + getValue(operandY!, registers1);
        break;
      case 'mul':
        registers1[regX] = (registers1[regX] ?? 0) * getValue(operandY!, registers1);
        break;
      case 'mod':
        registers1[regX] = (registers1[regX] ?? 0) % getValue(operandY!, registers1);
        break;
      case 'rcv':
        if (getValue(regX, registers1) != 0) {
          recoveredFrequency = lastSound;
          break; // Exit loop after recovering
        }
        break;
      case 'jgz':
        if (getValue(regX, registers1) > 0) {
          pc1 += getValue(operandY!, registers1);
          continue; // Skip pc1++
        }
        break;
    }
    if (recoveredFrequency != null) {
      break; // Exit while loop
    }
    pc1++;
  }
  print('Part 1: $recoveredFrequency');

  // Part 2
  int program1SendCount = 0;
  List<int> queue0To1 = [];
  List<int> queue1To0 = [];
  Map<String, int> registersP0 = {'p': 0};
  Map<String, int> registersP1 = {'p': 1};
  int pcP0 = 0;
  int pcP1 = 0;
  bool blockedP0 = false;
  bool blockedP1 = false;

  while (true) {
    blockedP0 = false;
    blockedP1 = false;

    // Program 0 turn
    while (!blockedP0) {
      if (pcP0 < 0 || pcP0 >= instructions.length) {
        blockedP0 = true;
        break;
      }
      final instructionPartsP0 = instructions[pcP0].split(' ');
      final opcodeP0 = instructionPartsP0[0];
      final regXP0 = instructionPartsP0[1];
      final operandYP0 = instructionPartsP0.length > 2 ? instructionPartsP0[2] : null;

      switch (opcodeP0) {
        case 'snd':
          queue0To1.add(getValue(regXP0, registersP0));
          break;
        case 'set':
          registersP0[regXP0] = getValue(operandYP0!, registersP0);
          break;
        case 'add':
          registersP0[regXP0] = (registersP0[regXP0] ?? 0) + getValue(operandYP0!, registersP0);
          break;
        case 'mul':
          registersP0[regXP0] = (registersP0[regXP0] ?? 0) * getValue(operandYP0!, registersP0);
          break;
        case 'mod':
          registersP0[regXP0] = (registersP0[regXP0] ?? 0) % getValue(operandYP0!, registersP0);
          break;
        case 'rcv':
          if (queue1To0.isNotEmpty) {
            registersP0[regXP0] = queue1To0.removeAt(0);
          } else {
            blockedP0 = true;
            break; // Block program 0
          }
          break;
        case 'jgz':
          if (getValue(regXP0, registersP0) > 0) {
            pcP0 += getValue(operandYP0!, registersP0);
            continue;
          }
          break;
      }
      if (blockedP0) break;
      pcP0++;
    }

    // Program 1 turn
    while (!blockedP1) {
      if (pcP1 < 0 || pcP1 >= instructions.length) {
        blockedP1 = true;
        break;
      }
      final instructionPartsP1 = instructions[pcP1].split(' ');
      final opcodeP1 = instructionPartsP1[0];
      final regXP1 = instructionPartsP1[1];
      final operandYP1 = instructionPartsP1.length > 2 ? instructionPartsP1[2] : null;

      switch (opcodeP1) {
        case 'snd':
          queue1To0.add(getValue(regXP1, registersP1));
          program1SendCount++;
          break;
        case 'set':
          registersP1[regXP1] = getValue(operandYP1!, registersP1);
          break;
        case 'add':
          registersP1[regXP1] = (registersP1[regXP1] ?? 0) + getValue(operandYP1!, registersP1);
          break;
        case 'mul':
          registersP1[regXP1] = (registersP1[regXP1] ?? 0) * getValue(operandYP1!, registersP1);
          break;
        case 'mod':
          registersP1[regXP1] = (registersP1[regXP1] ?? 0) % getValue(operandYP1!, registersP1);
          break;
        case 'rcv':
          if (queue0To1.isNotEmpty) {
            registersP1[regXP1] = queue0To1.removeAt(0);
          } else {
            blockedP1 = true;
            break; // Block program 1
          }
          break;
        case 'jgz':
          if (getValue(regXP1, registersP1) > 0) {
            pcP1 += getValue(operandYP1!, registersP1);
            continue;
          }
          break;
      }
      if (blockedP1) break;
      pcP1++;
    }

    if (blockedP0 && blockedP1 && queue0To1.isEmpty && queue1To0.isEmpty) {
      break; // Deadlock
    }
    if (pcP0 < 0 && pcP1 < 0 && pcP0 >= instructions.length && pcP1 >= instructions.length) {
        break; // Both terminated naturally
    }
    if (pcP0 >= instructions.length && blockedP1 && queue0To1.isEmpty) break;
     if (pcP1 >= instructions.length && blockedP0 && queue1To0.isEmpty) break;
  }

  print('Part 2: $program1SendCount');
}
