
import 'dart:io';

void main() {
  final input = File('input.txt').readAsStringSync().trim();

  final parts = input.split('\n\n\n\n');
  final samplesInput = parts[0];
  final programInput = parts[1].trim();

  final samples = parseSamples(samplesInput);
  final program = parseProgram(programInput);

  final part1Result = solvePart1(samples);
  print('Part 1: $part1Result');

  final opcodeMap = determineOpcodeMap(samples);
  final part2Result = solvePart2(program, opcodeMap);
  print('Part 2: $part2Result');
}

List<Sample> parseSamples(String input) {
  final sampleBlocks = input.split('\n\n');
  return sampleBlocks.map((block) {
    final lines = block.trim().split('\n');
    final before = parseRegisters(lines[0].substring(8));
    final instruction = parseInstruction(lines[1]);
    final after = parseRegisters(lines[2].substring(8));
    return Sample(before, instruction, after);
  }).toList();
}

List<int> parseRegisters(String registersStr) {
  return registersStr
      .replaceAll('[', '')
      .replaceAll(']', '')
      .split(', ')
      .map(int.parse)
      .toList();
}

List<int> parseInstruction(String instructionStr) {
  return instructionStr.split(' ').map(int.parse).toList();
}

List<List<int>> parseProgram(String programInput) {
  return programInput.split('\n').map((line) => parseInstruction(line)).toList();
}

class Sample {
  final List<int> before;
  final List<int> instruction;
  final List<int> after;

  Sample(this.before, this.instruction, this.after);
}

typedef OpcodeFn = List<int> Function(List<int> registers, int a, int b, int c);

Map<String, OpcodeFn> opcodes = {
  'addr': (registers, a, b, c) => [...registers]..[c] = registers[a] + registers[b],
  'addi': (registers, a, b, c) => [...registers]..[c] = registers[a] + b,
  'mulr': (registers, a, b, c) => [...registers]..[c] = registers[a] * registers[b],
  'muli': (registers, a, b, c) => [...registers]..[c] = registers[a] * b,
  'banr': (registers, a, b, c) => [...registers]..[c] = registers[a] & registers[b],
  'bani': (registers, a, b, c) => [...registers]..[c] = registers[a] & b,
  'borr': (registers, a, b, c) => [...registers]..[c] = registers[a] | registers[b],
  'bori': (registers, a, b, c) => [...registers]..[c] = registers[a] | b,
  'setr': (registers, a, b, c) => [...registers]..[c] = registers[a],
  'seti': (registers, a, b, c) => [...registers]..[c] = a,
  'gtir': (registers, a, b, c) => [...registers]..[c] = a > registers[b] ? 1 : 0,
  'gtri': (registers, a, b, c) => [...registers]..[c] = registers[a] > b ? 1 : 0,
  'gtrr': (registers, a, b, c) => [...registers]..[c] = registers[a] > registers[b] ? 1 : 0,
  'eqir': (registers, a, b, c) => [...registers]..[c] = a == registers[b] ? 1 : 0,
  'eqri': (registers, a, b, c) => [...registers]..[c] = registers[a] == b ? 1 : 0,
  'eqrr': (registers, a, b, c) => [...registers]..[c] = registers[a] == registers[b] ? 1 : 0,
};

int solvePart1(List<Sample> samples) {
  int count = 0;
  for (final sample in samples) {
    int possibleOpcodes = 0;
    for (final opcodeFn in opcodes.values) {
      final result = opcodeFn(sample.before, sample.instruction[1], sample.instruction[2], sample.instruction[3]);
      if (listEquals(result, sample.after)) {
        possibleOpcodes++;
      }
    }
    if (possibleOpcodes >= 3) {
      count++;
    }
  }
  return count;
}

Map<int, String> determineOpcodeMap(List<Sample> samples) {
  final possibleOpcodes = <int, Set<String>>{};
  for (int i = 0; i <= 15; i++) {
    possibleOpcodes[i] = opcodes.keys.toSet();
  }

  for (final sample in samples) {
    final instruction = sample.instruction;
    final opcodeNumber = instruction[0];
    final a = instruction[1];
    final b = instruction[2];
    final c = instruction[3];

    final validOpcodesForSample = <String>{};
    for (final entry in opcodes.entries) {
      final opcodeName = entry.key;
      final opcodeFn = entry.value;
      final result = opcodeFn(sample.before, a, b, c);
      if (listEquals(result, sample.after)) {
        validOpcodesForSample.add(opcodeName);
      }
    }
    possibleOpcodes[opcodeNumber] = possibleOpcodes[opcodeNumber]!.intersection(validOpcodesForSample);
  }

  final opcodeMap = <int, String>{};
  while (opcodeMap.length < 16) {
    for (int opcodeNumber = 0; opcodeNumber <= 15; opcodeNumber++) {
      if (opcodeMap.containsKey(opcodeNumber)) continue;
      final possible = possibleOpcodes[opcodeNumber]!;
      if (possible.length == 1) {
        final opcodeName = possible.first;
        opcodeMap[opcodeNumber] = opcodeName;
        for (int i = 0; i <= 15; i++) {
          if (i != opcodeNumber) {
            possibleOpcodes[i]!.remove(opcodeName);
          }
        }
      }
    }
  }
  return opcodeMap;
}

int solvePart2(List<List<int>> program, Map<int, String> opcodeMap) {
  List<int> registers = [0, 0, 0, 0];
  for (final instruction in program) {
    final opcodeNumber = instruction[0];
    final a = instruction[1];
    final b = instruction[2];
    final c = instruction[3];
    final opcodeName = opcodeMap[opcodeNumber]!;
    registers = opcodes[opcodeName]!(registers, a, b, c);
  }
  return registers[0];
}

bool listEquals<T>(List<T> list1, List<T> list2) {
  if (list1.length != list2.length) {
    return false;
  }
  for (int i = 0; i < list1.length; i++) {
    if (list1[i] != list2[i]) {
      return false;
    }
  }
  return true;
}
