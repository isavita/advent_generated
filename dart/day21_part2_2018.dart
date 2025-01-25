
import 'dart:io';

typedef OpcodeFunc = List<int> Function(List<int>, List<int>);

class Instruction {
  String name;
  List<int> abcValues;
  Instruction(this.name, this.abcValues);
}

class OpcodeComputer {
  List<Instruction> instructions;
  List<int> registers = List.filled(6, 0);
  int instructionPointer;
  OpcodeComputer(this.instructions, this.instructionPointer);

  bool tick() {
    if (registers[instructionPointer] >= instructions.length) {
      return true;
    }
    int instIndex = registers[instructionPointer];
    Instruction inst = instructions[instIndex];
    OpcodeFunc opcodeFunc = opcodeNamesToFuncs[inst.name]!;
    registers = opcodeFunc(registers, inst.abcValues);
    registers[instructionPointer]++;
    if (registers[instructionPointer] >= instructions.length) {
      return true;
    }
    return false;
  }
}

Map<String, OpcodeFunc> opcodeNamesToFuncs = {
  "addr": addr,
  "addi": addi,
  "mulr": mulr,
  "muli": muli,
  "banr": banr,
  "bani": bani,
  "borr": borr,
  "bori": bori,
  "setr": setr,
  "seti": seti,
  "gtir": gtir,
  "gtri": gtri,
  "gtrr": gtrr,
  "eqir": eqir,
  "eqri": eqri,
  "eqrr": eqrr,
};

List<int> addr(List<int> registers, List<int> abcValues) {
  registers[abcValues[2]] = registers[abcValues[0]] + registers[abcValues[1]];
  return registers;
}

List<int> addi(List<int> registers, List<int> abcValues) {
  registers[abcValues[2]] = registers[abcValues[0]] + abcValues[1];
  return registers;
}

List<int> mulr(List<int> registers, List<int> abcValues) {
  registers[abcValues[2]] = registers[abcValues[0]] * registers[abcValues[1]];
  return registers;
}

List<int> muli(List<int> registers, List<int> abcValues) {
  registers[abcValues[2]] = registers[abcValues[0]] * abcValues[1];
  return registers;
}

List<int> banr(List<int> registers, List<int> abcValues) {
  registers[abcValues[2]] = registers[abcValues[0]] & registers[abcValues[1]];
  return registers;
}

List<int> bani(List<int> registers, List<int> abcValues) {
  registers[abcValues[2]] = registers[abcValues[0]] & abcValues[1];
  return registers;
}

List<int> borr(List<int> registers, List<int> abcValues) {
  registers[abcValues[2]] = registers[abcValues[0]] | registers[abcValues[1]];
  return registers;
}

List<int> bori(List<int> registers, List<int> abcValues) {
  registers[abcValues[2]] = registers[abcValues[0]] | abcValues[1];
  return registers;
}

List<int> setr(List<int> registers, List<int> abcValues) {
  registers[abcValues[2]] = registers[abcValues[0]];
  return registers;
}

List<int> seti(List<int> registers, List<int> abcValues) {
  registers[abcValues[2]] = abcValues[0];
  return registers;
}

List<int> gtir(List<int> registers, List<int> abcValues) {
  registers[abcValues[2]] = abcValues[0] > registers[abcValues[1]] ? 1 : 0;
  return registers;
}

List<int> gtri(List<int> registers, List<int> abcValues) {
  registers[abcValues[2]] = registers[abcValues[0]] > abcValues[1] ? 1 : 0;
  return registers;
}

List<int> gtrr(List<int> registers, List<int> abcValues) {
  registers[abcValues[2]] = registers[abcValues[0]] > registers[abcValues[1]] ? 1 : 0;
  return registers;
}

List<int> eqir(List<int> registers, List<int> abcValues) {
  registers[abcValues[2]] = abcValues[0] == registers[abcValues[1]] ? 1 : 0;
  return registers;
}

List<int> eqri(List<int> registers, List<int> abcValues) {
  registers[abcValues[2]] = registers[abcValues[0]] == abcValues[1] ? 1 : 0;
  return registers;
}

List<int> eqrr(List<int> registers, List<int> abcValues) {
  registers[abcValues[2]] = registers[abcValues[0]] == registers[abcValues[1]] ? 1 : 0;
  return registers;
}

OpcodeComputer parseInput(String input) {
  List<String> lines = input.split("\n");
  int instructionPointer = int.parse(lines[0].split(" ")[1]);
  List<Instruction> instructions = [];
  for (String line in lines.sublist(1)) {
    List<String> parts = line.split(" ");
    String name = parts[0];
    List<int> abcValues = [
      int.parse(parts[1]),
      int.parse(parts[2]),
      int.parse(parts[3])
    ];
    instructions.add(Instruction(name, abcValues));
  }
  return OpcodeComputer(instructions, instructionPointer);
}

int solve(String input) {
  OpcodeComputer opcodeComputer = parseInput(input);
  Set<int> comparedRegister5s = {};
  int lastReg5 = 0;
    while (true) {
        if (opcodeComputer.registers[opcodeComputer.instructionPointer] == 28)
        {
            int reg5 = opcodeComputer.registers[5];
            if (comparedRegister5s.contains(reg5))
            {
                break;
            }
            comparedRegister5s.add(reg5);
            lastReg5 = reg5;
        }
        if (opcodeComputer.tick())
            break;
    }
    
    return lastReg5;
}

void main() {
  String input = File("input.txt").readAsStringSync();
  print(solve(input));
}
