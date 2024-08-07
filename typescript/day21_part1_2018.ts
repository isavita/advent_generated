import * as fs from 'fs';

interface Instruction {
  name: string;
  abcValues: [number, number, number];
}

interface OpcodeComputer {
  instructions: Instruction[];
  registers: [number, number, number, number, number, number];
  instructionPointer: number;
}

const opcodeNamesToFuncs: { [key: string]: (registers: [number, number, number, number, number, number], abcValues: [number, number, number]) => [number, number, number, number, number, number] } = {
  addr,
  addi,
  mulr,
  muli,
  banr,
  bani,
  borr,
  bori,
  setr,
  seti,
  gtir,
  gtri,
  gtrr,
  eqir,
  eqri,
  eqrr,
};

function addr(registers: [number, number, number, number, number, number], abcValues: [number, number, number]): [number, number, number, number, number, number] {
  registers[abcValues[2]] = registers[abcValues[0]] + registers[abcValues[1]];
  return registers;
}

function addi(registers: [number, number, number, number, number, number], abcValues: [number, number, number]): [number, number, number, number, number, number] {
  registers[abcValues[2]] = registers[abcValues[0]] + abcValues[1];
  return registers;
}

function mulr(registers: [number, number, number, number, number, number], abcValues: [number, number, number]): [number, number, number, number, number, number] {
  registers[abcValues[2]] = registers[abcValues[0]] * registers[abcValues[1]];
  return registers;
}

function muli(registers: [number, number, number, number, number, number], abcValues: [number, number, number]): [number, number, number, number, number, number] {
  registers[abcValues[2]] = registers[abcValues[0]] * abcValues[1];
  return registers;
}

function banr(registers: [number, number, number, number, number, number], abcValues: [number, number, number]): [number, number, number, number, number, number] {
  registers[abcValues[2]] = registers[abcValues[0]] & registers[abcValues[1]];
  return registers;
}

function bani(registers: [number, number, number, number, number, number], abcValues: [number, number, number]): [number, number, number, number, number, number] {
  registers[abcValues[2]] = registers[abcValues[0]] & abcValues[1];
  return registers;
}

function borr(registers: [number, number, number, number, number, number], abcValues: [number, number, number]): [number, number, number, number, number, number] {
  registers[abcValues[2]] = registers[abcValues[0]] | registers[abcValues[1]];
  return registers;
}

function bori(registers: [number, number, number, number, number, number], abcValues: [number, number, number]): [number, number, number, number, number, number] {
  registers[abcValues[2]] = registers[abcValues[0]] | abcValues[1];
  return registers;
}

function setr(registers: [number, number, number, number, number, number], abcValues: [number, number, number]): [number, number, number, number, number, number] {
  registers[abcValues[2]] = registers[abcValues[0]];
  return registers;
}

function seti(registers: [number, number, number, number, number, number], abcValues: [number, number, number]): [number, number, number, number, number, number] {
  registers[abcValues[2]] = abcValues[0];
  return registers;
}

function gtir(registers: [number, number, number, number, number, number], abcValues: [number, number, number]): [number, number, number, number, number, number] {
  if (abcValues[0] > registers[abcValues[1]]) {
    registers[abcValues[2]] = 1;
  } else {
    registers[abcValues[2]] = 0;
  }
  return registers;
}

function gtri(registers: [number, number, number, number, number, number], abcValues: [number, number, number]): [number, number, number, number, number, number] {
  if (registers[abcValues[0]] > abcValues[1]) {
    registers[abcValues[2]] = 1;
  } else {
    registers[abcValues[2]] = 0;
  }
  return registers;
}

function gtrr(registers: [number, number, number, number, number, number], abcValues: [number, number, number]): [number, number, number, number, number, number] {
  if (registers[abcValues[0]] > registers[abcValues[1]]) {
    registers[abcValues[2]] = 1;
  } else {
    registers[abcValues[2]] = 0;
  }
  return registers;
}

function eqir(registers: [number, number, number, number, number, number], abcValues: [number, number, number]): [number, number, number, number, number, number] {
  if (abcValues[0] == registers[abcValues[1]]) {
    registers[abcValues[2]] = 1;
  } else {
    registers[abcValues[2]] = 0;
  }
  return registers;
}

function eqri(registers: [number, number, number, number, number, number], abcValues: [number, number, number]): [number, number, number, number, number, number] {
  if (registers[abcValues[0]] == abcValues[1]) {
    registers[abcValues[2]] = 1;
  } else {
    registers[abcValues[2]] = 0;
  }
  return registers;
}

function eqrr(registers: [number, number, number, number, number, number], abcValues: [number, number, number]): [number, number, number, number, number, number] {
  if (registers[abcValues[0]] == registers[abcValues[1]]) {
    registers[abcValues[2]] = 1;
  } else {
    registers[abcValues[2]] = 0;
  }
  return registers;
}

function parseInput(input: string): OpcodeComputer {
  const lines = input.split('\n');
  let instructionPointer = 0;
  const instructions: Instruction[] = [];

  for (const line of lines) {
    if (line.startsWith('#ip')) {
      instructionPointer = parseInt(line.split(' ')[1]);
    } else {
      const [name, a, b, c] = line.split(' ');
      instructions.push({ name, abcValues: [parseInt(a), parseInt(b), parseInt(c)] });
    }
  }

  return {
    instructions,
    registers: [0, 0, 0, 0, 0, 0],
    instructionPointer,
  };
}

function solve(input: string): number {
  const opcodeComputer = parseInput(input);

  while (true) {
    if (opcodeComputer.registers[opcodeComputer.instructionPointer] >= opcodeComputer.instructions.length) {
      console.log('Out of range instruction, terminating...');
      break;
    }

    const instructionIndex = opcodeComputer.registers[opcodeComputer.instructionPointer];
    const instruction = opcodeComputer.instructions[instructionIndex];

    const opcodeFunc = opcodeNamesToFuncs[instruction.name];

    opcodeComputer.registers = opcodeFunc(opcodeComputer.registers, instruction.abcValues);

    opcodeComputer.registers[opcodeComputer.instructionPointer]++;

    if (opcodeComputer.registers[opcodeComputer.instructionPointer] >= opcodeComputer.instructions.length) {
      break;
    }

    if (opcodeComputer.registers[opcodeComputer.instructionPointer] === 28) {
      break;
    }
  }

  return opcodeComputer.registers[5];
}

const input = fs.readFileSync('input.txt', 'utf8');
console.log(solve(input));