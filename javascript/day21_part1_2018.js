const fs = require('fs');

function solve(input) {
  const opcodeComputer = parseInput(input);

  while (!opcodeComputer_tick(opcodeComputer)) {
    if (opcodeComputer.registers[opcodeComputer.instructionPointer] === 28) {
      break;
    }
  }

  return opcodeComputer.registers[5];
}

function parseInput(input) {
  const lines = input.split('\n');

  let instructionPointer;
  const instructions = [];

  for (let i = 0; i < lines.length; i++) {
    if (i === 0) {
      [, instructionPointer] = lines[i].match(/#ip (\d+)/);
    } else {
      const [name, a, b, c] = lines[i].split(' ');
      instructions.push({ name, abcValues: [+a, +b, +c] });
    }
  }

  return {
    instructions,
    registers: [0, 0, 0, 0, 0, 0],
    instructionPointer: +instructionPointer,
  };
}

const opcodeNamesToFuncs = {
  addr: (registers, [a, b, c]) => {
    registers[c] = registers[a] + registers[b];
    return registers;
  },
  addi: (registers, [a, b, c]) => {
    registers[c] = registers[a] + b;
    return registers;
  },
  mulr: (registers, [a, b, c]) => {
    registers[c] = registers[a] * registers[b];
    return registers;
  },
  muli: (registers, [a, b, c]) => {
    registers[c] = registers[a] * b;
    return registers;
  },
  banr: (registers, [a, b, c]) => {
    registers[c] = registers[a] & registers[b];
    return registers;
  },
  bani: (registers, [a, b, c]) => {
    registers[c] = registers[a] & b;
    return registers;
  },
  borr: (registers, [a, b, c]) => {
    registers[c] = registers[a] | registers[b];
    return registers;
  },
  bori: (registers, [a, b, c]) => {
    registers[c] = registers[a] | b;
    return registers;
  },
  setr: (registers, [a, b, c]) => {
    registers[c] = registers[a];
    return registers;
  },
  seti: (registers, [a, b, c]) => {
    registers[c] = a;
    return registers;
  },
  gtir: (registers, [a, b, c]) => {
    registers[c] = a > registers[b] ? 1 : 0;
    return registers;
  },
  gtri: (registers, [a, b, c]) => {
    registers[c] = registers[a] > b ? 1 : 0;
    return registers;
  },
  gtrr: (registers, [a, b, c]) => {
    registers[c] = registers[a] > registers[b] ? 1 : 0;
    return registers;
  },
  eqir: (registers, [a, b, c]) => {
    registers[c] = a === registers[b] ? 1 : 0;
    return registers;
  },
  eqri: (registers, [a, b, c]) => {
    registers[c] = registers[a] === b ? 1 : 0;
    return registers;
  },
  eqrr: (registers, [a, b, c]) => {
    registers[c] = registers[a] === registers[b] ? 1 : 0;
    return registers;
  },
};

function opcodeComputer_tick(opcodeComputer) {
  if (opcodeComputer.registers[opcodeComputer.instructionPointer] >= opcodeComputer.instructions.length) {
    console.log('Out of range instruction, terminating...');
    return true;
  }

  const instIndex = opcodeComputer.registers[opcodeComputer.instructionPointer];
  const inst = opcodeComputer.instructions[instIndex];

  const opcodeFunc = opcodeNamesToFuncs[inst.name];
  opcodeComputer.registers = opcodeFunc(opcodeComputer.registers, inst.abcValues);

  opcodeComputer.registers[opcodeComputer.instructionPointer]++;

  if (opcodeComputer.registers[opcodeComputer.instructionPointer] >= opcodeComputer.instructions.length) {
    return true;
  }

  return false;
}

const input = fs.readFileSync('input.txt', 'utf8');
console.log(solve(input));