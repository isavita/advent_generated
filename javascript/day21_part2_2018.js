const fs = require('fs');

const opcodeNamesToFuncs = {
  addr, addi,
  mulr, muli,
  banr, bani,
  borr, bori,
  setr, seti,
  gtir, gtri, gtrr,
  eqir, eqri, eqrr,
};

function solve(input) {
  const opcodeComputer = parseInput(input);
  let lastReg5;
  const comparedRegister5s = new Set();
  
  while (true) {
    if (opcodeComputer.tick()) break;
    if (opcodeComputer.registers[opcodeComputer.instructionPointer] === 28) {
      const reg5 = opcodeComputer.registers[5];
      if (comparedRegister5s.has(reg5)) break;
      comparedRegister5s.add(reg5);
      lastReg5 = reg5;
    }
  }

  return lastReg5;
}

function parseInput(input) {
  const lines = input.split('\n');
  const [, instructionPointer] = lines[0].match(/#ip (\d+)/);
  const instructions = lines.slice(1).map(l => {
    const [name, ...abcValues] = l.split(' ');
    return { name, abcValues: abcValues.map(Number) };
  });
  return new OpcodeComputer(instructions, instructionPointer);
}

class OpcodeComputer {
  constructor(instructions, instructionPointer) {
    this.instructions = instructions;
    this.registers = [0, 0, 0, 0, 0, 0];
    this.instructionPointer = +instructionPointer;
  }

  tick() {
    const instIndex = this.registers[this.instructionPointer];
    if (instIndex >= this.instructions.length) return true;
    const inst = this.instructions[instIndex];
    const opcodeFunc = opcodeNamesToFuncs[inst.name];
    opcodeFunc(this.registers, inst.abcValues);
    this.registers[this.instructionPointer]++;
    return this.registers[this.instructionPointer] >= this.instructions.length;
  }
}

function addr(r, [a, b, c]) { r[c] = r[a] + r[b]; }
function addi(r, [a, b, c]) { r[c] = r[a] + b; }
function mulr(r, [a, b, c]) { r[c] = r[a] * r[b]; }
function muli(r, [a, b, c]) { r[c] = r[a] * b; }
function banr(r, [a, b, c]) { r[c] = r[a] & r[b]; }
function bani(r, [a, b, c]) { r[c] = r[a] & b; }
function borr(r, [a, b, c]) { r[c] = r[a] | r[b]; }
function bori(r, [a, b, c]) { r[c] = r[a] | b; }
function setr(r, [a, _, c]) { r[c] = r[a]; }
function seti(r, [a, _, c]) { r[c] = a; }
function gtir(r, [a, b, c]) { r[c] = a > r[b] ? 1 : 0; }
function gtri(r, [a, b, c]) { r[c] = r[a] > b ? 1 : 0; }
function gtrr(r, [a, b, c]) { r[c] = r[a] > r[b] ? 1 : 0; }
function eqir(r, [a, b, c]) { r[c] = a === r[b] ? 1 : 0; }
function eqri(r, [a, b, c]) { r[c] = r[a] === b ? 1 : 0; }
function eqrr(r, [a, b, c]) { r[c] = r[a] === r[b] ? 1 : 0; }

const input = fs.readFileSync('input.txt', 'utf8');
console.log(solve(input));