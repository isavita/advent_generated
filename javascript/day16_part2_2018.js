const fs = require('fs');

const opcodes = [
  { name: "addr", action: '+', a: 'r', b: 'r', matchCount: [] },
  { name: "addi", action: '+', a: 'r', b: 'v', matchCount: [] },
  { name: "mulr", action: '*', a: 'r', b: 'r', matchCount: [] },
  { name: "muli", action: '*', a: 'r', b: 'v', matchCount: [] },
  { name: "banr", action: '&', a: 'r', b: 'r', matchCount: [] },
  { name: "bani", action: '&', a: 'r', b: 'v', matchCount: [] },
  { name: "borr", action: '|', a: 'r', b: 'r', matchCount: [] },
  { name: "bori", action: '|', a: 'r', b: 'v', matchCount: [] },
  { name: "setr", action: 'a', a: 'r', b: 'r', matchCount: [] },
  { name: "seti", action: 'a', a: 'v', b: 'r', matchCount: [] },
  { name: "gtir", action: '>', a: 'v', b: 'r', matchCount: [] },
  { name: "gtri", action: '>', a: 'r', b: 'v', matchCount: [] },
  { name: "gtrr", action: '>', a: 'r', b: 'r', matchCount: [] },
  { name: "eqir", action: '=', a: 'v', b: 'r', matchCount: [] },
  { name: "eqri", action: '=', a: 'r', b: 'v', matchCount: [] },
  { name: "eqrr", action: '=', a: 'r', b: 'r', matchCount: [] },
];

const inputStr = fs.readFileSync('input.txt', 'utf-8').trim();
const lines = inputStr.split('\n');

let sum = 0;
let lineCount = 0;

while (lineCount < lines.length) {
  if (lines[lineCount].length > 0 && lines[lineCount][0] === 'B') {
    const registers = regSplit(lines[lineCount], '[^0-9]+').slice(1).map(Number);
    const instruction = regSplit(lines[lineCount + 1], '[^0-9]+').map(Number);
    const result = regSplit(lines[lineCount + 2], '[^0-9]+').slice(1).map(Number);
    const tempSum = testCode(registers, result, instruction, opcodes);

    if (tempSum >= 3) {
      sum++;
    }

    lineCount += 4;
  } else {
    break;
  }
}

const orderedOpCodes = new Map();

while (orderedOpCodes.size < 16) {
  for (const op of opcodes) {
    if (op.matchCount.length === 1) {
      const c = op.matchCount[0];
      orderedOpCodes.set(c, op);
      for (const op2 of opcodes) {
        remove(op2, c);
      }
    }
  }
}

lineCount += 2;

const r = [0, 0, 0, 0];

for (; lineCount < lines.length; lineCount++) {
  const instruction = regSplit(lines[lineCount], '[^0-9]+').map(Number);
  runOp(orderedOpCodes.get(instruction[0]), r, instruction);
}

console.log(r[0]);

function remove(op, c) {
  const index = op.matchCount.indexOf(c);
  if (index !== -1) {
    op.matchCount.splice(index, 1);
  }
}

function add(op, c) {
  if (!op.matchCount.includes(c)) {
    op.matchCount.push(c);
  }
}

function testCode(registers, result, instruction, opcodes) {
  let sum = 0;
  for (const op of opcodes) {
    if (match(result, runOp(op, registers.slice(), instruction))) {
      add(op, instruction[0]);
      sum++;
    }
  }
  return sum;
}

function match(r, c) {
  return r.length === c.length && r.every((v, i) => v === c[i]);
}

function runOp(op, registers, instruction) {
  const A = op.a === 'r' ? registers[instruction[1]] : instruction[1];
  const B = op.b === 'r' ? registers[instruction[2]] : instruction[2];

  switch (op.action) {
    case '+':
      registers[instruction[3]] = A + B;
      break;
    case '*':
      registers[instruction[3]] = A * B;
      break;
    case '&':
      registers[instruction[3]] = A & B;
      break;
    case '|':
      registers[instruction[3]] = A | B;
      break;
    case 'a':
      registers[instruction[3]] = A;
      break;
    case '>':
      registers[instruction[3]] = A > B ? 1 : 0;
      break;
    case '=':
      registers[instruction[3]] = A === B ? 1 : 0;
      break;
    default:
      console.log("not valid instruction");
  }

  return registers;
}

function regSplit(text, delimiter) {
  return text.split(new RegExp(delimiter));
}