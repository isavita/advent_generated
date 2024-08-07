import * as fs from 'fs';

interface OP {
  a: string;
  b: string;
  action: string;
  name: string;
  matchCount: number[];
}

function main() {
  const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');

  const opcodes: OP[] = [
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

  let sum = 0;
  let lineCount = 0;

  while (lineCount < input.length) {
    if (input[lineCount].startsWith('Before:')) {
      const before = input[lineCount].match(/\d+/g)!.map(Number);
      const instruction = input[lineCount + 1].match(/\d+/g)!.map(Number);
      const after = input[lineCount + 2].match(/\d+/g)!.map(Number);
      const tempSum = testCode(before, after, instruction, opcodes);

      if (tempSum >= 3) {
        sum++;
      }

      lineCount += 4;
    } else {
      break;
    }
  }

  const orderedOpCodes: { [key: number]: OP } = {};

  while (Object.keys(orderedOpCodes).length < 16) {
    for (let i = 0; i < opcodes.length; i++) {
      if (opcodes[i].matchCount.length === 1) {
        const c = opcodes[i].matchCount[0];
        orderedOpCodes[c] = opcodes[i];
        for (let j = 0; j < opcodes.length; j++) {
          remove(opcodes[j], c);
        }
      }
    }
  }

  lineCount += 2;
  let r = [0, 0, 0, 0];

  while (lineCount < input.length) {
    const instruction = input[lineCount].match(/\d+/g)!.map(Number);
    r = runOp(orderedOpCodes[instruction[0]], r, instruction);
    lineCount++;
  }

  console.log(r[0]);
}

function remove(op: OP, c: number) {
  op.matchCount = op.matchCount.filter(v => v !== c);
}

function add(op: OP, c: number) {
  if (!op.matchCount.includes(c)) {
    op.matchCount.push(c);
  }
}

function testCode(registers: number[], result: number[], instruction: number[], opcodes: OP[]) {
  let sum = 0;
  for (let i = 0; i < opcodes.length; i++) {
    if (match(result, runOp(opcodes[i], registers, instruction))) {
      add(opcodes[i], instruction[0]);
      sum++;
    }
  }
  return sum;
}

function match(r: number[], c: number[]) {
  return r.every((val, index) => val === c[index]);
}

function runOp(op: OP, registers: number[], instruction: number[]) {
  const registerCP = [...registers];
  let A, B;
  if (op.a === 'r') {
    A = registerCP[instruction[1]];
  } else {
    A = instruction[1];
  }
  if (op.b === 'r') {
    B = registerCP[instruction[2]];
  } else {
    B = instruction[2];
  }

  switch (op.action) {
    case '+':
      registerCP[instruction[3]] = A + B;
      break;
    case '*':
      registerCP[instruction[3]] = A * B;
      break;
    case '&':
      registerCP[instruction[3]] = A & B;
      break;
    case '|':
      registerCP[instruction[3]] = A | B;
      break;
    case 'a':
      registerCP[instruction[3]] = A;
      break;
    case '>':
      registerCP[instruction[3]] = A > B ? 1 : 0;
      break;
    case '=':
      registerCP[instruction[3]] = A === B ? 1 : 0;
      break;
    default:
      console.log("not valid instruction");
  }
  return registerCP;
}

main();