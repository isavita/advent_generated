const fs = require('fs');
const OP = {
  addr: { action: '+', a: 'r', b: 'r', matchCount: [] },
  addi: { action: '+', a: 'r', b: 'v', matchCount: [] },
  mulr: { action: '*', a: 'r', b: 'r', matchCount: [] },
  muli: { action: '*', a: 'r', b: 'v', matchCount: [] },
  banr: { action: '&', a: 'r', b: 'r', matchCount: [] },
  bani: { action: '&', a: 'r', b: 'v', matchCount: [] },
  borr: { action: '|', a: 'r', b: 'r', matchCount: [] },
  bori: { action: '|', a: 'r', b: 'v', matchCount: [] },
  setr: { action: 'a', a: 'r', b: 'r', matchCount: [] },
  seti: { action: 'a', a: 'v', b: 'r', matchCount: [] },
  gtir: { action: '>', a: 'v', b: 'r', matchCount: [] },
  gtri: { action: '>', a: 'r', b: 'v', matchCount: [] },
  gtrr: { action: '>', a: 'r', b: 'r', matchCount: [] },
  eqir: { action: '=', a: 'v', b: 'r', matchCount: [] },
  eqri: { action: '=', a: 'r', b: 'v', matchCount: [] },
  eqrr: { action: '=', a: 'r', b: 'r', matchCount: [] }
};

function main() {
  const input = fs.readFileSync('input.txt', 'utf8').trim();
  const lines = input.split('\n');

  let sum = 0;
  let lineCount = 0;
  while (lineCount < lines.length) {
    if (lines[lineCount][0] === 'B') {
      const registers = lines[lineCount].match(/\d+/g).map(Number);
      const instruction = lines[lineCount + 1].match(/\d+/g).map(Number);
      const n = lines[lineCount + 2].match(/\d+/g).map(Number);

      const tempSum = testCode(registers, n, instruction, OP);

      if (tempSum >= 3) {
        sum++;
      }

      lineCount += 4;
    } else {
      break;
    }
  }

  console.log(sum);
}

function add(op, c) {
  if (!op.matchCount.includes(c)) {
    op.matchCount.push(c);
  }
}

function testCode(registers, n, instruction, opcodes) {
  let sum = 0;
  for (let op of Object.values(opcodes)) {
    if (match(n, runOp(op, registers, instruction))) {
      add(op, instruction[0]);
      sum++;
    }
  }
  return sum;
}

function match(r, c) {
  if (r.length !== c.length) {
    return false;
  }
  for (let i = 0; i < r.length; i++) {
    if (r[i] !== c[i]) {
      return false;
    }
  }
  return true;
}

function runOp(op, registers, instruction) {
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
      console.log('not valid instruction');
  }
  return registerCP;
}

main();