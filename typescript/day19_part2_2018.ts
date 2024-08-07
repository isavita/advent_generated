const fs = require('fs');
const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

interface Operation {
  (r: number[], a: number, b: number): number;
}

const instructions: { [key: string]: Operation } = {
  addr: (r, a, b) => r[a] + r[b],
  addi: (r, a, b) => r[a] + b,
  mulr: (r, a, b) => r[a] * r[b],
  muli: (r, a, b) => r[a] * b,
  banr: (r, a, b) => r[a] & r[b],
  bani: (r, a, b) => r[a] & b,
  borrr: (r, a, b) => r[a] | r[b],
  bori: (r, a, b) => r[a] | b,
  setr: (r, a, b) => r[a],
  seti: (r, a, b) => a,
  gtir: (r, a, b) => a > r[b] ? 1 : 0,
  gtri: (r, a, b) => r[a] > b ? 1 : 0,
  gtrr: (r, a, b) => r[a] > r[b] ? 1 : 0,
  eqir: (r, a, b) => a === r[b] ? 1 : 0,
  eqri: (r, a, b) => r[a] === b ? 1 : 0,
  eqrr: (r, a, b) => r[a] === r[b] ? 1 : 0,
};

function loadProgram(lines: string[]): [number | undefined, ((r: number[]) => void)[]] {
  const program: ((r: number[]) => void)[] = [];
  let ipRegister: number | undefined;
  for (const line of lines) {
    if (line.startsWith('#ip')) {
      ipRegister = parseInt(line.split(' ')[1]);
      continue;
    }

    const [op, a, b, c] = line.split(' ');
    const instruction = instructions[op];
    const numA = parseInt(a);
    const numB = parseInt(b);
    const numC = parseInt(c);

    program.push((r) => {
      r[numC] = instruction(r, numA, numB);
    });
  }
  return [ipRegister, program];
}

function runProgram(ipRegister: number | undefined, program: ((r: number[]) => void)[], registers: number[], maxCycles: number): number[] {
  let ip = 0;
  let cycles = 0;

  while (ip >= 0 && ip < program.length) {
    if (ipRegister !== undefined) {
      registers[ipRegister] = ip;
    }
    program[ip](registers);
    if (ipRegister !== undefined) {
      ip = registers[ipRegister] + 1;
    } else {
      ip += 1;
    }
    cycles++;
    if (maxCycles > 0 && cycles >= maxCycles) {
      break;
    }
  }
  return registers;
}

function max(arr: number[]): number {
  return Math.max(...arr);
}

const [ipRegister, program] = loadProgram(input);

if (!ipRegister) {
  console.log("Invalid ip register value.");
  process.exit(1);
}

const registers = new Array(6).fill(0);
registers[0] = 1;
const result = runProgram(ipRegister, program, registers, 1000);
const n = max(result);
let total = 0;
for (let i = 1; i <= n; i++) {
  if (n % i === 0) {
    total += i;
  }
}
console.log(total);