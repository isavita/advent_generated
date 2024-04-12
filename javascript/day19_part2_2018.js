const fs = require('fs');

const instructions = {
  addr: (r, a, b) => r[a] + r[b],
  addi: (r, a, b) => r[a] + b,
  mulr: (r, a, b) => r[a] * r[b],
  muli: (r, a, b) => r[a] * b,
  banr: (r, a, b) => r[a] & r[b],
  bani: (r, a, b) => r[a] & b,
  borr: (r, a, b) => r[a] | r[b],
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

function loadProgram(lines) {
  const program = [];
  let ipRegister = 0;
  const re = /\d+/g;

  for (const line of lines) {
    if (line.startsWith('#ip')) {
      ipRegister = parseInt(line.split(' ')[1]);
      continue;
    }

    const parts = line.split(' ');
    const op = instructions[parts[0]];
    const [a, b, c] = line.match(re).map(Number);

    program.push((r) => {
      r[c] = op(r, a, b);
    });
  }
  return [ipRegister, program];
}

function runProgram(ipRegister, program, registers, maxCycles) {
  let ip = 0;
  let cycles = 0;

  while (ip >= 0 && ip < program.length) {
    registers[ipRegister] = ip;
    program[ip](registers);
    ip = registers[ipRegister] + 1;
    cycles++;
    if (maxCycles > 0 && cycles >= maxCycles) {
      break;
    }
  }
  return registers;
}

function max(arr) {
  return Math.max(...arr);
}

function sumOfDivisors(n) {
  let total = 0;
  for (let i = 1; i <= Math.sqrt(n); i++) {
    if (n % i === 0) {
      total += i;
      if (i !== n / i) {
        total += n / i;
      }
    }
  }
  return total;
}

const input = fs.readFileSync('input.txt', 'utf-8');
const lines = input.trim().split('\n').filter(Boolean);

const [ipRegister, program] = loadProgram(lines);

const registers = [1, 0, 0, 0, 0, 0];
runProgram(ipRegister, program, registers, 1000);
const n = max(registers);
const total = sumOfDivisors(n);
console.log(total);