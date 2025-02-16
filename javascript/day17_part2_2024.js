
const fs = require('node:fs');

function simulateComputer(program) {
  let outs = [];
  let a = program.a, b = program.b, c = program.c;
  let inputProgram = program.program;
  let i = 0;
  while (i < inputProgram.length) {
    const cmd = inputProgram[i++];
    const computeOperand = (val) => {
      if (val >= 0 && val <= 3) return val;
      if (val === 4) return a;
      if (val === 5) return b;
      if (val === 6) return c;
      throw new Error(`Invalid combo operand: ${val}`);
    };
    if (cmd === 0) a >>>= computeOperand(inputProgram[i]);
    else if (cmd === 1) b ^= inputProgram[i];
    else if (cmd === 2) b = computeOperand(inputProgram[i]) % 8;
    else if (cmd === 3) {
      if (a !== 0) i = inputProgram[i] - 1;
      else i++;
    } else if (cmd === 4) b ^= c;
    else if (cmd === 5) outs.push(computeOperand(inputProgram[i]) % 8);
    else if (cmd === 6) b = a >>> computeOperand(inputProgram[i]);
    else if (cmd === 7) c = a >>> computeOperand(inputProgram[i]);
    else throw new Error(`Invalid opcode: ${cmd}`);
    if (i < inputProgram.length) i++;
  }
  return outs;
}

function check(program) {
  let valids = [];
  let stack = [[0, 0]];
  let seen = new Set();
  while (stack.length > 0) {
    let [depth, score] = stack.pop();
    if (seen.has(`${depth}-${score}`)) continue;
    seen.add(`${depth}-${score}`);

    if (depth === program.program.length) {
      valids.push(score);
    } else {
      for (let i = 0; i < 8; i++) {
        let newScore = i + 8 * score;
        let testProgram = { a: newScore, b: program.b, c: program.c, program: program.program };
        let result = simulateComputer(testProgram);
        if (result && result[0] === program.program[program.program.length - 1 - depth]) {
          stack.push([depth + 1, newScore]);
        }
      }
    }
  }
    return valids
}

const data = fs.readFileSync('input.txt', 'utf8');
let a = 0, b = 0, c = 0, program = [];
data.split('\n').forEach(line => {
  line = line.trim();
  if (line.startsWith("Register A:")) a = parseInt(line.split(":")[1].trim());
  else if (line.startsWith("Register B:")) b = parseInt(line.split(":")[1].trim());
  else if (line.startsWith("Register C:")) c = parseInt(line.split(":")[1].trim());
  else if (line.startsWith("Program:")) program = line.split(":")[1].trim().split(",").map(Number);
});

const p = { a, b, c, program };
console.log(Math.min(...check(p)));
