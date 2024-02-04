const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8');
const instructions = data.split('\n');

const [accumulator, _] = executeBootCode(instructions);
console.log(accumulator);

function executeBootCode(instructions) {
  let accumulator = 0;
  const visited = new Set();
  let currentInstruction = 0;

  while (currentInstruction < instructions.length) {
    if (visited.has(currentInstruction)) {
      return [accumulator, true];
    }

    visited.add(currentInstruction);
    const parts = instructions[currentInstruction].split(' ');
    const op = parts[0];
    const arg = parseInt(parts[1]);

    switch (op) {
      case 'acc':
        accumulator += arg;
        currentInstruction++;
        break;
      case 'jmp':
        currentInstruction += arg;
        break;
      case 'nop':
        currentInstruction++;
        break;
    }
  }

  return [accumulator, false];
}