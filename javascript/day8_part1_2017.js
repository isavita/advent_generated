const fs = require('fs');

// Step 1: Read Input
const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

// Step 2: Initialize Registers
const registers = {};

// Step 3: Process Instructions
for (const line of input) {
  const parts = line.split(' ');
  const reg = parts[0];
  const op = parts[1];
  const amount = parseInt(parts[2]);
  const condReg = parts[4];
  const condOp = parts[5];
  const condVal = parseInt(parts[6]);

  // Check condition
  let cond = false;
  switch (condOp) {
    case '>':
      cond = (registers[condReg] || 0) > condVal;
      break;
    case '>=':
      cond = (registers[condReg] || 0) >= condVal;
      break;
    case '<':
      cond = (registers[condReg] || 0) < condVal;
      break;
    case '<=':
      cond = (registers[condReg] || 0) <= condVal;
      break;
    case '==':
      cond = (registers[condReg] || 0) === condVal;
      break;
    case '!=':
      cond = (registers[condReg] || 0) !== condVal;
      break;
  }

  if (cond) {
    switch (op) {
      case 'inc':
        registers[reg] = (registers[reg] || 0) + amount;
        break;
      case 'dec':
        registers[reg] = (registers[reg] || 0) - amount;
        break;
    }
  }
}

// Step 4: Find Max Value
let maxValue = 0;
for (const value of Object.values(registers)) {
  if (value > maxValue) {
    maxValue = value;
  }
}

console.log(maxValue);